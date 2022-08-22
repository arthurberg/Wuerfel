library(shiny)
library(MCMCpack)
library(ggplot2)
library(gridExtra)
library(partitions)
library(shinyjs)
library(XNomial)

textInputRow<-function (inputId, label, value = "") 
{
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

# Define UI for dataset viewer app ----
ui <- fluidPage(# App title ----
                titlePanel("Bayesianische Erkundungen mit Würfeln"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                    # Sidebar panel for inputs ----
                    sidebarPanel(width=6,
                        h3("Auswahl des Würfels"),
                        
                        radioButtons(
                            "select",
                            label = "Wähle den Würfel aus, der gerollt werden soll:",
                            choiceNames = list(
                                HTML("<b>Regulärer Würfel</b> (alle Augenzahlen gleichwahrscheinlich)"),
                                HTML("<b>3fach verzerrter Würfel</b> (1-6 mit Wahrscheinlichkeiten von 8, 8, 64, 1, 6 und 13%)"),
                                HTML("<b>
Benutzerdefinierter Würfel</b> (Werte in 2 eintragen!)"),
                                HTML("<b>Mysteriöser Würfel 1: </b> 1⁄2 für regulären Würfel, 1⁄2 für 3fach verzerrten Würfel"),
                                HTML("<b>Mysteriöser Würfel 2: </b> 1⁄2 für regulären Würfel, 1⁄2 für einen nicht-spezifizierten Würfel (dessen Wahrscheinlichkeiten zufällig aus einer Dirichlet-Verteilung mit (1⁄2,1⁄2,1⁄2,1⁄2,1⁄2,1⁄2) gewählt werden)"),
                                HTML("<b>Mysteriöser Würfel 3: </b> 1⁄2 für regulären Würfel, 1⁄2 für einen benutzerdefinierten Würfel (spezifiziere die Wahrscheinlichkeiten in #2):")
                            ),
                            choiceValues = c("standard","loaded","custom","mystery1","mystery2","mystery3"),
                            selected = "standard"
                        ),
                        
                        conditionalPanel(
                            condition = "input.select == 'custom' || input.select == 'mystery3'",
                            textInput("custom", 
                                      h5("spezifiziere die Wahrscheinlichkeiten (sie sollten 1 als Summe ergeben)"),
                                      ".1,.1,.4,.1,.1,.2")
                        ),

                        
                        h3("Auswahl der priori-Verteilungen"),
                        #h4("Select possible dice and their prior probabilities"),
                        checkboxGroupInput("priors", "",
                                           choiceNames =
                                               list("Regulärer Würfel","3fach verzerrter Würfel","Breit gefächerte Würfel (3003 Priori-Würfel, alle gleich gewichtet)","Benutzerdefinierter Würfel"),
                                           choiceValues =
                                               list("standard", "three.loaded", "many","custom"),
                                           selected=list("standard","three.loaded")
                        ),
                        
                        
                            conditionalPanel(
                            condition = "input.priors.includes('custom')",
                            textInput("custom2", 
                                      h6("Trage die a priori-Wahrscheinlichkeiten ein (sie sollten 1 als Summe ergeben)"),
                                      ".1,.1,.4,.1,.1,.2")
                        ),
                        
                        HTML("<b>Trage die a priori-Wahrscheinlichkeiten ein (sie sollten 1 als Summe ergeben)</b>"),
                    
                        fluidRow(
                            
                                
                            column(12,
                                   splitLayout(
                                       conditionalPanel(
                                           condition = "input.priors.includes('standard')",
                                       textInput("prior.standard", "Regulär", value = .5)
                                       ),
                                       conditionalPanel(
                                           condition = "input.priors.includes('three.loaded')",
                                           textInput("prior.three", "3fach verzerrt", value = .5)
                                       ),
                                   conditionalPanel(
                                       condition = "input.priors.includes('many')",
                                       textInput("prior.many", "Breit gefächerte", value = 0)
                                   ),
                                   conditionalPanel(
                                       condition = "input.priors.includes('custom')",
                                       textInput("prior.custom", "Custom", value = 0)
                                   )
                                   )
                                          
                            )

                        ),
                        
                        h3("Werfen des Würfels"),
                        
                        numericInput("n.rolls", "Anfängliche Zahl der Würfe", 1),
                        
                        actionButton("roll1", "1 Wurf mehr"),
                        
                        actionButton("roll5", "5 Würfe mehr"),
                        
                        actionButton("roll20", "20 Würfe mehr"),
                        
                        textOutput("count"),
                        
                        fluidRow(
                            column(12,
                                   splitLayout(    
                        
                        actionButton("reveal", "Aufdecken des Würfels"),
                        
                        checkboxInput("always.reveal","Immer den Würfel aufdecken",FALSE)
                                   ))),
                        
                        actionButton("clear", "Reset"),
                        
                        
                      
                        
                    ),
                
                    
                    
                    # Main panel for displaying outputs ----
                    mainPanel(width=6,
                        
#                        textOutput("txt"),
                        
#                        verbatimTextOutput("mysummary"),
                        
                        h3("Ergebnisse"),
                        
 #                       verbatimTextOutput("myrolls"),
                        htmlOutput("myrolls2"),
                        
                        plotOutput('Plots'),                        

                        h3("Anpassungstest für einen regulären Würfel"),
                        
                        #verbatimTextOutput("chisq"),
                        htmlOutput("chisq"),
                        
                        h3("Würfel mit höchsten a posteriori-Wahrscheinlichkeiten"),
                        
                        tableOutput("table"),
                        
                        h3("Zugrundeliegender Würfel"),
                        
                        textOutput("reveal")
                        
                    )
                ))



# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

    counter <- reactiveValues(countervalue = 0,counter2value=1,counter.reveal=0)
    
    
    fun.ind.standard=reactive({
        out<-FALSE
        if(is.element("standard",input$priors)){out<-TRUE}
        return(out)
    })
    
    fun.ind.three=reactive({
        out<-FALSE
        if(is.element("three.loaded",input$priors)){out<-TRUE}
        return(out)
    })
    
    fun.ind.many=reactive({
        out<-FALSE
        if(is.element("many",input$priors)){out<-TRUE}
        return(out)
    })
    
    fun.ind.custom=reactive({
        out<-FALSE
        if(is.element("custom",input$priors)){out<-TRUE}
        return(out)
    })    
    
    
    observeEvent(input$clear, {
        counter$countervalue <- 0
        counter$counter2value <- counter$counter2value+1
        counter$counter.reveal <- 0
    })

    observeEvent(input$select, {
        counter$counter.reveal <- 0     
    })
        

    
    
        observeEvent(input$reveal, {
        counter$counter.reveal <- 1     
    })
    
    
        observeEvent(input$n.rolls, {
        counter$countervalue <- 0     
    })
    
    observeEvent(input$select, {
        counter$countervalue <- 0     
    })
    
    
    observeEvent(input$roll1, {
        counter$countervalue <- counter$countervalue + 1     
    })
    
    observeEvent(input$roll5, {
        counter$countervalue <- counter$countervalue + 5     
    })
    
    observeEvent(input$roll20, {
        counter$countervalue <- counter$countervalue + 20    
    })
    
    
    

    
        mystery.select=reactive({
            return(rbinom(10^2,1,.5)[counter$counter2value])
        })
        
        random.die=reactive({
            return(rdirichlet(10^2,rep(1/2,6))[counter$counter2value,])
            
        })
        
#        reveal=reactive({
#            if(input$reveal){reveal=TRUE}
#        })
        
        dice.prob=reactive({
        input$select
        ms=mystery.select()    
        if(input$select=="standard"){myprob=rep(1/6,6)}
        if(input$select=="loaded"){myprob=c(.08,.08,.64,.01,.06,.13)}
#        if(input$select=="custom"){myprob=c(.08,.08,.64,.01,.06,.13)}
        if(input$select=="custom"){myprob=as.numeric(unlist(strsplit(input$custom,",")))}
        if(input$select=="mystery1"){
            if(ms==0){myprob=rep(1/6,6)}
            if(ms==1){myprob=c(.08,.08,.64,.01,.06,.13)}
        }
        if(input$select=="mystery2"){
            if(ms==0){myprob=rep(1/6,6)}
            if(ms==1){myprob=random.die()}
        }        
        if(input$select=="mystery3"){
            if(ms==0){myprob=rep(1/6,6)}
            if(ms==1){myprob=as.numeric(unlist(strsplit(input$custom,",")))}
        }        
        
            return(myprob)
    })
    

        dice.name=reactive({
            input$select
            ms=mystery.select()    
            if(input$select=="standard"){myname="Regulär"}
            if(input$select=="loaded"){myname="3fach verzerrt"}
            #        if(input$select=="custom"){myprob=c(.08,.08,.64,.01,.06,.13)}
            if(input$select=="custom"){myname="Custom"}
            if(input$select=="mystery1"){
                if(ms==0){myname="Regulär"}
                if(ms==1){myname="3fach verzerrt"}
            }
            if(input$select=="mystery2"){
                if(ms==0){myname="Regulär"}
                if(ms==1){myname="Random"}
            }        
            if(input$select=="mystery3"){
                if(ms==0){myname="Regulär"}
                if(ms==1){myname="Custom"}
            }        
            
            return(myname)
        })
        
        
            rolls=reactive({
        input$clear
        myprob=dice.prob()
        myrolls=sample(1:6,input$n.rolls,replace=T,prob=myprob)
        return(myrolls)
    })
    
    rolls2=reactive({
        input$roll1
        myrolls=rolls()
        myprob=dice.prob()
        for(i in 1:counter$countervalue){
            myrolls=c(myrolls,sample(1:6,1,replace=T,prob=myprob))
        }
        return(myrolls)
    })

    rolls5=reactive({
        input$roll5
        myrolls=rolls()
        myprob=dice.prob()
        for(i in 1:counter$countervalue){
            myrolls=c(myrolls,sample(1:6,5,replace=T,prob=myprob))
        }
        return(myrolls)
    })    
    extrarolls=reactive({
        input$clear
        myprob=dice.prob()
        return(sample(1:6,10^4,replace=T,prob=myprob))
    })

    rolls3=reactive({
        input$roll1
        myrolls=rolls()
        myextrarolls=extrarolls()
        if(counter$countervalue==0){allrolls=myrolls}
        if(counter$countervalue>0){allrolls=c(myrolls,myextrarolls[1:(counter$countervalue)])}
#        allrolls=ifelse(counter$countervalue>0,c(myrolls,myextrarolls[1:(counter$countervalue)]),myrolls)
#        allrolls=c(myrolls,myextrarolls[1:(counter$countervalue)])
        return(allrolls)
    })

    pt1 <- reactive({
        df=data.frame(table(factor(rolls3(),levels=1:6)))
        
        ggplot(df,aes(x=Var1,y=Freq)) + geom_bar(stat="identity",width=0.5) + theme_minimal()+theme(text = element_text(size = 20),plot.title = element_text(size=18)) +geom_text(aes(label=Freq), vjust=1.6, color="white", size=6) + xlab("") + ylab("Häufigkeiten") + ggtitle("Tabellierte Ergebnisse")
        
    })   
    pt2 <- reactive({
        mydata=table(factor(rolls3(),levels=1:6))
        
        ind.standard=fun.ind.standard()
        ind.three=fun.ind.three()
        ind.many=fun.ind.many()
        ind.custom=fun.ind.custom()
        
        
        
        standard=rep(1/6,6)
        three.loaded=c(.08,.08,.64,.01,.06,.13)
        custom=as.numeric(unlist(strsplit(input$custom2,",")))
        
        m=10
        myparts0=compositions(m,6,include.zero=T)
        myparts=t(myparts0/m)
        
        prior.dice=rbind(if(ind.standard==TRUE){standard},if(ind.three==TRUE){three.loaded},if(ind.many==TRUE){myparts},if(ind.custom==TRUE){custom})
        
        mydice=droplevels(factor(c(if(ind.standard==TRUE){"Regulär"},if(ind.three==TRUE){"3fach verzerrt"},if(ind.many==TRUE){"Many"},if(ind.custom==TRUE){"Custom"}),levels=c("Regulär","3fach verzerrt","Many","Custom")))
        
        ind.dice=c(if(ind.standard==TRUE){"Regulär"},if(ind.three==TRUE){"3fach verzerrt"},if(ind.many==TRUE){rep("Many",dim(myparts)[1])},if(ind.custom==TRUE){"Custom"})
        
        n=dim(prior.dice)[1]
        
        likelihood=rep(NA,n)
        for(i in 1:n){
            prob=prior.dice[i,]
            likelihood[i]=dmultinom(mydata,prob=prob)
        }
        
        prob.prior1A=c(if(ind.standard==TRUE){as.numeric(input$prior.standard)},if(ind.three==TRUE){as.numeric(input$prior.three)},if(ind.many==TRUE){rep(as.numeric(input$prior.many)/dim(myparts)[1],dim(myparts)[1])},if(ind.custom==TRUE){as.numeric(input$prior.custom)})
        prob.prior2A=c(if(ind.standard==TRUE){as.numeric(input$prior.standard)},if(ind.three==TRUE){as.numeric(input$prior.three)},if(ind.many==TRUE){as.numeric(input$prior.many)},if(ind.custom==TRUE){as.numeric(input$prior.custom)})
        
        prob.prior1B=prob.prior1A/sum(prob.prior1A)
        prob.prior2B=prob.prior2A/sum(prob.prior2A)
        prior=prob.prior1B
        
        posterior=prior*likelihood/sum(prior*likelihood)
        posterior2=sapply(split(posterior,ind.dice),sum)
        
        
        
        df2=data.frame(selected.dice=rep(mydice,2),prior.posterior=factor(c(rep(c("A priori","A posteriori"),each=length(mydice))),levels=c("A priori","A posteriori")),probs.prior.posterior=c(prob.prior2B,posterior2[as.character(mydice)]))
        
        ggplot(data=df2, aes(x=selected.dice, y=probs.prior.posterior, fill=prior.posterior)) +
            geom_bar(stat="identity", position=position_dodge())+ylim(0,1) + theme_minimal()+theme(text = element_text(size = 20),legend.position="top",plot.title = element_text(size=18)) +labs(fill="") + ylab("Wahrscheinlichkeit") + xlab("")  + ggtitle("Bayesianische Aktualisierung") 
        
    })       
    output$Plots = renderPlot({
        ptlist <- list(pt1(),pt2())
        grid.arrange(grobs=ptlist,ncol=length(ptlist))
    })
    
    output$Hist <- renderPlot({
        barplot(table(factor(rolls3(),levels=c(1:6))))
        }) 

    
    ############################################################################################
    ############################################################################################
    ############################################################################################
    
    output$myrolls <- renderPrint({
        print(rolls3())
    })
    
    output$myrolls2 <- renderUI({
      h4(HTML(paste(rolls3(),collapse=", ")))
    })
    
    output$chisq <- renderUI({
        mydata=rolls3()
        obs=table(factor(mydata,levels=1:6))
        expected=rep(1/6,6)*length(mydata)
        test.stat=sum((obs-expected)^2/expected)
        p.asy=signif(pchisq(test.stat,5,lower.tail = FALSE),3)
        p.exact="(only calculated up to 100 rolls)"
        if(sum(obs)<=100){
        p.exact=signif(xmulti(obs,rep(1/6,6),statName="Chisq")$pChi,3) # exact goodness-of-fit
        }
        #print("Goodness-of-fit test statistic: ")
        #cat(signif(test.stat,3))
        
        HTML(paste("Goodness-of-Fit-Statistik: ",test.stat,"<br>","p-Wert:: ",p.asy,"<br>","Exakter p-Wert: ",p.exact))

        #paste("Goodness-of-fit p-value: ",signif(pchisq(test.stat,5,lower.tail = FALSE),3))
    })
    
    output$mysummary <- renderPrint({
        ind.standard=fun.ind.standard()
        ind.three=fun.ind.three()
        ind.many=fun.ind.many()
        ind.custom=fun.ind.custom()
        
        x=mystery.select()
        
        print(input$select)
        print(counter$counter2value)
        prob.prior0=c(if(ind.standard==TRUE){as.numeric(input$prior.standard)},if(ind.three==TRUE){as.numeric(input$prior.three)},if(ind.many==TRUE){as.numeric(input$prior.many)},if(ind.custom==TRUE){as.numeric(input$prior.custom)})
        prob.prior=prob.prior0/sum(prob.prior0)
        print(prob.prior)
        cat("ind(4):\n")
        print(ind.standard)
        print(ind.three)
        print(ind.many)
        print(ind.custom)
        print(input$priors)
        print(counter$ind.standard)
        print(x)
        print(random.die())
        print(rolls3())
        print(counter$counter.reveal)
    })

    
    output$count <- renderText({
        paste("Gesamtzahl aller Würfe bisher:", counter$countervalue+1)   # print the latest value stored in the reactiveValues object
    })
    
    output$txt <- renderText({
        priors <- paste(input$priors, collapse = ", ")
        paste("You chose", priors)
    })
    
    output$table <- renderTable({

        mydata=table(factor(rolls3(),levels=1:6))
        
        ind.standard=fun.ind.standard()
        ind.three=fun.ind.three()
        ind.many=fun.ind.many()
        ind.custom=fun.ind.custom()

        
        
        standard=rep(1/6,6)
        three.loaded=c(.08,.08,.64,.01,.06,.13)
        custom=as.numeric(unlist(strsplit(input$custom2,",")))
        
        m=10
        myparts0=compositions(m,6,include.zero=T)
        myparts=t(myparts0/m)
        
        prior.dice=rbind(if(ind.standard==TRUE){standard},if(ind.three==TRUE){three.loaded},if(ind.many==TRUE){myparts},if(ind.custom==TRUE){custom})
        
        mydice=droplevels(factor(c(if(ind.standard==TRUE){"Regulär"},if(ind.three==TRUE){"3fach verzerrt"},if(ind.many==TRUE){"viele"},if(ind.custom==TRUE){"Benutzerdefiniert"}),levels=c("Regulär","3fach verzerrt","viele","Benutzerdefiniert")))
        
        ind.dice=c(if(ind.standard==TRUE){"Regulär"},if(ind.three==TRUE){"3fach verzerrt"},if(ind.many==TRUE){rep("viele",dim(myparts)[1])},if(ind.custom==TRUE){"Benutzerdefiniert"})
        
        n=dim(prior.dice)[1]
        
        likelihood=rep(NA,n)
        for(i in 1:n){
            prob=prior.dice[i,]
            likelihood[i]=dmultinom(mydata,prob=prob)
        }
        
        prob.prior1A=c(if(ind.standard==TRUE){as.numeric(input$prior.standard)},if(ind.three==TRUE){as.numeric(input$prior.three)},if(ind.many==TRUE){rep(as.numeric(input$prior.many)/dim(myparts)[1],dim(myparts)[1])},if(ind.custom==TRUE){as.numeric(input$prior.custom)})
        prob.prior2A=c(if(ind.standard==TRUE){as.numeric(input$prior.standard)},if(ind.three==TRUE){as.numeric(input$prior.three)},if(ind.many==TRUE){as.numeric(input$prior.many)},if(ind.custom==TRUE){as.numeric(input$prior.custom)})
        
        prob.prior1B=prob.prior1A/sum(prob.prior1A)
        prob.prior2B=prob.prior2A/sum(prob.prior2A)
        prior=prob.prior1B
        
        posterior=prior*likelihood/sum(prior*likelihood)
        posterior2=sapply(split(posterior,ind.dice),sum)
        
        if(sum(posterior>=.05)>1){
        tab=cbind(signif(posterior[posterior>=.05],3),ind.dice[posterior>=.05],signif(prior.dice[posterior>=.05,],3))
        colnames(tab)=c("a posteriori-Wahrscheinlichkeiten","Klasse",as.character(1:6))            
        ord=order(tab[,1],decreasing = TRUE)
        tab=tab[ord,]
        }
        
        if(sum(posterior>=.05)==1){
            tab=t(c(signif(posterior[posterior>=.05],3),ind.dice[posterior>=.05],signif(prior.dice[posterior>=.05,],3)))
            colnames(tab)=c("a posteriori-Wahrscheinlichkeiten","Klasse",as.character(1:6))            
        }
        
        if(sum(posterior>=.05)==0){
            tab="Nearly all of the posterior probability is split in small amounts among many of the 3003 broad ranging dice.  Try rolling the die more times."
        }
        
        
                #if(dim(tab)[2]<4){
        #tab=t(tab)
        #}
        tab
    })
  
    
    output$reveal <- 
        renderPrint({
            if(counter$counter.reveal==0 & !input$always.reveal){
                cat("Drücke „Aufdecken des Würfels“, um die Wahr-scheinlichkeiten des Würfels zu zeigen")
            }
            if(counter$counter.reveal==1 | input$always.reveal){
                name=dice.name()
                prob=dice.prob()
                #print("Dice rolled: ",name)
                cat(name)
                cat(" die selected with probabilities ")
                cat(paste(signif(prob,3),collapse=", "),".",sep="")
            }            
            })

            
        # Return the requested dataset ----
    # By declaring datasetInput as a reactive expression we ensure
    # that:
    #
    # 1. It is only called when the inputs it depends on changes
    # 2. The computation and result are shared by all the callers,
    #    i.e. it only executes a single time

    
}

# Create Shiny app ----
shinyApp(ui, server)