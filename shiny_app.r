library(writexl)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(reshape2)
require(deSolve)
library('shiny')
# install.packages("reshape2") #Already installed, so this gives a warning


generate_scenario <- function(t,s,e,threshold,scen_name,case=FALSE){
  Sim<-list()
  for(i in 1:1000) { Sim[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=threshold,1,0)}
  Case<-list()
  for(i in 1:1000) { Case[[i]]<-apply(X=Sim[[i]], MARGIN=2, FUN=mean)}
  
  Scen_Frame<- data.frame(Case)
  colnames(Scen_Frame)<- c(1:1000)
  Frame_2<-melt(Scen_Frame,id.vars=NULL)
  colnames(Frame_2)<-c("Case",paste(scen_name,"a",sep="_"))
  
  #Find the mean per number of cases
  Frame_3<-aggregate(Frame_2[,2],by=list(Frame_2[,1]), FUN=mean)
  colnames(Frame_3)<-c("Case",paste(scen_name,"b",sep="_"))
  
  if(case){
    return(list(scen_name,Frame_2,Frame_3))  
  }
  else {
    Frame_2$Case=NULL
    Frame_3$Case=NULL
    return(list(scen_name,Frame_2,Frame_3)) 
  }
  }

#Create SEIR model
SEIR <- function(time, current_state, params){
  
  with(as.list(c(current_state, params)),{
    N <- S+E+I+R
    dS <- -(beta*S*I)/N
    dE <- (beta*S*I)/N - sigma*E
    dI <- sigma*E - gamma*I
    dR <- gamma*I
    
    return(list(c(dS, dE, dI, dR)))
  })
}
#Across all diseases:
initial_state <- c(S=6500000, E=0, I=1, R=0)
times <- 0:365

# We want a list of possible SEIR models to run, and will use that for the interface.
Disease_Cases <- list(list(name="SARS-CoV-2", params=c(beta=0.32, sigma=0.15, gamma=0.125)),
                      list(name="1918 Flu", params=c(beta=0.32, sigma=0.15, gamma=0.125)),
                      list(name="MERS", params=c(beta=0.32, sigma=0.15, gamma=0.125))
                      )


#We need the scenarios to run in order to run them in the function we need to build for Shiny.
if(!exists("ScenariosRun")){ScenariosRun=FALSE}
#Check if they are already run:
if(!ScenariosRun){
  
  Percentages = c(10,30,50,70)
  Thresholds = c(1,3,5)
  Scenarios = list(Percentages,Thresholds)
  
  Run_cases <- function(){
  case=TRUE
  for (i in Thresholds){
    for(j in Percentages){
      scen_name=paste("Scen",j,i,sep="_")
      print(scen_name)
      S = generate_scenario(t=j/100,s=0.5,e=0.25,threshold=i,
                            scen_name=scen_name,case=case)
      
      if(case){
        Master_Frame_a = data.frame(S[[2]])
        Master_Frame_b = data.frame(S[[3]])
        case=FALSE}
      else{
        Master_Frame_a <- cbind(Master_Frame_a,S[[2]])
        Master_Frame_b <- cbind(Master_Frame_b,S[[3]])
      }
    }
  }
  
  Master_Frame_a$Cases_continuous<-as.numeric(as.character(Master_Frame_a$Case))
  Master_Frame_b$Cases_continuous_b<-as.numeric(as.character(Master_Frame_b$Case))
  
  return(list(Master_Frame_a,Master_Frame_b))
  }
  
  output = Run_cases()
  Master_Frame_a <- output[[1]]
  Master_Frame_b <- output[[2]]
  
  
  #NEW:Pivot Master Frame 1a longer and subset to only include detection threshold=1
  Master_Frame_c<-Master_Frame_a[,c(1:5,14)] 
  Master_Frame_c <-Master_Frame_c %>%
    pivot_longer(!c("Case","Cases_continuous"), names_to = "Coverage", values_to = "Cumulative_probability")
  ScenariosRun= TRUE
  }



ui <- fluidPage(
  tags$head(HTML("<style>
  div.shiny-text-output{
  	display: inline;
    padding: 0px;
    }</style>")
  ),
  #tags$head(tags$script(HTML(JS.logify))),
  #tags$head(tags$script(HTML(JS.onload))),
  title="Basic model for ThreatNet-IL",
  # App title ----
  titlePanel("Basic model for ThreatNet-IL"),
  # Sidebar layout with input and output definitions ----
  # Pick Disease, Number of detection for threshold, Number of Hospitals.
  # Number of Hospitals will map to the 
  column(width=4,
         # Input: Slider for the number of bins ----
         radioButtons(inputId = "Disease_Scenario",
                       label = "Disease Scenario",
                       choices = c("SARS-CoV-2", "1918 Flu", "MERS"),
                       selected = 1
                       ),
         sliderInput(inputId = "Threshold",
                     label = "Detections needed to confirm disease)",
                     min = 1,
                     max = 5,
                     value = 1,
                     step=2,
                     ticks=FALSE),
         selectInput(inputId = "Coverage",
                     label = "Hospitals participating in Threatnet",
                     choices = c("Sheba","6 Largest", "10 Largest"),
                     selected = 2)
  ),
  # Main panel for displaying outputs ----
  textOutput("test"),
  withTags({
    div(checked=NA,
        h3("Model Outcomes"),
        "This analysis shows the outcome",br(),
        "The expected delay before detection is ", textOutput("expected_delay"), br(),
        p("Per linked thing", a(href='https://www.nytimes.com/2020/05/22/well/live/putting-the-risk-of-covid-19-in-perspective.html'), "here are other facts.")
    )}),
  mainPanel(plotOutput("Detection_plot")
            ),
  withTags({
    div(class="bodytext", checked=NA,
        h3("Methodology"),
        p("This model is a rewritten adaptation of the Sharma et. al. Threat Net paper."),
        p("Sharma S, Pannu J, Chorlton S, Swett JL, Ecker DJ. 
          Threat Net: A Metagenomic Surveillance Network for Biothreat Detection and Early Warning.
          Health Secur. 2023 Jun 27. doi: 10.1089/hs.2022.0160."))}
  )
  
)


server <- function(input, output, session) {
  #Inputs are Disease_Scenario, Threshold, Hospitals

  observe("scenario") #Index: 
  
  output$Detection_plot <-renderPlot({
    p2<-ggplot() + 
      geom_point(data=Master_Frame_c, aes(y=Cumulative_probability, x=Cases_continuous, color=Coverage), size=1)+
      geom_smooth(data=Master_Frame_b, formula=y ~ s(x, bs = "cs"), 
                  aes(y=Scen_10_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
      geom_smooth(data=Master_Frame_b,  formula=y ~ s(x, bs = "cs"), 
                  aes(y=Scen_30_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
      geom_smooth(data=Master_Frame_b,  formula=y ~ s(x, bs = "cs"), 
                  aes(y=Scen_50_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
      geom_smooth(data=Master_Frame_b,  formula=y ~ s(x, bs = "cs"), 
                  aes(y=Scen_70_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
      labs(x="Infections", y="Cumulative probability")+
      xlim(0,100)
    
    p2+ theme_gray()+
      theme(axis.title.x = element_text(face="bold", size=15))+
      theme(axis.title.y = element_text(face="bold", size=15))+
      theme(title= element_text(face="bold", size=20))+
      theme(legend.position = c(0.8, 0.2))+
      scale_color_discrete(labels=c('10%', '30%', '50%',"70%"))+
      geom_hline(yintercept=0.5, linetype=5, color = "gray40")+
      geom_hline(yintercept=0.8, linetype=5, color = "gray20")+
      geom_hline(yintercept=0.95, linetype=5, color = "gray0")
  })

    renderPrint({
    #Disease Analyses
    model <- ode(initial_state, times, SEIR, Disease_Cases[[input$Disease_Scenario]]$params)
    matplot(model, type="l", lty=1, main="SEIR model", xlab="Time")
    legend <- colnames(model)[2:6]
    })
  
}

#Run!
shinyApp(ui, server)


# p1<-ggplot() + 
#   geom_point(data=Master_Frame_a, aes(y=Scen_10_1_a, x=Cases_continuous), color="coral3", size=1)+
#   geom_point(data=Master_Frame_a, aes(y=Scen_30_1_a, x=Cases_continuous), color="darkslategray3", size=1)+
#   geom_point(data=Master_Frame_a, aes(y=Scen_50_1_a, x=Cases_continuous), color="darkseagreen3", size=1)+
#   geom_point(data=Master_Frame_a, aes(y=Scen_70_1_a, x=Cases_continuous), color="darkorchid3", size=1)+
#   geom_smooth(data=Master_Frame_b, aes(y=Scen_10_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
#   geom_smooth(data=Master_Frame_b, aes(y=Scen_30_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
#   geom_smooth(data=Master_Frame_b, aes(y=Scen_50_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
#   geom_smooth(data=Master_Frame_b, aes(y=Scen_70_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
#   labs(x="Infections", y="Cumulative probability")+
#   xlim(0,100)


# Try to rewrite graph to be cleaner for Shiny graphing:
# 
# df.long<-melt(Master_Frame_a,id.vars=c("Case","Cases_continuous"))
# df2.long<-melt(Master_Frame_b,id.vars=c("Case","Cases_continuous_b"))
# 
# #Graph all the cases.
# p1_long= ggplot() +
#   geom_point(data=df.long, aes(y=value, x=Cases_continuous,color=variable), #Missing rows for cases we don't plot here.
#              #color=cols[df.long$variable],
#              size=1) +
#   labs(x="Infections", y="Cumulative probability") +
#   xlim(0,100) +
#   geom_smooth(data=df2.long, aes(y=value, x=Cases_continuous_b, group=variable), method="gam",
#               color=c("black"),se=FALSE, linetype=5, size=1.3) + #Graphs even things not in the earlier cases.
#   geom_hline(yintercept=c(0.5,0.8,0.95), linetype=5, color = c("gray40","gray20","gray0"))
#   
