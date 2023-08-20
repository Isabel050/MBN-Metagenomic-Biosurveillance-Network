library(writexl)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(reshape2)
require(deSolve)
library('shiny')
require("FinancialMath") # For NPV in costs
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
                      list(name="SARS-CoV-2 Omicron", params=c(beta=1.19, sigma=0.25, gamma=0.125)),
                      list(name="SARS", params=c(beta=0.24, sigma=0.25, gamma=0.1)),
                      list(name="Seasonal Flu", params=c(beta=0.33, sigma=0.5, gamma=0.25)),
                      list(name="Pandemic 1918 Flu", params=c(beta=0.5, sigma=0.5, gamma=0.25)),
                      list(name="MERS", params=c(beta=0.9, sigma=0.9, gamma=0.9))
)
Disease_names=c()
for (i in 1:length(Disease_Cases)){
  Disease_names=append(Disease_names,Disease_Cases[[i]]$name)
}


#Percentage Coverage is based on scenarios for hospitals.
Coverage_names = c("Six Largest", "Ten Largest", "Only Sheba")
#Note that these coverage names need to be in this order, or it breaks.
#That is due to order depending on scenario names, and is bad, but works now.

Coverage_rates = c(8,35,50)
#Thresholds are the number of detections (cases) needed for response.
Thresholds = c(1,3,5)

#We need the scenarios to run in order to run them in the function we need to build for Shiny.
if(!exists("ScenariosRun")){ScenariosRun=FALSE}
#Check if they are already run:
if(!ScenariosRun){
  
  Percentages = Coverage_rates
  Scenarios = list(Percentages,Thresholds)
  
  Run_cases <- function(){
  case=TRUE
  for (i in Thresholds){
    for(j in Percentages){
      scen_name=paste("Scen",j,i,sep="_")
      #It turns out that these names are used to order the graph.
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
  
  
  #NEW:Pivot Master Frame 1a longer 
  # We subset to only include detection threshold=1 later, so it can be interface-driven.
  Master_Frame_c <-Master_Frame_a %>%
    pivot_longer(!c("Case","Cases_continuous"), names_to = "Coverage", values_to = "Cumulative_probability") 
  
  Master_Frame_c <- 
    separate_wider_delim(Master_Frame_c, "Coverage", "_", 
                         names= c(NA, "HospPCT","DetectThreshold",NA))
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
         selectInput(inputId = "Disease_Scenario",
                     label = "Disease Scenario",
                     choices = Disease_names,
                     selected = "SARS",
                     multiple= TRUE
                    ),
         sliderInput(inputId = "Threshold",
                     label = "Detections needed to confirm disease",
                     min = 1,
                     max = 5,
                     value = 1,
                     step=2,
                     ticks=FALSE),
         sliderInput(inputId = "Delay",
                     label = "Days needed to confirm result",
                     min = 1,
                     max = 5,
                     value = 1,
                     step=2,
                     ticks=FALSE),
         selectInput(inputId = "Coverage",
                     label = "Hospitals participating in Threatnet",
                     choices = c("Only Sheba", "Six Largest", "Ten Largest"),
                     selected = 2)
  ),
  # Main panel for displaying outputs ----
  textOutput("test"),
  withTags({
    div(checked=NA,
        h3("Model Outcomes"),
        "This analysis shows the outcome",br(),
        "checking if this works:", textOutput("Testing"), br(),
        p("Per linked thing", a(href='https://www.nytimes.com/2020/05/22/well/live/putting-the-risk-of-covid-19-in-perspective.html'), "here are other facts.")
    )}),
  mainPanel(plotOutput("Detection_plot"),
            plotOutput("Disease_plot"),
            plotOutput("p_1Disease"),
            plotOutput("p_1HospPCT")
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
  # observe("scenario") #Index
  
  #Need to sort so that the factors are in alphabetical order, to match the graph.
  Coverage_names_factor <- factor(Coverage_names, levels = unique(Coverage_names))
  
  output$Detection_plot <-renderPlot({
    p2 <- ggplot() + 
      geom_point(data=Master_Frame_c %>% filter(DetectThreshold==input$Threshold),
                 aes(y=Cumulative_probability, x=Cases_continuous, color=HospPCT), size=1) +
      labs(x="Infections", y="Cumulative probability")+
      xlim(0,100)+
      geom_smooth(data=Master_Frame_c %>% filter(DetectThreshold==input$Threshold),
                  formula=y ~ s(x, bs = "cs"), 
                              aes(y=Cumulative_probability, 
                                  x=Cases_continuous, group = HospPCT), 
                              method="gam",color="black",se=FALSE, 
                              linetype=5, linewidth=1.3)
     
    p2 + theme_gray() +
      theme(axis.title.x = element_text(face="bold", size=15))+
      theme(axis.title.y = element_text(face="bold", size=15))+
      theme(title= element_text(face="bold", size=20))+
      theme(legend.position = c(0.8, 0.2)) +
      scale_color_discrete(labels=Coverage_names_factor)+
      geom_hline(yintercept=0.5, linetype=5, color = "gray40")+
      geom_hline(yintercept=0.8, linetype=5, color = "gray20")+
      geom_hline(yintercept=0.95, linetype=5, color = "gray0")
  })

  #For diseases, we need to run the models then plot them.

  model_df<-data.frame(times)
  for(i in 1:length(Disease_Cases)){
    model <- ode(initial_state, times, SEIR, Disease_Cases[[i]]$params)
    matplot(model, type="l", lty=1, main=paste("SEIR model",Disease_Cases[[i]]$name), xlab="Time")
    legend <- colnames(model)[2:6]
    model_df<-cbind(model_df,data.frame(model))
  }
  
  #Get distinct names per case:
  colnames(model_df) <- make.names(colnames(model_df), unique = TRUE)

  
  #Group into cumulative infections
  model_df_aggregated<-data.frame(times)
  for(i in 1:length(Disease_Cases)){
    case = i-1
    if(case==0){
      model_df_aggregated<-cbind(model_df_aggregated,model_df$I+model_df$R)
    }
    else{
      model_df_aggregated<-cbind(model_df_aggregated,
                                 get(paste0('I.',case), envir=as.environment(model_df)) +
                                   get(paste0('R.',case), envir=as.environment(model_df))
      )
    }
    
  }
  names(model_df_aggregated) <- c("time",Disease_names)
  colnames(model_df_aggregated)[1]<-c("Days")
  
  Cases_to_Run <- reactive({Input$Disease_Scenario})
  
  #Restructure for ggplot
  #model_df2 <- reactive({ #Switch what is in this depending on input data
  model_df2 <- #For testing
    model_df_aggregated %>%
    pivot_longer(!Days, names_to = "Pathogen", values_to = "Cases_TTD") # %>%
    # filter(Pathogen %in% c('SARS','MERS'))
  # })
  
  
  output$Disease_plot <- renderPlot({ggplot() +
      geom_line(data=model_df2 %>% filter(Pathogen %in% input$Disease_Scenario),
                aes(y=Cases_TTD, x=Days, color=Pathogen), linewidth=1.1,linetype=1)+
      labs(x="Days", y="Infections")+
      xlim(0,70)+
      ylim(0,400)+
      ggtitle("SEIR Model") + theme_gray()+
      theme(axis.title.x = element_text(face="bold", size=15))+
      theme(axis.title.y = element_text(face="bold", size=15))+
      theme(title= element_text(face="bold", size=20))
  })
  
  output$Testing <- renderPrint({
    print(input$Coverage[[1]])
    print(Coverage_names)
    print(match(input$Coverage,Coverage_names))
  })
  
  #Combination Plot - the detection plot shifted based on the disease(s)
  
  #model_df_aggregated is the disease number of cases by day
  #Master_Frame_c is the detection time by number of cases
  
  #We need a new data frame with the levels from both DF.
  
  # Master_frame_c has HospPCT and DetectThreshold, plus merge variable Cases_continuous
  # model_df_aggregated has Days and Cases per disease.
  # Discretize model_df_aggregated case counts by rounding down:
  model_df_aggregated_discrete <- data.frame(lapply(model_df_aggregated,floor))
  #I only want the first day that there are X cases.
  All_disease_mapping_table <- 0
  #Disease Names with spcaes, etc. get mangled, so:
  corr_Disease_names = gsub(" ",".",gsub("-",".",Disease_names))
  
  # Add disease columns to Master_Frame_c.
  for (name in corr_Disease_names){ # Which are columns in model_df_aggregated_discrete
    #print(name) }
    temp_table <- model_df_aggregated_discrete[,c('Days',name)]
    names(temp_table)<-c('Day','Cases_continuous')
    temp_table$Disease = name
    if (length(All_disease_mapping_table)==1){
      All_disease_mapping_table <- temp_table
    } else {All_disease_mapping_table <- rbind(All_disease_mapping_table, temp_table)}
  }
    #Include only the first day with that many cases.
    temp_2 <- All_disease_mapping_table %>% group_by(Cases_continuous, Disease) %>% filter(rank(Cases_continuous,ties.method="first")==1)
    temp_2 <- data.frame(temp_2)
    All_disease_mapping_table <- temp_2
    
    
    # At this point, we need to use colors EITHER to represent HospPCT *OR* Disease.
    Disease_num <-reactive(match(input$Disease_Scenario[[1]]))
    output$p_1Disease <- renderPlot({ggplot() + 
        geom_point(data=inner_join(Master_Frame_c,
                                   All_disease_mapping_table 
                                   %>% filter(Disease==gsub(" ",".",gsub("-",".",input$Disease_Scenario[[1]]))))
                   %>% filter(DetectThreshold==input$Threshold),
                   aes(y=Cumulative_probability, x=Day, color=HospPCT), size=1) +
        labs(x="Day", y="Cumulative probability")+
        xlim(0,100)+
        geom_smooth(data=inner_join(Master_Frame_c,
                                    All_disease_mapping_table
                                    %>% filter(Disease==gsub(" ",".",gsub("-",".",input$Disease_Scenario[[1]])))) 
                    %>% filter(DetectThreshold==input$Threshold),
                    formula=y ~ s(x, bs = "cs"), 
                    aes(y=Cumulative_probability, 
                        x=Day, group = HospPCT),
                    method="gam",color="black")+
        ggtitle(input$Disease_Scenario[[1]])
    })
    
        
  output$p_1HospPCT <- renderPlot({ggplot() + 
     geom_point(data=inner_join(Master_Frame_c 
                                %>% filter(HospPCT==Coverage_rates[match(input$Coverage,Coverage_names)]),
                                All_disease_mapping_table ) 
                %>% filter(DetectThreshold==input$Threshold),
                        aes(y=Cumulative_probability, x=Day, color=Disease), size=1) +
     labs(x="Day", y="Cumulative probability")+
     xlim(0,100)+
     geom_smooth(data=inner_join(Master_Frame_c 
                                 %>% filter(HospPCT==Coverage_rates[match(input$Coverage,Coverage_names)]),
                                 All_disease_mapping_table ) 
                 %>% filter(DetectThreshold==input$Threshold),
                 formula=y ~ s(x, bs = "cs"), 
                 aes(y=Cumulative_probability, 
                     x=Day, group = Disease),
                 method="gam",color="black")+
     ggtitle(paste(input$Coverage, "Detection Threshold:",input$Threshold))
  })
   
}


#New Graph needed - Infections on Day of Detection versus Cost:

#First, Figure out how to join to get the data!
#Day of Detection
Detect_Threshold_ForDate <- 0.5 # 50% Chance of Detection

Case_Count_at_Detection <- data.frame(Master_Frame_c %>% group_by(DetectThreshold, HospPCT) %>% 
  filter(Cumulative_probability>=Detect_Threshold_ForDate) %>%
  slice_min(Cumulative_probability,with_ties=FALSE))

#Reorder HospPCT to be lowest to highest.
Case_Count_at_Detection$HospPCT <- as.character(Case_Count_at_Detection$HospPCT)
Case_Count_at_Detection$HospPCT <- as.numeric(Case_Count_at_Detection$HospPCT)
#Then turn it back into a factor with the levels in the correct order
Case_Count_at_Detection$HospPCT  <- factor(Case_Count_at_Detection$HospPCT, 
                                                   levels=unique(sort(Case_Count_at_Detection$HospPCT,decreasing=TRUE)))


ggplot()+ geom_point(data=Case_Count_at_Detection,
                     aes(y=Case, x=HospPCT, color=DetectThreshold), size=1) +
  labs(x="Detection Threshold", y="Cases So Far")
#Costs:
#Just put the algebra in manually:
HospPCT=c("8","35","50")
n_Hosp=c("1","6","10")
map=data.frame(HospPCT,n_Hosp)
Case_Count_at_Detection=merge(Case_Count_at_Detection,map)


#Costs per site:
Cost_Sequencers = 450000
Cost_Staff = 225000
Cost_Compute_Storage = 24000
Cost_Reagents_Yearly = 2360*365*2 # 2 Machines, both run 1x/day?

Cost<-function(number){
  Yearly_cost = Cost_Staff+Cost_Compute_Storage+Cost_Reagents_Yearly
  Fixed_Cost = Cost_Sequencers
  return(number*(NPV(cf0=Fixed_Cost,cf=rep(Yearly_cost,10),times=c(1:10),i=0.03)))
}

n_Hosp=c("1","6","10")
Cost = c(Cost(1),Cost(6),Cost(10))
Cost_map=data.frame(n_Hosp,Cost)
Case_Count_at_Detection<- merge(Case_Count_at_Detection,Cost_map)

ggplot()+ geom_point(data=Case_Count_at_Detection,
                     aes(y=Cost, x=Case, color=DetectThreshold), size=1) +
  labs(x="Cases At Detection", y="Costs")+
  geom_line(data=Case_Count_at_Detection,
              aes(y=Cost, x=Case, group=DetectThreshold, color=DetectThreshold)
              )+
  ggtitle("Costs and Number of Cases at First Detection")

#Run!
shinyApp(ui, server)
