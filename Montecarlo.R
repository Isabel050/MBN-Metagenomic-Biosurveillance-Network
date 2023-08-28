library(writexl)
library(openxlsx)

#Manual rerun if there are changes: ScenariosRun=FALSE

#Percentage Coverage is based on scenarios for hospitals.
#These three *MUST* have the same order.
Coverage_names = c("Only Sheba","Top 2", "Six Largest", "Ten Largest", "Sixteen Largest")
n_Hosp=c(1,2,6,10,16)
Coverage_rates = c(7,14 ,37, 56, 75)


#Thresholds are the number of detections (cases) needed for response.
Thresholds = c(1,3,5)


generate_scenario <- function(t,s,e,threshold,scen_name,case=FALSE){
  Sim<-list()
  for(i in 0:1001) { Sim[[i+1]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=threshold,1,0)}
  Case<-list()
  for(i in 0:1001) { Case[[i+1]]<-apply(X=Sim[[i+1]], MARGIN=2, FUN=mean)}
  
  Scen_Frame<- data.frame(Case)
  colnames(Scen_Frame)<- c(0:1001)
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
  
  
  #NEW:Pivot Master Frame 1a longer and subset to only include detection threshold=1
  Master_Frame_c <-Master_Frame_a %>%
    pivot_longer(!c("Case","Cases_continuous"), names_to = "Coverage", values_to = "Cumulative_probability") 
  
  
  write_xlsx(Master_Frame_a, "Master_Frame_a.xlsx")
  write_xlsx(Master_Frame_b, "Master_Frame_b.xlsx")
  write_xlsx(Master_Frame_c, "Master_Frame_c.xlsx")
  
  ScenariosRun= TRUE
}

Master_Frame_a <- read.xlsx("Master_Frame_a.xlsx")
Master_Frame_b <- read.xlsx("Master_Frame_b.xlsx")
Master_Frame_c <- read.xlsx("Master_Frame_c.xlsx")