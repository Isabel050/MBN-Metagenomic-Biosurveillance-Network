library(writexl)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(reshape2)
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

Percentages = c(10,30,50,70)
Thresholds = c(1,3,5)

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


#Plot probability vs infections graph


p1<-ggplot() + 
  geom_point(data=Master_Frame_a, aes(y=Scen_10_1_a, x=Cases_continuous), color="coral3", size=1)+
  geom_point(data=Master_Frame_a, aes(y=Scen_30_1_a, x=Cases_continuous), color="darkslategray3", size=1)+
  geom_point(data=Master_Frame_a, aes(y=Scen_50_1_a, x=Cases_continuous), color="darkseagreen3", size=1)+
  geom_point(data=Master_Frame_a, aes(y=Scen_70_1_a, x=Cases_continuous), color="darkorchid3", size=1)+
  geom_smooth(data=Master_Frame_b, aes(y=Scen_10_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  geom_smooth(data=Master_Frame_b, aes(y=Scen_30_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  geom_smooth(data=Master_Frame_b, aes(y=Scen_50_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  geom_smooth(data=Master_Frame_b, aes(y=Scen_70_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  labs(x="Infections", y="Cumulative probability")+
  xlim(0,100)

p2<-ggplot() + 
  # geom_point(data=Master_Frame_1_c, aes(y=Cumulative_probability, x=Infections, color=Coverage), size=1)+
  geom_smooth(data=Master_Frame_b, aes(y=Scen_10_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  geom_smooth(data=Master_Frame_b, aes(y=Scen_30_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  geom_smooth(data=Master_Frame_b, aes(y=Scen_50_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  geom_smooth(data=Master_Frame_b, aes(y=Scen_70_1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
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

write_xlsx(Master_Frame_1_a, "Master_Frame_1_a.xlsx")
write_xlsx(Master_Frame_1_b, "Master_Frame_1_b.xlsx")
