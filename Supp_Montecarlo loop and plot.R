library(writexl)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(reshape2)
install.packages("reshape2")


#10% Hospital coverage; detection threshold = 1
#proportion of Threat Net hospitals
t<- 0.1
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

#Simulate detection with increasing cases using a binomial distribution
Sim_10_1<-list()
for(i in 1:1000) { Sim_10_1[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=1,1,0)}

#Find the mean per number of cases per simulation (10 simulations per case) for graph
Case_10_1<-list()
for(i in 1:1000) { Case_10_1[[i]]<-apply(X=Sim_10_1[[i]], MARGIN=2, FUN=mean)}

#Add to data frame
Frame_10_1_1<- data.frame(Case_10_1)
colnames(Frame_10_1_1)<- c(1:1000)
Frame_10_1_2<-melt(Frame_10_1_1)
colnames(Frame_10_1_2)<-c("Case","Probability_10%_1_a")

#Find the mean per number of cases
Frame_10_1_3<-aggregate(Frame_10_1_2$`Probability_10%_1`,by=list(Frame_10_1_2$Case), FUN=mean)
colnames(Frame_10_1_3)<-c("Case","Probability_10%_1_b")


#Repeat above for increasing hospital coverage and detection threshold
#30% Hospital coverage; detection threshold = 1
#proportion of threatnet hospitals
t<- 0.3
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_30_1<-list()
for(i in 1:1000) { Sim_30_1[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=1,1,0)}
Case_30_1<-list()
for(i in 1:1000) { Case_30_1[[i]]<-apply(X=Sim_30_1[[i]], MARGIN=2, FUN=mean)}

Frame_30_1_1<- data.frame(Case_30_1)
colnames(Frame_30_1_1)<- c(1:1000)
Frame_30_1_2<-melt(Frame_30_1_1)
colnames(Frame_30_1_2)<-c("Case","Probability_30%_1_a")

Frame_30_1_3<-aggregate(Frame_30_1_2$`Probability_30%_1_a`,by=list(Frame_30_1_2$Case), FUN=mean)
colnames(Frame_30_1_3)<-c("Case","Probability_30%_1_b")




#50% Hospital coverage; detection threshold = 1
#proportion of threatnet hospitals
t<- 0.5
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_50_1<-list()
for(i in 1:1000) { Sim_50_1[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=1,1,0)}
Case_50_1<-list()
for(i in 1:1000) { Case_50_1[[i]]<-apply(X=Sim_50_1[[i]], MARGIN=2, FUN=mean)}

Frame_50_1_1<- data.frame(Case_50_1)
colnames(Frame_50_1_1)<- c(1:1000)
Frame_50_1_2<-melt(Frame_50_1_1)
colnames(Frame_50_1_2)<-c("Case","Probability_50%_1_a")

Frame_50_1_3<-aggregate(Frame_50_1_2$`Probability_50%_1_a`,by=list(Frame_50_1_2$Case), FUN=mean)
colnames(Frame_50_1_3)<-c("Case","Probability_50%_1_b")



#70% Hospital coverage; detection threshold = 1
#proportion of threatnet hospitals
t<- 0.7
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_70_1<-list()
for(i in 1:1000) { Sim_70_1[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=1,1,0)}
Case_70_1<-list()
for(i in 1:1000) { Case_70_1[[i]]<-apply(X=Sim_70_1[[i]], MARGIN=2, FUN=mean)}

Frame_70_1_1<- data.frame(Case_70_1)
colnames(Frame_70_1_1)<- c(1:1000)
Frame_70_1_2<-melt(Frame_70_1_1)
colnames(Frame_70_1_2)<-c("Case","Probability_70%_1_a")

Frame_70_1_3<-aggregate(Frame_70_1_2$`Probability_70%_1_a`,by=list(Frame_70_1_2$Case), FUN=mean)
colnames(Frame_70_1_3)<-c("Case","Probability_70%_1_b")

#10% Hospital coverage; detection threshold = 3
#proportion of threatnet hospitals
t<- 0.1
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_10_3<-list()
for(i in 1:1000) { Sim_10_3[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=3,1,0)}
Case_10_3<-list()
for(i in 1:1000) { Case_10_3[[i]]<-apply(X=Sim_10_3[[i]], MARGIN=2, FUN=mean)}

Frame_10_3_1<- data.frame(Case_10_3)
colnames(Frame_10_3_1)<- c(1:1000)
Frame_10_3_2<-melt(Frame_10_3_1)
colnames(Frame_10_3_2)<-c("Case","Probability_10%_3_a")

Frame_10_3_3<-aggregate(Frame_10_3_2$`Probability_10%_3_a`,by=list(Frame_10_3_2$Case), FUN=mean)
colnames(Frame_10_3_3)<-c("Case","Probability_10%_3_b")

#30% Hospital coverage; detection threshold = 3
#proportion of threatnet hospitals
t<- 0.3
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_30_3<-list()
for(i in 1:1000) { Sim_30_3[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=3,1,0)}
Case_30_3<-list()
for(i in 1:1000) { Case_30_3[[i]]<-apply(X=Sim_30_3[[i]], MARGIN=2, FUN=mean)}

Frame_30_3_1<- data.frame(Case_30_3)
colnames(Frame_30_3_1)<- c(1:1000)
Frame_30_3_2<-melt(Frame_30_3_1)
colnames(Frame_30_3_2)<-c("Case","Probability_30%_3_a")

Frame_30_3_3<-aggregate(Frame_30_3_2$`Probability_30%_3_a`,by=list(Frame_30_3_2$Case), FUN=mean)
colnames(Frame_30_3_3)<-c("Case","Probability_30%_3_b")



#50% Hospital coverage; detection threshold = 3
#proportion of threatnet hospitals
t<- 0.5
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_50_3<-list()
for(i in 1:1000) { Sim_50_3[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=3,1,0)}
Case_50_3<-list()
for(i in 1:1000) { Case_50_3[[i]]<-apply(X=Sim_50_3[[i]], MARGIN=2, FUN=mean)}

Frame_50_3_1<- data.frame(Case_50_3)
colnames(Frame_50_3_1)<- c(1:1000)
Frame_50_3_2<-melt(Frame_50_3_1)
colnames(Frame_50_3_2)<-c("Case","Probability_50%_3_a")

Frame_50_3_3<-aggregate(Frame_50_3_2$`Probability_50%_3_a`,by=list(Frame_50_3_2$Case), FUN=mean)
colnames(Frame_50_3_3)<-c("Case","Probability_50%_3_b")

#70% Hospital coverage; detection threshold = 3
#proportion of threatnet hospitals
t<- 0.7
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_70_3<-list()
for(i in 1:1000) { Sim_70_3[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=3,1,0)}
Case_70_3<-list()
for(i in 1:1000) { Case_70_3[[i]]<-apply(X=Sim_70_3[[i]], MARGIN=2, FUN=mean)}

Frame_70_3_1<- data.frame(Case_70_3)
colnames(Frame_70_3_1)<- c(1:1000)
Frame_70_3_2<-melt(Frame_70_3_1)
colnames(Frame_70_3_2)<-c("Case","Probability_70%_3_a")

Frame_70_3_3<-aggregate(Frame_70_3_2$`Probability_70%_3_a`,by=list(Frame_70_3_2$Case), FUN=mean)
colnames(Frame_70_3_3)<-c("Case","Probability_70%_3_b")


#10% Hospital coverage; detection threshold = 5
#proportion of threatnet hospitals
t<- 0.1
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_10_5<-list()
for(i in 1:1000) { Sim_10_5[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=5,1,0)}
Case_10_5<-list()
for(i in 1:1000) { Case_10_5[[i]]<-apply(X=Sim_10_5[[i]], MARGIN=2, FUN=mean)}

Frame_10_5_1<- data.frame(Case_10_5)
colnames(Frame_10_5_1)<- c(1:1000)
Frame_10_5_2<-melt(Frame_10_5_1)
colnames(Frame_10_5_2)<-c("Case","Probability_10%_5_a")

Frame_10_5_3<-aggregate(Frame_10_5_2$`Probability_10%_5_a`,by=list(Frame_10_5_2$Case), FUN=mean)
colnames(Frame_10_5_3)<-c("Case","Probability_10%_5_b")

#30% Hospital coverage; detection threshold = 5
#proportion of threatnet hospitals
t<- 0.3
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_30_5<-list()
for(i in 1:1000) { Sim_30_5[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=5,1,0)}
Case_30_5<-list()
for(i in 1:1000) { Case_30_5[[i]]<-apply(X=Sim_30_5[[i]], MARGIN=2, FUN=mean)}

Frame_30_5_1<- data.frame(Case_30_5)
colnames(Frame_30_5_1)<- c(1:1000)
Frame_30_5_2<-melt(Frame_30_5_1)
colnames(Frame_30_5_2)<-c("Case","Probability_30%_5_a")

Frame_30_5_3<-aggregate(Frame_30_5_2$`Probability_30%_5_a`,by=list(Frame_30_5_2$Case), FUN=mean)
colnames(Frame_30_5_3)<-c("Case","Probability_30%_5_b")

#50% Hospital coverage; detection threshold = 5
#proportion of threatnet hospitals
t<- 0.5
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_50_5<-list()
for(i in 1:1000) { Sim_50_5[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=5,1,0)}
Case_50_5<-list()
for(i in 1:1000) { Case_50_5[[i]]<-apply(X=Sim_50_5[[i]], MARGIN=2, FUN=mean)}

Frame_50_5_1<- data.frame(Case_50_5)
colnames(Frame_50_5_1)<- c(1:1000)
Frame_50_5_2<-melt(Frame_50_5_1)
colnames(Frame_50_5_2)<-c("Case","Probability_50%_5_a")

Frame_50_5_3<-aggregate(Frame_50_5_2$`Probability_50%_5_a`,by=list(Frame_50_5_2$Case), FUN=mean)
colnames(Frame_50_5_3)<-c("Case","Probability_50%_5_b")

#70% Hospital coverage; detection threshold = 5
#proportion of threatnet hospitals
t<- 0.7
#proportion who seek care
s<- 0.5
#proportion who attend ED
e<- 0.25

Sim_70_5<-list()
for(i in 1:1000) { Sim_70_5[[i]]<-ifelse(replicate(n=10, rbinom(1000,i,s*e*t))>=5,1,0)}
Case_70_5<-list()
for(i in 1:1000) { Case_70_5[[i]]<-apply(X=Sim_70_5[[i]], MARGIN=2, FUN=mean)}

Frame_70_5_1<- data.frame(Case_70_5)
colnames(Frame_70_5_1)<- c(1:1000)
Frame_70_5_2<-melt(Frame_70_5_1)
colnames(Frame_70_5_2)<-c("Case","Probability_70%_5_a")

Frame_70_5_3<-aggregate(Frame_70_5_2$`Probability_70%_5_a`,by=list(Frame_70_5_2$Case), FUN=mean)
colnames(Frame_70_5_3)<-c("Case","Probability_70%_5_b")


#Collate data
Master_Frame_1_a<- data.frame(
  Frame_10_1_2, 
  Frame_30_1_2, 
  Frame_50_1_2,
  Frame_70_1_2,
  Frame_10_3_2,
  Frame_30_3_2,
  Frame_50_3_2,
  Frame_70_3_2,
  Frame_10_5_2,
  Frame_30_5_2,
  Frame_50_5_2,
  Frame_70_5_2)

Master_Frame_1_b<- data.frame(
  Frame_10_1_3,
  Frame_30_1_3,
  Frame_50_1_3,
  Frame_70_1_3,
  Frame_10_3_3,
  Frame_30_3_3,
  Frame_50_3_3,
  Frame_70_3_3,
  Frame_10_5_3,
  Frame_30_5_3,
  Frame_50_5_3,
  Frame_70_5_3)


#Reformat cases to numeric data 
Master_Frame_1_a$Cases_continuous<-as.numeric(as.character(Master_Frame_1_a$Case))
Master_Frame_1_b$Cases_continuous_b<-as.numeric(as.character(Master_Frame_1_b$Case))


#Plot probability vs infections graph

p1<-ggplot() + 
  geom_point(data=Master_Frame_1_c, aes(y=Cumulative_probability, x=Infections, color=Coverage), size=1)+
  geom_smooth(data=Master_Frame_1_b, aes(y=Probability_10._1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  geom_smooth(data=Master_Frame_1_b, aes(y=Probability_30._1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  geom_smooth(data=Master_Frame_1_b, aes(y=Probability_50._1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  geom_smooth(data=Master_Frame_1_b, aes(y=Probability_70._1_b, x=Cases_continuous_b), method="gam",color="black",se=FALSE, linetype=5, size=1.3) +
  labs(x="Infections", y="Cumulative probability")+
  xlim(0,100)

p1+ theme_gray()+
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

