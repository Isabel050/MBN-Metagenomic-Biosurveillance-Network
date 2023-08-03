require(deSolve)

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

#Replace individual runs with a list of what to run.
Disease_Cases <- list(list(name="SARS-CoV-2", params=c(beta=0.32, sigma=0.15, gamma=0.125)),
                      list(name="SARS-CoV-2 Omicron", params=c(beta=1.19, sigma=0.25, gamma=0.125)),
                      list(name="SARS", params=c(beta=0.24, sigma=0.25, gamma=0.1)),
                      list(name="Seasonal Flu", params=c(beta=0.33, sigma=0.5, gamma=0.25)),
                      list(name="1918 Flu", params=c(beta=0.5, sigma=0.5, gamma=0.25)),
                      list(name="MERS", params=c(beta=0.9, sigma=0.9, gamma=0.9))
)
Disease_names=c()
for (i in 1:length(Disease_Cases)){
  Disease_names=append(Disease_names,Disease_Cases[[i]]$name)
}

#Across all diseases:
initial_state <- c(S=6500000, E=0, I=1, R=0)
times <- 0:365

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
