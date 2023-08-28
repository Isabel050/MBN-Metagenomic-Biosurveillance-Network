
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

#Disease Names *Cannot* start with numbers, or the systme breaks!

# We want a list of possible SEIR models to run, and will use that for the interface.
Disease_Cases <- list(list(name="SARS-CoV-2", params=c(beta=0.32, sigma=0.15, gamma=0.125)),
                      list(name="SARS-CoV-2 Omicron", params=c(beta=1.19, sigma=0.25, gamma=0.125)),
                      list(name="SARS", params=c(beta=0.24, sigma=0.25, gamma=0.1)),
                      list(name="Seasonal Flu", params=c(beta=0.33, sigma=0.5, gamma=0.25)),
                      list(name="Pandemic 1918 Flu", params=c(beta=0.5, sigma=0.5, gamma=0.25)),
                      list(name="MERS", params=c(beta=0.11, sigma=0.18, gamma=0.13))
)
Disease_names=c()
for (i in 1:length(Disease_Cases)){
  Disease_names=append(Disease_names,Disease_Cases[[i]]$name)
}
