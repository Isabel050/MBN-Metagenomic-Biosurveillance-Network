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

### Modify parameters for SARS-CoV-2 Wt and run ode

params <- c(beta=0.32, sigma=0.15, gamma=0.125)

initial_state <- c(S=6500000, E=0, I=1, R=0)

times <- 0:365

model <- ode(initial_state, times, SEIR, params)

matplot(model, type="l", lty=1, main="SEIR model", xlab="Time")

legend <- colnames(model)[2:6]

### Modify parameters for SARS-CoV-2 Omi and run ode

params <- c(beta=1.19, sigma=0.25, gamma=0.125)

initial_state <- c(S=6500000, E=0, I=1, R=0)

model_2 <- ode(initial_state, times, SEIR, params)

matplot(model_2, type="l", lty=1, main="SEIR model", xlab="Time")

legend <- colnames(model_2)[2:6]

legend("right", legend=legend, col=2:6, lty = 1)

### Modify parameters for SARS and run ode

params <- c(beta=0.24, sigma=0.25, gamma=0.1)

initial_state <- c(S=6500000, E=0, I=1, R=0)

model_3 <- ode(initial_state, times, SEIR, params)

matplot(model_3, type="l", lty=1, main="SEIR model", xlab="Time")

legend <- colnames(model_3)[2:6]

legend("right", legend=legend, col=2:6, lty = 1)

### Modify parameters for Seasonal Influenza and run ode

params <- c(beta=0.33, sigma=0.5, gamma=0.25)

initial_state <- c(S=6500000, E=0, I=1, R=0)

model_4 <- ode(initial_state, times, SEIR, params)

matplot(model_4, type="l", lty=1, main="SEIR model", xlab="Time")

legend <- colnames(model_4)[2:6]

legend("right", legend=legend, col=2:6, lty = 1)

### 1918 Influenza

params <- c(beta=0.5, sigma=0.5, gamma=0.25)

initial_state <- c(S=6500000, E=0, I=1, R=0)

model_5 <- ode(initial_state, times, SEIR, params)

matplot(model_5, type="l", lty=1, main="SEIR model", xlab="Time")

legend <- colnames(model_5)[2:6]

legend("right", legend=legend, col=2:6, lty = 1)

#Collate data into a dataframe

model_df<-data.frame(model,
                     model_2,
                     model_3,
                     model_4,
                     model_5)

write_xlsx(model_df, "model.xlsx")
