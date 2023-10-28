# First, ensure you've installed the necessary packages
library(adaptivetau)
library(future)
library(future.apply)
library(tidyverse)
library(deSolve)

# Define the SEIR transitions
transitions <- list(
  c(S = -1, E = 1), # Infection
  c(E = -1, I = 1), # Progression to infectious  
  c(E = -1, I = +1, D = +1), # Detection
  c(I = -1, R = 1), # Recovery
  c(D = +1) # Outbreak alerted
)

# Function to calculate transition rates for SEIR
SEIRrates <- function(x, params, t) {
  with(as.list(c(params, x)), {
    N <- sum(x)
    SE <- beta * S * I / N
    EI <- sigma * E
    IR <- gamma * I
    return(c(SE, EI, EI * p, IR, ifelse(D >= threshold, 1e9, 0)))
  })
}

SEIR_ode <- function(t, y, params) {
  with(as.list(c(params, y)), {
    N <- sum(y)
    dS <- -beta * S * I / N
    dE <- beta * S * I / N - sigma * E
    dI <- sigma * E - gamma * I
    dR <- gamma * I
    return(list(c(dS, dE, dI, dR)))
  })
}

# Run stochastic SEIR for a given disease multiple times using parallelization
run_SEIR <- function(disease_name, rep=100, p=0.01, threshold = 1,
  initial_state = c(S = 6500000, E = 0, I = 1, R = 0, D = 0), time = 100, deterministic = FALSE) {
  params <- Disease_Cases[[which(Disease_names == disease_name)]]$params
  params["p"] <- p
  params["threshold"] <- threshold
  
  if (deterministic) {
    results <- data.frame(time = seq(0, time, by = 1))
    out <- ode(y = initial_state[1:4], times = results$time, func = SEIR_ode, parms = params)
    results <- cbind(results, out[, 2:5])
    results <- results %>%
      mutate(rep = 1, cum_I = I + R, halted = NA) %>%
      pivot_longer(cols = -c(time, rep, halted), names_to = "variable", values_to = "value")
  } else {
    plan(multisession)
    results <- pivot_longer(bind_rows(future_lapply(1:rep, function(i) {
      out <- ssa.adaptivetau(initial_state, transitions, SEIRrates, params, tf = time, halting = 5)
      tibble_data <- as_tibble(out$dynamics) %>%
        mutate(rep = i, cum_I = I + R, halted = out$haltingTransition) # add replication number
      return(tibble_data)
    }, future.seed = TRUE)), cols = -c(time, rep, halted), names_to = "variable")
  }

  return(results)
}

plot_SEIR <- function(data, deterministic_data = NULL) {
  # Base plot
  p <- ggplot() +
       theme_light() +
       labs(x = "Days since first case", y = "Cumulative infections until detection") +
       ylim(0, 500) +
       theme(
         legend.position = "none",
         axis.title = element_text(size = 35),
         axis.text = element_text(size = 25),
         legend.title = element_text(size = 20),
         legend.text = element_text(size = 20),
         legend.key.size = unit(1.5, "cm")
       )
  
  # Add stochastic results if provided
  if (!is.null(data)) {
    last <- data %>%
      group_by(rep) %>%
      filter(!is.na(halted)) %>%
      slice_tail(n = 1)
    
    p <- p + geom_line(data = data, aes(x = time, y = value, group = interaction(variable, rep)), color = "grey80", alpha = 1) +
      geom_point(data = last, aes(x = time, y = value), size = 5, color = "red")
  }
  
  # Add deterministic results if provided
  if (!is.null(deterministic_data)) {
    p <- p + geom_line(data = deterministic_data, aes(x = time, y = value, group = variable), size = 1.5)
  }
  
  return(p)
}

# Example usage:
data_s <- run_SEIR("SARS-CoV-2", rep = 100, deterministic = F)
data_d <- run_SEIR("SARS-CoV-2", rep = 1, deterministic = T)
plot_SEIR(data_s[data_s$variable == "cum_I", ], data_d[data_d$variable == "cum_I", ])
