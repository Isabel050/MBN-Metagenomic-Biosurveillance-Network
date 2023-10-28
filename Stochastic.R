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
run_SEIR <- function(disease_name, rep = 100, p = 0.01, threshold = 1,
  initial_state = c(S = 6500000, E = 0, I = 1, R = 0, D = 0), time = 100) {

  params <- Disease_Cases[[which(Disease_names == disease_name)]]$params
  params["p"] <- p
  params["threshold"] <- threshold
  
  # Deterministic simulation
  out_det <- ode(y = initial_state[1:4], times = seq(0, time, by = 1), func = SEIR_ode, parms = params)
  results_det <- tibble(
    time = out_det[, "time"],
    cum_I = out_det[, "I"] + out_det[, "R"],
    rep = 0,
    halted = NA
  )
  
  # Stochastic simulations
  plan(multisession)
  results_sto <- bind_rows(future_lapply(1:rep, function(i) {
    out <- ssa.adaptivetau(initial_state, transitions, SEIRrates, params, tf = time, halting = 5)
    tibble_data <- as_tibble(out$dynamics) %>%
      mutate(rep = i, cum_I = I + R, halted = out$haltingTransition)
  }, future.seed = TRUE))
  
  return(bind_rows(results_det, results_sto))
}

plot_SEIR <- function(data) {
  # Base plot
  p <- ggplot(data, aes(x = time, y = cum_I, group = rep)) +
       geom_line(aes(color = rep != 0, size = rep != 0)) +
       scale_size_manual(values = c(1.5, 0.5)) +
       labs(x = "Days since first case", y = "Cumulative infections until detection") +
       ylim(0, 500) +
       theme_light() +
       scale_color_manual(values = c("blue", "grey80")) +
       theme(
         axis.title = element_text(size = 35),
         axis.text = element_text(size = 25),
         legend.position = "none"
       )

  # Highlight points where the outbreak is detected in stochastic runs
  last <- data %>%
      group_by(rep) %>%
      filter(!is.na(halted)) %>%
      slice_tail(n = 1)
  p <- p + geom_point(data = last, aes(x = time, y = cum_I), size = 5, color = "red")
  
  return(p)
}

# Example usage:
data <- run_SEIR("SARS-CoV-2", rep = 10)
plot_SEIR(data)