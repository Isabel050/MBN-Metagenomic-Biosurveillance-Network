# First, ensure you've installed the necessary packages
library(adaptivetau)
library(future)
library(future.apply)
library(tidyverse)

# Define the SEIR transitions
transitions = list(
  c(S = -1, E = 1),               # Infection
  c(E = -1, I = 1),               # Progression to infectious
  c(I = -1, R = 1)                # Recovery
)

# Function to calculate transition rates for SEIR
SEIRrates <- function(x, params, t) {
  N <- sum(x)
  lambda_SE <- (params["beta"] * x["S"] * x["I"]) / N
  lambda_EI <- params["sigma"] * x["E"]
  lambda_IR <- params["gamma"] * x["I"]
  
  return(c(lambda_SE, lambda_EI, lambda_IR))
}


# Run stochastic SEIR for a given disease
run_SEIR <- function(disease_name, rep = 1) {
  params <- Disease_Cases[[which(Disease_names == disease_name)]]$params
  plan(multisession)
  results <- bind_rows(future_lapply(1:rep, function(x) {
    out <- ssa.adaptivetau(initial_state, transitions, SEIRrates, params, tf = max(times))
    df <- as_tibble(out)
    return(df)
  }, future.seed = TRUE))
  return(df)
}

# Run stochastic SEIR for a given disease multiple times using parallelization
run_SEIR <- function(disease_name, n_times=100) {
  params <- Disease_Cases[[which(Disease_names == disease_name)]]$params
  results <- pivot_longer(bind_rows(future_lapply(1:n_times, function(x) {
    out <- ssa.adaptivetau(initial_state, transitions, SEIRrates, params, tf = max(times))
    tibble_data <- as_tibble(out) %>%
      mutate(rep = x) # add replication number
    return(tibble_data)
  }, future.seed = TRUE)), cols = -c(time, rep), names_to = "variable")
}


# Your existing data
Disease_Cases <- list(
  list(name = "SARS-CoV-2", params = c(beta = 0.32, sigma = 0.15, gamma = 0.125)),
  list(name = "SARS-CoV-2 Omicron", params = c(beta = 1.19, sigma = 0.25, gamma = 0.125)),
  list(name = "SARS", params = c(beta = 0.24, sigma = 0.25, gamma = 0.1)),
  list(name = "Seasonal Flu", params = c(beta = 0.33, sigma = 0.5, gamma = 0.25)),
  list(name = "Pandemic 1918 Flu", params = c(beta = 0.5, sigma = 0.5, gamma = 0.25)),
  list(name = "MERS", params = c(beta = 0.11, sigma = 0.18, gamma = 0.13))
)
Disease_names <- sapply(Disease_Cases, function(x) x$name)

initial_state <- c(S = 6500000, E = 0, I = 1, R = 0)
times <- 0:365

# Setting up parallelization
plan(multisession)

# Example usage:
data_for_SARS_CoV_2 <- run_SEIR("SARS-CoV-2", 2)