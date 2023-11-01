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
      mutate(rep = i, cum_I = I + R, halted = out$haltingTransition) %>%
      select(time, cum_I, rep, halted)
  }, future.seed = TRUE))

  return(bind_rows(results_det, results_sto))
}

plot_SEIR <- function(data) {
  # Base plot
  p <- ggplot(data, aes(x = time, y = cum_I, group = rep)) +
       geom_line(aes(color = rep != 0, linewidth = rep != 0)) +
       scale_linewidth_manual(values = c(1.5, 0.5)) +
       labs(x = "Days since first case", y = "Cumulative infections until detection") +
       scale_x_continuous(breaks = seq(0, 100, by = 20),
          minor_breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
       scale_y_continuous(breaks = seq(0, 500, by = 100),
          minor_breaks = seq(0, 500, by = 50)) +
       coord_cartesian(ylim = c(0, 500)) +
       theme_light() +
       scale_color_manual(values = c("blue", "grey80")) +
       theme(
         axis.title = element_text(size = 30),
         axis.text = element_text(size = 20),
         legend.position = "none"
       )

  # Highlight points where the outbreak is detected in stochastic runs
  last <- data %>%
      group_by(rep) %>%
      filter(!is.na(halted)) %>%
      slice_tail(n = 1)
  percentile_y <- quantile(last$cum_I, probs = c(0.1, 0.5, 0.9))
  percentile_x <- quantile(last$time, probs = c(0.1, 0.5, 0.9))
  p <- p + geom_hline(yintercept = percentile_y, linetype = "dashed", color = "black") +
       geom_vline(xintercept = percentile_x, linetype = "dashed", color = "black") +
       geom_point(data = last, aes(x = time, y = cum_I), size = 5, color = "red")
  return(p)
}

# Load data
Disease_Cases <- list(
  list(name = "SARS-CoV-2", params = c(beta = 0.32, sigma = 0.15, gamma = 0.125)),
  list(name = "SARS-CoV-2 Omicron", params = c(beta = 1.19, sigma = 0.25, gamma = 0.125)),
  list(name = "SARS", params = c(beta = 0.24, sigma = 0.25, gamma = 0.1)),
  list(name = "Seasonal Flu", params = c(beta = 0.33, sigma = 0.5, gamma = 0.25)),
  list(name = "Pandemic 1918 Flu", params = c(beta = 0.5, sigma = 0.5, gamma = 0.25)),
  list(name = "MERS", params = c(beta = 0.11, sigma = 0.18, gamma = 0.13))
)
Disease_names <- sapply(Disease_Cases, function(x) x$name)

Hospital_visitors <- read.csv("Hospital Visitors.csv")

delta <- 0.25 # Probability of infectious person visiting ER
tau <- 0.77 # sensitivity of mNGS

# Define hospital coverage
coverage <- function(hospitals, visitors = Hospital_visitors) {
  total <- sum(visitors$Visitors.2021)
  selected <- sum(visitors$Visitors.2021[visitors$Hospital %in% hospitals])
  return(selected / total)
}

# Compute the detection time for a given dataset
detection_time <- function(data){
    detected <- data %>%
      group_by(rep) %>%
      filter(!is.na(halted)) %>%
      summarize(time = max(time), cum_I = max(cum_I))
    return(detected)
}

# Find the cost for a given number of hospitals
Cost <- function(hospitals = 1, years = 10) {
  # Costs per site:
  Cost_Sequencers <- 450000
  Cost_Staff <- 225000
  Cost_Floor_Space <- 0 # Get value.
  Cost_Compute_Storage <- 24000
  Cost_Sequencing_Reagents_Yearly <- 2300 * 365 * 2 # 2 Machines, both run 1x/day
  Costs_Reagents_Per_Sample <- 60 # Need to multiply by number of patients
  Patients_Per_Year <- 50000 # Current number. This might need to change dynamically? (Maybe todo: put in function definition and call it again.)
  Yearly_cost <- Cost_Staff + Cost_Compute_Storage + Cost_Sequencing_Reagents_Yearly + (Costs_Reagents_Per_Sample * Patients_Per_Year)

  return(hospitals * (Cost_Sequencers + sum(Yearly_cost / (1.03) ^ (1:years))))
}

plot_cost <- function(data_subset) {
  p <- ggplot(data_subset, aes(x = cost_mil_annu, group = interaction(t, d))) +
    geom_ribbon(aes(ymin = q10, ymax = q90), fill = "grey80", alpha = 0.5) +
    geom_line(aes(y = q50), linewidth = 1.5, color = "blue") +
    scale_x_continuous(breaks = seq(0, 120, by = 20),
      minor_breaks = seq(0, 120, by = 10), limits = c(0, 120)) +
    labs(x = "10y discounted cost (USD millions)") +
    theme_minimal()+
       theme(
         axis.title = element_text(size = 30),
         axis.text = element_text(size = 20),
         legend.position = "none"
       )
  if (data_subset$output_cases[1]) {
    p <- p + labs(y = "Infections until detection") +
      scale_y_continuous(breaks = seq(0, 500, by = 100),
        minor_breaks = seq(0, 500, by = 50), limits = c(0, 500))
  } else {
    p <- p + labs(y = "Days until detection") +
      scale_y_continuous(breaks = seq(0, 100, by = 20),
        minor_breaks = seq(0, 100, by = 10), limits = c(0, 100))
  }
  return(p)
}

# # Create a dataframe with all combinations of parameters
# results <- expand.grid(d = Disease_names, t = 1:5,
#   h = 1:nrow(Hospital_visitors), output_cases = c(FALSE, TRUE))
# results$cost_mil_annu <- Cost(results$h) / 1e6 / 10
# for (i in seq_len(nrow(results))) {
#   p <- delta * tau * coverage(Hospital_visitors$Hospital[1:results$h[i]])
#   data <- run_SEIR(results$d[i], rep = 1e3, p = p, threshold = results$t[i])
#   detect <- detection_time(data)
#   if (results$output_cases[i]) {
#     temp <- detect$cum_I
#   } else {
#     temp <- detect$time
#   }
#   results[i, c("q10", "q50", "q90")] <- quantile(temp, c(0.1, 0.5, 0.9))
#   print(i / nrow(results))
# }

# save(results, file = "results.RData")

# # Example usage
# subset_data <- results %>% filter(output_cases == F, t == 1)
# plot_cost(subset_data)

# data <- run_SEIR("SARS-CoV-2 Omicron", rep = 1e1, p = 0.01, threshold = 1)
# plot_SEIR(data)
