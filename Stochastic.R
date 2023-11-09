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
  c(I = -1, L = +1), # Lag until late stage infection
  c(D = +1), # Detection at transition to late stage infection
  c(L = -1, R = 1), # Recovery
  c(D = +1) # Outbreak alerted
)

# Function to calculate transition rates for SEIR
SEIRrates <- function(x, params, t) {
  with(as.list(c(params, x)), {
    N <- sum(x)
    SE <- beta * S * (I + L) / N
    EI <- sigma * E
    IL <- I / lag
    LR <- L / (1 / gamma - lag)
    return(c(SE, EI, IL, IL * p, LR, ifelse(D >= threshold, 1e9, 0)))
  })
}

# Define a deterministic version of the SEIR model
SEIR_ode <- function(t, y, params) {
  with(as.list(c(params, y)), {
    N <- sum(y)
    dS <- -beta * S * (I + L) / N
    dE <- beta * S * (I + L) / N - sigma * E
    dI <- sigma * E - I / lag
    dL <- 0 # No lag in deterministic model
    dR <- gamma * I
    dD <- 0 # No detection in the deterministic model
    return(list(c(dS, dE, dI, dL, dR, dD)))
  })
}

# Run stochastic SEIR for a given disease multiple times using parallelization
run_SEIR <- function(
    disease_name, rep = 100, p = 0.01, threshold = 1, lag = 7, time = 100,
    initial_state = c(S = 6500000, E = 0, I = 1, L = 0, R = 0, D = 0)) {
  params <- Disease_Cases[[which(Disease_names == disease_name)]]$params
  params["p"] <- p
  params["threshold"] <- threshold
  params["lag"] <- min(max(1e-9, lag), 1 / params["gamma"] - 1e-9) # lag must be positive

  # Deterministic simulation
  out_det <- ode(y = initial_state, times = seq(0, time, by = 1), func = SEIR_ode, parms = params)
  results_det <- tibble(
    time = out_det[, "time"],
    cum_I = out_det[, "I"] + out_det[, "L"] + out_det[, "R"], # cumulative infections
    rep = 0, # later we will extract the deterministic solution as rep == 0
    halted = NA # the deterministic solution continues until the end
  )

  # Stochastic simulations
  plan(multisession) # works on windows and linux
  results_sto <- bind_rows(future_lapply(1:rep, function(i) {
    out <- ssa.adaptivetau(initial_state, transitions, SEIRrates, params, tf = time, halting = 6)
    tibble_data <- as_tibble(out$dynamics) %>%
      mutate(rep = i, cum_I = I + L + R, halted = out$haltingTransition) %>%
      select(time, cum_I, rep, halted)
  }, future.seed = TRUE)) # future.seed is needed for parallelization

  return(bind_rows(results_det, results_sto))
}

plot_SEIR <- function(data) {
  # Base plot
  p <- ggplot(data, aes(x = time, y = cum_I, group = rep)) +
    geom_line(aes(color = rep != 0, linewidth = rep != 0)) +
    scale_linewidth_manual(values = c(1.5, 0.5)) +
    labs(x = "Days since first case", y = "Cumulative infections until detection") +
    # Fix the axes to be the same for all plots
    scale_x_continuous(
      breaks = seq(0, 100, by = 20),
      minor_breaks = seq(0, 100, by = 10), limits = c(0, 100)
    ) +
    scale_y_continuous(
      breaks = seq(0, 500, by = 100),
      minor_breaks = seq(0, 500, by = 50)
    ) +
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
detection_time <- function(data) {
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

  return(hospitals * (Cost_Sequencers + sum(Yearly_cost / (1.03)^(1:years))))
}

# Function to plot the cost vs. detection time or infections
plot_cost <- function(results) {
  p <- ggplot(results, aes(x = cost_mil_annu, group = interaction(t, d))) +
    geom_ribbon(aes(ymin = q10, ymax = q90), fill = "grey80", alpha = 0.5) +
    geom_line(aes(y = q50), linewidth = 1.5, color = "blue") +
    scale_x_continuous(
      breaks = seq(0, 120, by = 20),
      minor_breaks = seq(0, 120, by = 10), limits = c(0, 120)
    ) +
    labs(x = "10y discounted cost (USD millions)") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.position = "none"
    )
  if (results$output_cases[1]) {
    p <- p + labs(y = "Infections until detection") +
      scale_y_continuous(
        breaks = seq(0, 500, by = 100),
        minor_breaks = seq(0, 500, by = 50)
      ) +
      coord_cartesian(ylim = c(0, 500))
  } else {
    p <- p + labs(y = "Days until detection") +
      scale_y_continuous(
        breaks = seq(0, 100, by = 20),
        minor_breaks = seq(0, 100, by = 10)
      ) +
      coord_cartesian(ylim = c(0, 100))
  }
  return(p)
}

# # Create a dataframe with all combinations of parameters
# takes ~2 hours, so leave commented unless needed
# results <- expand.grid(d = Disease_names, t = 1:5,
#   h = 1:nrow(Hospital_visitors), output_cases = c(FALSE, TRUE))
# results$cost_mil_annu <- Cost(results$h) / 1e6 / 10
# # Generate all the simulations and save them
# data <- list()
# for (i in seq_len(nrow(results))) {
#   p <- delta * tau * coverage(Hospital_visitors$Hospital[1:results$h[i]])
#   data[[i]] <- run_SEIR(results$d[i], rep = 1e3, p = p, threshold = results$t[i])
#   print(i / nrow(results))
# }

# save(data, file = "full_data.RData")

# # Generate summary statistics from the data
# for (i in seq_len(nrow(results))) {
#   detect <- detection_time(data[[i]])
#   if (results$output_cases[i]) {
#     temp <- detect$cum_I
#   } else {
#     temp <- detect$time
#   }
#   results[i, c("q10", "q50", "q90")] <- quantile(temp, c(0.1, 0.5, 0.9))
# }

# save(results, file = "results.RData")

# # Example usage
# subset_data <- results %>% filter(output_cases == T, t == 5, d == "SARS-CoV-2 Omicron")
# plot_cost(subset_data)

# data <- run_SEIR("SARS-CoV-2 Omicron", rep = 1e1, p = 0.01, threshold = 1)
# plot_SEIR(data)
