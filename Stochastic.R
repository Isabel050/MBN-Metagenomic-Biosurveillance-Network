# First, ensure you've installed the necessary packages
library(adaptivetau)
library(future)
library(future.apply)
library(tidyverse)
library(deSolve)

# Define the SEIR transitions
transitions <- list(
  c(S = -1, E = +1), # Infection
  c(E = -1, I = +1), # Progression to infectious
  c(E = -1, I = +1, P = +1), # The infection is severe, hospitalisation track
  c(I = -1, R = +1), # Receovered (ie no longer infectious)
  c(P = -1, H = +1), # Hospitalised but ThreatNet misses detection
  c(P = -1, H = +1, T = +1), # Hospitalised and ThreatNet detects pathogen
  c(T = 0) # Detection threshold met, outbreak alerted
)

# Function to calculate transition rates for SEIR
SEIRrates <- function(x, params, t) {
  with(as.list(c(params, x)), {
    return(c(
      beta * S * I / (S + E + I + R), # Infection
      sigma * E * c(1 - delta, # Becoming infectious, not severe
      delta), # Becoming infections, severe
      gamma * I, # Recovery
      theta * P * c(1 - mu * tau, # Hospitalisation, not detected
      mu * tau), # Hospitalisation, detected
      ifelse(T >= threshold, 1e9, 0))) # outbreak declared
  })
}

# Define a deterministic version of the SEIR model
SEIR_ode <- function(t, y, params) {
    # Calculate the transition rates using the SEIRrates function
    rates <- SEIRrates(y, params, t)
    
    # Initialize the derivatives with zeros
    dy <- setNames(rep(0, length(y)), names(y))
    
    # Calculate the derivatives based on transitions and rates
    for (i in seq_along(transitions)) {
      transition <- transitions[[i]]
      rate <- rates[i]
      for (compartment in names(transition)) {
        dy[compartment] <- dy[compartment] + transition[compartment] * rate
      }
    }
    
    # Return the list of derivatives
    return(list(dy))
}

# Run stochastic SEIR for a given disease multiple times using parallelization
run_SEIR <- function(
  disease_name, # name of disease to run
  rep = 100, # number of replicates (0 if only deterministic is wanted)
  tau = 0.77, # sensitivity of mNGS
  mu = 1, # proportion of emergency rooms connected to ThreatNet
  threshold = 1, # number of detections needed to declare outbreak
  time = 100, # number of days to run the simulation
  init = c(S = 6.5e6, E = 1, I = 0, R = 0, P = 0, H = 0, T = 0)
) {
  params <- all_params[disease_name, ]
  params["tau"] <- tau
  params["mu"] <- mu
  params["threshold"] <- threshold

  # Deterministic simulation
  out_det <- ode(y = init, times = seq(0, time, by = 1), func = SEIR_ode, parms = params)
  results_det <- tibble(
    time = out_det[, "time"],
    cum_I = out_det[, "I"] + out_det[, "R"], # cumulative infections
    rep = 0, # later we will extract the deterministic solution as rep == 0
    halted = NA # the deterministic solution continues until the end
  )

  # Stochastic simulations
  plan(multisession) # works on windows and linux
  results_sto <- bind_rows(future_lapply(seq_len(rep), function(i) {
    out <- ssa.adaptivetau(init, transitions, SEIRrates, params, tf = time, halting = length(transitions))
    tibble_data <- as_tibble(out$dynamics) %>%
      mutate(rep = i, cum_I = I + R, halted = out$haltingTransition) %>%
      select(time, cum_I, rep, halted)
  }, future.seed = TRUE)) # future.seed is needed for parallelization

  return(bind_rows(results_det, results_sto))
}

plot_SEIR <- function(data) {
  # Base plot
  p <- ggplot(data, aes(x = time, y = cum_I, group = rep)) +
    geom_line(aes(color = rep != 0, linewidth = rep != 0)) +
    scale_linewidth_manual(values = c(1.5, 0.5)) +
    labs(x = "Days since first case", y = "Cumulative infections") +
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
all_params <- read.csv("params.csv", row.names = 1)
Hospital_visitors <- read.csv("Hospital Visitors.csv")

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
Cost <- function(hospitals = 1, years = 10, d = 0.03) {
  # Costs per site:
  Cost_Sequencers <- 450000
  Cost_Staff <- 225000
  Cost_Floor_Space <- 0 # Get value.
  Cost_Compute_Storage <- 24000
  Cost_Sequencing_Reagents_Yearly <- 2300 * 365 * 2 # 2 Machines, both run 1x/day
  Costs_Reagents_Per_Sample <- 60 # Need to multiply by number of patients
  Patients_Per_Year <- 50000 # Current number. This might need to change dynamically? (Maybe todo: put in function definition and call it again.)
  Yearly_cost <- Cost_Staff + Cost_Compute_Storage + Cost_Sequencing_Reagents_Yearly + (Costs_Reagents_Per_Sample * Patients_Per_Year)

  return(hospitals * (Cost_Sequencers + sum(Yearly_cost / (1 + d)^(seq_len(years)))))
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
    labs(x = "Cost (USD millions)") +
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
# results <- expand.grid(d = row.names(all_params), t = c(1, 3, 5),
#   h = 1:nrow(Hospital_visitors))
# results$cost_mil_annu <- Cost(results$h) / 1e6 / 10
# # Generate all the simulations and save them
# data <- list()
# for (i in seq_len(nrow(results))) {
#   mu <- coverage(Hospital_visitors$Hospital[1:results$h[i]])
#   data[[i]] <- run_SEIR(results$d[i], rep = 1e3, mu = mu,
#     threshold = results$t[i])
#   print(i / nrow(results))
# }

# save(data, file = "full_data.RData")

# # Generate summary statistics from the data
# results_cases <- transform(results, output_cases = TRUE)
# results_time <- transform(results, output_cases = FALSE)
# for (i in seq_len(nrow(results))) {
#   detect <- detection_time(data[[i]])
#   results_cases[i, c("q10", "q50", "q90")] <- quantile(detect$cum_I, c(0.1, 0.5, 0.9))
#   results_time[i, c("q10", "q50", "q90")] <- quantile(detect$time, c(0.1, 0.5, 0.9))
# }

# results <- bind_rows(results_cases, results_time)
# save(results, file = "results.RData")

# # Example usage
# subset_data <- results %>% filter(output_cases == T, t == 5, d == "SARS-CoV-2 Omicron")
# plot_cost(subset_data)

# data <- run_SEIR("SARS-CoV-2", rep = 1e1, threshold = 1)
# plot_SEIR(data)
