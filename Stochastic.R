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
    H = out_det[, "H"], # hospitalisations
    rep = 0, # later we will extract the deterministic solution as rep == 0
    halted = NA # the deterministic solution continues until the end
  )

  # Stochastic simulations
  plan(multisession) # works on windows and linux
  results_sto <- bind_rows(future_lapply(seq_len(rep), function(i) {
    out <- ssa.adaptivetau(init, transitions, SEIRrates, params, tf = time, halting = length(transitions))
    tibble_data <- as_tibble(out$dynamics) %>%
      mutate(rep = i, cum_I = I + R, halted = out$haltingTransition) %>%
      select(time, cum_I, H, rep, halted)
  }, future.seed = TRUE)) # future.seed is needed for parallelization

  return(bind_rows(results_det, results_sto))
}

# New function for running SEIR with custom parameters
run_SEIR_custom <- function(
  custom_params, # custom parameter set defined by user
  rep = 100, # number of replicates (0 if only deterministic is wanted)
  tau = 0.77, # sensitivity of mNGS
  mu = 1, # proportion of emergency rooms connected to ThreatNet
  threshold = 1, # number of detections needed to declare outbreak
  time = 100, # number of days to run the simulation
  init = c(S = 6.5e6, E = 1, I = 0, R = 0, P = 0, H = 0, T = 0)
) {
  # Create parameter vector for simulation
  params <- custom_params
  params["tau"] <- tau
  params["mu"] <- mu
  params["threshold"] <- threshold

  # Deterministic simulation
  out_det <- ode(y = init, times = seq(0, time, by = 1), func = SEIR_ode, parms = params)
  results_det <- tibble(
    time = out_det[, "time"],
    cum_I = out_det[, "I"] + out_det[, "R"], # cumulative infections
    H = out_det[, "H"], # hospitalisations
    rep = 0, # later we will extract the deterministic solution as rep == 0
    halted = NA # the deterministic solution continues until the end
  )

  # Stochastic simulations
  plan(multisession) # works on windows and linux
  results_sto <- bind_rows(future_lapply(seq_len(rep), function(i) {
    out <- ssa.adaptivetau(init, transitions, SEIRrates, params, tf = time, halting = length(transitions))
    tibble_data <- as_tibble(out$dynamics) %>%
      mutate(rep = i, cum_I = I + R, halted = out$haltingTransition) %>%
      select(time, cum_I, H, rep, halted)
  }, future.seed = TRUE)) # future.seed is needed for parallelization

  return(bind_rows(results_det, results_sto))
}

plot_SEIR <- function(data) {
  # Calculate relevant summary statistics
  last <- data %>%
    group_by(rep) %>%
    filter(!is.na(halted)) %>%
    slice_tail(n = 1)
  percentile_y <- quantile(last$cum_I, probs = c(0.25, 0.5, 0.75))
  percentile_x <- quantile(last$time, probs = c(0.25, 0.5, 0.75))
  percentiles <- data.frame(time = percentile_x, cum_I = percentile_y)
  y_lim <- 500 * ceiling(percentile_y[3] / 500)

  # Plot the individual simulation runs
  p <- ggplot(data = data[data$rep != 0, ]) +
    geom_line(aes(x = time, y = cum_I, group = rep), color = "grey80") +
    labs(x = "Days since first case", y = "Cumulative infections in population") +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, y_lim)) +
    theme_light() +
    scale_color_manual(values = c("grey80")) +
    theme(
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.position = "none",
      panel.grid.major = element_blank(), # Removes major grid lines
      panel.grid.minor = element_blank(), # Removes minor grid lines
      panel.background = element_blank()
    )

  # Add median dot and IQR error bars
  p <- p +
    geom_errorbar(aes(x = percentile_x[2], ymin = percentile_y[1], ymax = percentile_y[3]), 
                  width = 2, linewidth = 1) +
    geom_errorbarh(aes(y = percentile_y[2], xmin = percentile_x[1], xmax = percentile_x[3]), 
                   height = y_lim / 50, linewidth = 1) +
    geom_point(data = percentiles[2, ], aes(x = time, y = cum_I), size=5, shape=16, color="blue")
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
    summarize(time = max(time), cum_I = max(cum_I), H = max(H))
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
  y_lim <- 50 * ceiling(max(results$q50) / 50)
  y_label <- str_replace_all(results$output[1],
  c("cases" = "Infections in population until detection",
    "hosp" = "ED presentations until detection",
    "time" = "Time (days) until detection"))
  p <- ggplot(results, aes(x = cost_mil_annu, group = interaction(t, d))) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "grey80", alpha = 0.5) +
    geom_line(aes(y = q50), linewidth = 1.5, color = "blue") +
    scale_x_continuous(breaks = seq(0, 120, by = 20), limits = c(0, 120)) +
    coord_cartesian(ylim = c(0, y_lim)) +
    labs(x = "Cost (USD millions)", y = y_label) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.position = "none"
    )
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
# results_cases <- transform(results, output = "cases")
# results_hosp <- transform(results, output = "hosp")
# results_time <- transform(results, output = "time")
# for (i in seq_len(nrow(results))) {
#   detect <- detection_time(data[[i]])
#   results_cases[i, c("q25", "q50", "q75")] <- quantile(detect$cum_I, c(0.25, 0.5, 0.75))
#   results_hosp[i, c("q25", "q50", "q75")] <- quantile(detect$H, c(0.25, 0.5, 0.75))
#   results_time[i, c("q25", "q50", "q75")] <- quantile(detect$time, c(0.25, 0.5, 0.75))
# }

# results <- bind_rows(results_cases, results_hosp, results_time)
# save(results, file = "results.RData")

# # Example usage
# subset_data <- results %>% filter(output == "cases", t == 5, d == "SARS-CoV-2 Omicron")
# plot_cost(subset_data)

# data <- run_SEIR("SARS-CoV-2", rep = 1e1, threshold = 1)
# plot_SEIR(data)
