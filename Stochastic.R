# First, ensure you've installed the necessary packages
library(adaptivetau)
library(future)
library(future.apply)
library(tidyverse)

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

# Run stochastic SEIR for a given disease multiple times using parallelization
run_SEIR <- function(disease_name, rep=100, p=0.01, threshold = 1,
  initial_state = c(S = 6500000, E = 0, I = 1, R = 0, D = 0), time = 100) {
  params <- Disease_Cases[[which(Disease_names == disease_name)]]$params
  params["p"] <- p
  params["threshold"] <- threshold
  plan(multisession)
  results <- pivot_longer(bind_rows(future_lapply(1:rep, function(i) {
    out <- ssa.adaptivetau(initial_state, transitions, SEIRrates, params,
      tf = time, halting = 5)
    tibble_data <- as_tibble(out$dynamics) %>%
      mutate(rep = i, cum_I = I + R, halted = out$haltingTransition) # add replication number
    return(tibble_data)
  }, future.seed = TRUE)), cols = -c(time, rep, halted), names_to = "variable")
}

# plot results
plot_SEIR <- function(data) {
  last <- data %>%
    group_by(rep) %>%
    slice_tail(n = 1)
  ggplot(data, aes(x = time, y = value, group = interaction(variable, rep)
  # , color = factor(variable, levels = c("S", "E", "I", "R", "cum_I"))
  )) +
    geom_line(linewidth = 1.5) +
    geom_point(data = last, aes(x = time, y = value), size = 8, color = "red") +
    labs(
      x = "Days",
      y = "Cumulative infections",
      color = "Compartment"
    ) +
    scale_color_brewer(palette = "Set1") +
    theme_light() +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 35),
      axis.text = element_text(size = 25),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20),
      legend.key.size = unit(1.5, "cm")
    )
}

# Example usage:
# data <- run_SEIR("SARS-CoV-2", rep = rep)
# plot_SEIR(data[data$variable == "cum_I" & !is.na(data$halted), ])