source("Stochastic.r", local = TRUE)
load("results.RData")
# load("full_data.RData")

# now generate stochastic data for all diseases
data <- list()
for (i in Disease_names) {
    data[[i]] <- run_SEIR(i, rep = 0)
}

# combine into one df with a column for disease name
data2 <- bind_rows(data, .id = "disease")

ggplot(data2, aes(x = time, y = cum_I)) +
    geom_line(aes(color = disease), linewidth = 2) +
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
    # scale_color_manual(values = c("blue", "grey80")) +
    theme(
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.position = "bottom",
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 20)
    )

ggsave("figs/progression.png", width = 10, height = 10)

# make a subset of results that is only Sars-Cov-2, lag = 7, and p = 0.01
results_subset <- results %>%
    filter(d == "SARS-CoV-2", lag == 7, output_cases == FALSE)

ggplot(results_subset, aes(x = cost_mil_annu, group = t)) +
    geom_line(aes(y = q50, color = factor(t)), linewidth = 5) +
    scale_x_continuous(
      breaks = seq(0, 120, by = 20),
      minor_breaks = seq(0, 120, by = 10), limits = c(0, 120)
    ) +
    labs(x = "Cost (USD millions)", color = "threshold",
        y = "Days until detection") +
    scale_y_continuous(
        breaks = seq(0, 100, by = 20),
        minor_breaks = seq(0, 100, by = 10)
    ) +
    coord_cartesian(ylim = c(0, 100)) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20),
      legend.position = "bottom",
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 20)
    )

ggsave("figs/threshold2.png", width = 10, height = 10)
