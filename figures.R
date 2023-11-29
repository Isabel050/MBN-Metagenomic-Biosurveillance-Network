library(gt)
source("Stochastic.r", local = TRUE)
load("results.RData")
# load("full_data.RData")

# now generate stochastic data for all diseases
data <- list()
for (i in row.names(all_params)) {
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
    )  +
    guides(color = guide_legend(nrow = 3, byrow = TRUE, override.aes = list(size = 5)))

ggsave("outputs/progression.jpg", width = 12, height = 12)

# make a subset of results that is only Sars-Cov-2, and p = 0.01
results_threshold <- results %>%
    filter(d == "SARS-CoV-2", output == "time")

ggplot(results_threshold, aes(x = cost_mil_annu, group = t)) +
    geom_line(aes(y = q50, color = factor(t)), linewidth = 2) +
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

ggsave("outputs/threshold.jpg", width = 12, height = 12)

results_days <- results %>%
    filter(d == "SARS-CoV-2", t == 1, output == "time")
plot_cost(results_days)
ggsave("outputs/cost_days.jpg", width = 12, height = 12)

results_cases <- results %>%
    filter(d == "SARS-CoV-2", t == 1, output == "cases")
plot_cost(results_cases)
ggsave("outputs/cost_cases.jpg", width = 12, height = 12)

results_hosp <- results %>%
    filter(d == "SARS-CoV-2", t == 1, output == "hosp")
plot_cost(results_hosp)
ggsave("outputs/cost_hosp.jpg", width = 12, height = 12)


### Table 1
table1 <- results %>%
    filter(d == "SARS-CoV-2", h %in% c(1, 6, 10, 16, 26)) %>%
    select(c(t, h, cost_mil_annu, output, q50)) %>%
    # Calculate coverage
    rowwise() %>%
    mutate(coverage = coverage(Hospital_visitors[1:h, "Hospital"])) %>%
    # round to nearest integer
    mutate(q50 = round(q50), cost_mil_annu = round(cost_mil_annu)) %>%
    # format coverage as a percent
    mutate(coverage = paste0(round(coverage * 100), "%")) %>%
    # rename output_cases to "output" and TRUE to "cases" and FALSE to "days"
    rename("cost (Mil $)" = cost_mil_annu, hospitals = h) %>%
    # pivot t and output_cases to columns
    pivot_wider(names_from = c(t, output), values_from = q50)

table1

gt1 <- gt(table1) %>%
  tab_spanner(label = "threshold 1", columns = starts_with("1")) %>%
  tab_spanner(label = "threshold 3", columns = starts_with("3")) %>%
  tab_spanner(label = "threshold 5", columns = starts_with("5"))

gt1

### Table 2
table2 <- table1 %>%
    select(hospitals, coverage)

table2$implementation <- Cost(table2$hospitals, years = 0) / 1e6
table2$operating <- Cost(table2$hospitals, years = 1, d = 0) / 1e6 - table2$implementation
table2$ten_year <- Cost(table2$hospitals, years = 10) / 1e6
table2$annualised <- table2$ten_year / 10

table2

# save tables as csv files
write.csv(table1, "outputs/epidemiology_results.csv", row.names = FALSE)
write.csv(table2, "outputs/costs_results.csv", row.names = FALSE)
