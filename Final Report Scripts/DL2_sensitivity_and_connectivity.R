# Sensitivity and Connectivity Functions

# DATA ###########################
# Enumerate Architectures
# This perfectly replicates the example 
# from Slides in Class

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(archr)

source("workshops/functions_sensitivity_and_connectivity.R")

## Use HW5 tradespace data (2000 architectures)
data <- read_csv("Homeworks/random_architectures_N2000_v3_with_metrics_042526.csv", show_col_types = FALSE)

## Decisions = columns A–R (first 18 columns in this CSV)
decisions <- names(data)[1:18]

## Metrics = columns V–AB (7 metrics). Use names (more robust than positions).
metrics <- c(
  "total_cost",
  "on_schedule_delivery_risk",
  "reliability",
  "travel_time_reduction",
  "prediction_accuracy",
  "response_time",
  "data_coverage"
)

metric_labels <- c(
  total_cost = "total_cost ($)",
  on_schedule_delivery_risk = "on_schedule_delivery_risk ($)",
  reliability = "reliability (%)",
  travel_time_reduction = "travel_time_reduction (%)",
  prediction_accuracy = "prediction_accuracy (%)",
  response_time = "response_time (seconds)",
  data_coverage = "data_coverage (%)"
)

## Robustness overrides (keep same API as sourced functions)
## - NA-safe means in me_ij
## - order-safe TRUE/FALSE pairing in connectivity_ij (no reliance on diff row order)
me_ij <- function(data, decision_i, value_i, decision_j, value_j, metric = "m1", notj = FALSE) {
  data1 <- data %>%
    select(any_of(c(di = decision_i, dj = decision_j, m = metric)))

  if (notj) {
    data1 <- data1 %>% filter(dj != value_j)
  } else {
    data1 <- data1 %>% filter(dj == value_j)
  }

  output <- data1 %>%
    mutate(di = di == value_i) %>%
    summarize(
      xhat = mean(m[di == TRUE], na.rm = TRUE),
      x = mean(m[di == FALSE], na.rm = TRUE),
      diff = xhat - x
    ) %>%
    with(diff)

  return(output)
}

connectivity_ij <- function(data, decision_i, decision_j = "d2", metric = "m1") {
  data1 <- data %>%
    select(any_of(c(dj = decision_j))) %>%
    distinct() %>%
    expand_grid(notj = c(TRUE, FALSE)) %>%
    group_by(dj, notj) %>%
    summarize(
      stat = sensitivity_ij(
        data,
        decision_i = decision_i,
        decision_j = decision_j,
        value_j = dj,
        metric = metric,
        notj = notj
      ),
      .groups = "drop"
    )

  output <- data1 %>%
    group_by(dj) %>%
    summarize(
      stat = abs(stat[notj == TRUE] - stat[notj == FALSE]),
      .groups = "drop"
    ) %>%
    summarize(stat = mean(stat, na.rm = TRUE)) %>%
    with(stat)

  return(output)
}

# me(data, decision = "d2", value = 1, metric = "m1")
# 
# me_ij(data, 
#       decision_i = "d3", value_i = 1, 
#       decision_j = "d2", value_j = 1, 
#       notj = FALSE, metric = "m1")
# 
# sensitivity_ij(data, decision_i = "d3", decision_j = "d2", value_j = 1, metric = "m1", notj = FALSE)
# sensitivity_ij(data, decision_i = "d3", decision_j = "d2", value_j = 2, metric = "m1", notj = FALSE)
# sensitivity_ij(data, decision_i = "d3", decision_j = "d2", value_j = 1, metric = "m1", notj = TRUE)
# sensitivity_ij(data, decision_i = "d3", decision_j = "d2", value_j = 2, metric = "m1", notj = TRUE)
# 
# sensitivity_ij(data, decision_i = "d3", decision_j = "d2", 
#                value_j = 1, metric = "m1", notj = FALSE)
# sensitivity_ij(data, decision_i = "d3", decision_j = "d2", 
#                value_j = 1, metric = "m1", notj = TRUE)
# sensitivity_ij(data, decision_i = "d3", decision_j = "d2", 
#                value_j = 2, metric = "m1", notj = FALSE)
# sensitivity_ij(data, decision_i = "d3", decision_j = "d2", 
#                value_j = 2, metric = "m1", notj = TRUE)
# sensitivity_ij(data, decision_i = "d3", decision_j = "d2", 
#                value_j = 3, metric = "m1", notj = FALSE)
# sensitivity_ij(data, decision_i = "d3", decision_j = "d2", 
#                value_j = 3, metric = "m1", notj = TRUE)
# 
# 
# connectivity_ij(data, decision_i = "d3", decision_j = "d2", metric = "m1" )
# connectivity_ij(data, decision_i = "d3", decision_j = "d1", metric = "m1" )
# 
# 
# connectivity(data, decision_i = "d1", decisions = c("d1","d2","d3"), metric = "m1")
# connectivity(data, decision_i = "d2", decisions = c("d1","d2","d3"), metric = "m1")
# connectivity(data, decision_i = "d3", decisions = c("d1","d2","d3"), metric = "m1")
# connectivity(data, decision_i = "d1", decisions = c("d1","d2","d3"), metric = "m2")
# connectivity(data, decision_i = "d2", decisions = c("d1","d2","d3"), metric = "m2")
# connectivity(data, decision_i = "d3", decisions = c("d1","d2","d3"), metric = "m2")


# Compute Decision x Metric table ###################################
points = expand_grid(
  decision = decisions,
  metric = metrics
) %>%
  group_by(decision, metric) %>%
  #rowwise() %>% 
  summarize(
    c = connectivity(data = data, decision_i = decision, decisions = decisions, metric = metric),
    s = sensitivity(data = data, decision_i = decision, metric = metric),
    .groups = "drop"
  )

## Scale prediction_accuracy to % for consistency
points <- points %>%
  mutate(
    c = if_else(metric == "prediction_accuracy", c * 100, c),
    s = if_else(metric == "prediction_accuracy", s * 100, s)
  )

results <- points %>%
  transmute(
    Decision = decision,
    Metric = metric,
    Connectivity = c,
    Sensitivity = s
  )

## Sort + metric order (match input file order)
results <- results %>%
  mutate(
    Metric = factor(Metric, levels = metrics),
    Decision = factor(Decision, levels = decisions)
  ) %>%
  arrange(Decision, Metric) %>%
  mutate(
    ## Formatting requirements by metric
    Connectivity = case_when(
      as.character(Metric) %in% c("total_cost", "on_schedule_delivery_risk") ~ as.integer(round(Connectivity, 0)),
      as.character(Metric) == "prediction_accuracy" ~ round(Connectivity, 2),
      TRUE ~ round(Connectivity, 2)
    ),
    Sensitivity = case_when(
      as.character(Metric) %in% c("total_cost", "on_schedule_delivery_risk") ~ as.integer(round(Sensitivity, 0)),
      as.character(Metric) == "prediction_accuracy" ~ round(Sensitivity, 2),
      TRUE ~ round(Sensitivity, 2)
    ),
    Metric = metric_labels[as.character(Metric)],
    Decision = as.character(Decision)
  )

results

write_csv(results, "Homeworks/HW5_sensitivity_connectivity_results.csv")

## Simon's 4-quadrant decision view chart (midpoint-of-range cut lines per metric)
# Regenerate plot from saved results CSV (as requested)
plot_points <- read_csv("Homeworks/HW5_sensitivity_connectivity_results.csv", show_col_types = FALSE) %>%
  rename(
    decision = Decision,
    metric = Metric,
    c = Connectivity,
    s = Sensitivity
  ) %>%
  filter(is.finite(c), is.finite(s))

# Preserve metric order as it appears in the output file
metric_order <- unique(read_csv("Homeworks/HW5_sensitivity_connectivity_results.csv", show_col_types = FALSE)$Metric)
plot_points <- plot_points %>%
  mutate(
    metric = factor(metric, levels = metric_order),
    decision = factor(decision, levels = decisions)
  )

cuts <- plot_points %>%
  group_by(metric) %>%
  summarize(
    ## "Middle of the frame" for each facet: midpoint of observed min/max
    c_cut = (min(c, na.rm = TRUE) + max(c, na.rm = TRUE)) / 2,
    s_cut = (min(s, na.rm = TRUE) + max(s, na.rm = TRUE)) / 2,
    .groups = "drop"
  )

simmon_plot <- ggplot(plot_points, aes(x = c, y = s, color = decision)) +
  geom_vline(data = cuts, aes(xintercept = c_cut), color = "grey35") +
  geom_hline(data = cuts, aes(yintercept = s_cut), color = "grey35") +
  geom_point(size = 3, alpha = 0.9) +
  facet_wrap(~metric, scales = "free") +
  labs(
    x = expression(bold(connectivity)),
    y = expression(bold(sensitivity)),
    color = "decision"
  ) +
  theme_minimal() +
  theme(
    ## Bold-ish border around the entire figure (outside facets)
    plot.background = element_rect(color = "black", linewidth = 1.2, fill = NA),
    plot.margin = margin(8, 8, 8, 8)
  )

simmon_plot

ggsave("Homeworks/HW5_simmon_4quadrant.png", simmon_plot, width = 12, height = 7, dpi = 300)
ggsave("Homeworks/HW5_simmon_4quadrant.pdf", simmon_plot, width = 12, height = 7)



