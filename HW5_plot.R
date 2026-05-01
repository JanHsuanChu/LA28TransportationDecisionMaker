library(ggplot2)

print(
ggplot() +
  geom_point(
    data = pareto_true,
    aes(x = total_cost, y = travel_time_reduction),
    color = "blue",
    size = 3,
    alpha = 0.7
  ) +
  
  geom_point(
    data = pareto_ga,
    aes(x = total_cost, y = travel_time_reduction),
    color = "red",
    size = 4
  ) +

  
  labs(
    title = "Pareto Front Comparison",
    x = "Total Cost ($)",
    y = "Travel Time Reduction (%)"
  ) +
  
  theme_minimal()
)