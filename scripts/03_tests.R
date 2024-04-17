# Load necessary libraries
library(ggplot2)

# Plotting the distribution of transformed income
p1a <- ggplot(data, aes(x = tran_inc_normal)) +
  geom_histogram(aes(y = ..density..), color = "gray", fill = "gray", bins = 30) +
  labs(title = "Distributions of Transformed Income", x = "Inverse Hyperbolic Sin of Income (in Rs.)", y = "Density")
ggsave("/cloud/project/outputs/figures/income_distribution.png", plot = p1a, width = 10, height = 8, dpi = 300)

# Figure for comparing two periods of income
p_comparison <- ggplot(data, aes(x = tran_inc_normal)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_histogram(data = data, aes(x = tran_inc_current, y = ..density..), bins = 30, fill = "red", alpha = 0.5) +
  labs(title = "Comparison of Income During Normal and Current Periods")
ggsave("/cloud/project/outputs/figures/income_period_comparison.png", plot = p_comparison, width = 10, height = 8, dpi = 300)
