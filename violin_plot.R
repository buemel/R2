library(ggplot2)

# Example data
set.seed(123)
df <- data.frame(
  group = rep(c("A", "B"), each = 100),
  value = c(rnorm(100, 10, 2), rnorm(100, 15, 3))
)

# Plot
ggplot(df, aes(x = group, y = value)) +
  geom_violin(fill = "lightblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "red") +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(title = "Violin Plot with 95% CI and Boxplot Overlay")
