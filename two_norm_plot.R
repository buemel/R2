library(ggplot2)

# Define parameters
x <- seq(0, 15, length.out = 10000)
df <- data.frame(
  x = rep(x, 2),
  density = c(dnorm(x, mean = 5, sd = 1.5),
              dnorm(x, mean = 8, sd = 1.5)),
  group = factor(rep(c("N1", "N2"), each = length(x)))
)

# Plot
ggplot(df, aes(x = x, y = density, color = group)) +
  geom_line(size = 1) +
  labs(title = "Two Normal Distributions",
       x = "x", y = "Density") +
  theme_minimal()
