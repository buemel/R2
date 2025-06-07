library(ggplot2)
library(glue)

# Parameters
n <- 100
x <- 0:n
p1 = 0.3
p2 = 0.7

# Create data frame
df <- data.frame(
  x = rep(x, 2),
  prob = c(dbinom(x, size = n, prob = p1),
           dbinom(x, size = n, prob = p2)),
  group = factor(rep(c(glue("Binom({n}, {p1})"), glue("Binom({n}, {p2})")), each = length(x)))
  #group = factor(rep(c(glue("Binom({n}, {formatC(p1, format = 'f', digits = 2)})"), glue("Binom({n}, {formatC(p2, format = 'f', digits = 2)})")), each = length(x)))
)

# Plot
ggplot(df, aes(x = x, y = prob, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Two Binomial Distributions", x = "x", y = "Probability") +
  theme_minimal()
