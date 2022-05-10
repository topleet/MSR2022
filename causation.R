# Initializing plot.
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))

# Set seed to reproduce experiment
set.seed(1)
# Used to store resuls.
result.lr <- NULL
result.ao <- NULL

# SIMULATION CODE:

# The number of observations.
N <- 1000

# We fix alpha.
alpha <- -3.0

# Going through 30 different values for effect strength.
for (beta in seq(-1, 1, length.out = 30)) {
  # Synthetic random variable X.
  X <- rnorm(N)

  # Producing two alternative outcomes.
  prob_alt1 <- 1 / (1 + exp(-(alpha + beta * X)))
  prob_alt2 <- 1 / (1 + exp(-(alpha + beta * (X + 1))))

  # Corresponding alternative defects.
  Y_alt1 <- rbinom(N, size = 1, prob = prob_alt1)
  Y_alt2 <- rbinom(N, size = 1, prob = prob_alt2)

  # ANALYSIS METHODOLOGY:
  # We use a regular logistic regression applied to
  # one of the alternative outputs.
  model <- glm(Y_alt1 ~ X, family = binomial())

  # We store the difference between the alternative
  # outputs and the coefficient of the model.
  result.ao <- c(result.ao, mean(Y_alt2 - Y_alt1))
  result.lr <- c(result.lr, coef(model)["X"])
}
# This plot show that there is a clear correspondence.
plot(result.ao,
     result.lr,
     xlab = "Difference Potential Outcomes",
     ylab = "Identified Beta by Logistic regression",
     pch = 16)