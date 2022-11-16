# Loading the original data from our paper.
data <- read.csv("distribution.csv")

# Applying a model assuming a normal-distributed output.
normal_model <- lm(is_sparql ~ scale(log(created_days_ago)) +
  scale(log(stargazers_count + 0.5)) +
  scale(log(active_developers + 0.5)) +
  scale(log(active_files + 0.5)), data = data)

# Applying a model assuming a binomial-distributed output.
binomial_model <- glm(is_sparql ~ scale(log(created_days_ago)) +
  scale(log(stargazers_count + 0.5)) +
  scale(log(active_developers + 0.5)) +
  scale(log(active_files + 0.5)),
                      family = binomial(), data = data)

# Produce a table comparing the identified parameters for both model (and the scaled variants).
round(cbind(
  coef(normal_model)[2:5],
  coef(binomial_model)[2:5],
  coef(normal_model)[2:5] / sd(coef(normal_model)[2:5]),
  coef(binomial_model)[2:5] / sd(coef(binomial_model)[2:5])),3)


# Creating an empty plot for the simulations.
visx <- -1

par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))

plot(NULL, xlim = c(0.2, 32.7), ylim = c(-3, 3),xaxt='n',
     ylab = "Identified and Simulated Betas (Scaled)", xlab = "M Variables in 6 Simulation Runs")

N <- 120 # Number of repositories.
M <- 4 # Number of variables to examine.

set.seed(1)

for (sim in 1:6) {
  polygon(c(visx, visx + M + 1, visx + M + 1, visx),
          c(-100, -100, 100, 100),
          col = rgb(0,0,0,0.1), border = NA)

  Xs <- matrix(rnorm(N * M), nrow = N, ncol = M) # Producing a N * M matrix of random normally distributed values.

  # Producing a vector of M + 1 random betas, including one variable for a random intercept.
  betas <- rnorm(M + 1) # Random betas.

  # Adding a column of `ones' at the left of the matrix, later multiplied with the first beta and serves as intercept.
  Xs <- cbind(1, Xs)

  # Matrix multiplication ('%*%') of Xs and betas, then applying the logistic function.
  prob <- 1 / (1 + exp(-(Xs %*% betas))) # The probability deciding for one of the two languages.

  # Producing a decision (same as in the previous simulations).
  Y <- rbinom(N, size = 1, prob = prob)

  # Apply both models considering all variables (except the ones).
  Xs <- Xs[, 2:(M + 1)]

  normal_model <- lm(Y ~ Xs)
  binomial_model <- glm(Y ~ Xs, family = binomial())

  binomial_coef <- coef(binomial_model)[2:(M + 1)]
  normal_coef <- coef(normal_model)[2:(M + 1)]
  betas <- betas[2:(M + 1)]

  binomial_coef <- binomial_coef / sd(binomial_coef)
  normal_coef <- normal_coef / sd(normal_coef)
  betas <- betas / sd(betas)

  # Plot the difference.
  for (i in 1:M) {
    visx <- visx + 1

    segments(visx, betas[i], visx, normal_coef[i], lty =3)
    segments(visx, betas[i], visx, binomial_coef[i], lty =3)

    points(visx, betas[i], pch = 4)

    points(visx, binomial_coef[i], pch = 16)
    points(visx, normal_coef[i])
  }

  visx <- visx + 2
  abline(h = 0)
  text(visx - M, 2.8, paste("Sim.", sim))
}

legend(17, -1.7, legend=c("Identified  using a Binomial Out. Dist.",
                        "Identified using a Normal Out. Dist.",
                        "Simulated"), pch=c(16, 1,4), cex=1.1)