results <- NULL

par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))

plot(NULL, xlim = c(-15, 6), ylim = c(0, 11),
     xlab = "Confidence Interval vs. Sim. Effect of the Treatment (i.e., -4)",
     ylab = "Simulation Run")
abline(v = -4)
abline(v = 0, lty = 3)

for (i in 1:10000) {
  set.seed(i)
  N <- 20 # Number of subjects.
  S <- rnorm(N, mean = 60, sd = 5) # Unobserved preconditions of subjects.

  # Randomly assigning treatment and control.
  G <- sample(c("treatment", "control"), N, replace = T)

  # The effect of our treatment.
  X <- ifelse(G == "treatment", -4, 0)

  # Composing the time that a subject actually needs.
  Y <- S + X

  # How we deal with the uncertainty.
  CI <- -t.test(Y ~ G)$conf.int
  YbyG <- tapply(Y, G, mean)

  result <- (YbyG[2] - YbyG[1])[[1]]
  results <- c(results, YbyG[2] - YbyG[1])

  # Only plot the confidence interval for first ten simulations.
  if (i < 11) {
    points(result, i, pch = 16, col = "gray", cex = 1.4)

    arrows(CI[1], i, CI[2], i, length = 0.05, angle = 90, code = 3, col = "gray", lwd = 2)
    points(-4, i, pch = 16, cex = 1.4)
  }
}

par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))

hist(results, breaks = 20,
     xlab = "Difference Between Means of Treatment and Control Group",
     yaxt='n',
     main = "")