set.seed(1)
par(mfrow = c(1, 2))

plot(NULL,
    ylim = c(1 - 0.5, nsim + 0.5),
    xlim = c(0, 0.4),
    ylab = "Simulation Run",
    xlab = "Confidence Interval vs. Sim. rho",
    main = "Independent Observations (Sub. A)")

for (s in 1:10) {
  N <- 100
  rho <- 0.2

  X1 <- NULL
  X2 <- NULL
  for (repo in 1:100) {
    X1repo <- rnorm(N, mean = 0)
    X2repo <- rnorm(N, mean = 0)

    sigma <- matrix(c(1, rho, rho, 1), 2, 2)
    Xrepo <- cbind(X1repo, X2repo) %*% chol(sigma)

    X1 <- c(X1, Xrepo[, 1])
    X2 <- c(X2, Xrepo[, 2])
  }

  sample <- 1:(N * 9)

  r_sample <- cor(X1[sample], X2[sample], method = "pearson")
  r_all <- cor(X1, X2, method = "pearson")

  # https://shandou.medium.com/how-to-compute-confidence-interval-for-pearsons-r-a-brief-guide-951445b9cb2d
  n <- length(sample)
  z <- 0.5 * log((1 + r_sample) / (1 - r_sample))
  SE <- 1 / sqrt(n - 3)
  CI <- c(tanh(z - 1.96 * SE), tanh(z + 1.96 * SE))

  # Plot confidence interval based on repository mean.
  points(r_sample, s, pch = 16, col = "gray", cex = 1.4)
  arrows(CI[1], s, CI[2], s, length = 0.05, angle = 90, code = 3, col = "gray", lwd = 2)
  points(r_all, s, pch = 16, col = "black", cex = 1.4)

}

set.seed(1)

plot(NULL, ylim = c(1 - 0.5, nsim + 0.5), xlim = c(0, 0.4), ylab = "Simulation Run", xlab = "Confidence Interval vs. Sim. rho",
     main="Dependent Observations (Sub. B)")

for (s in 1:nsim) {
  N <- 100
  X1 <- NULL
  X2 <- NULL
  for (repo in 1:100) {
    rho <- rnorm(1, 0.2, sd = 0.23)
    rho <- max(-1, rho)
    rho <- min(1, rho)


    X1repo <- rnorm(N, mean = 0)
    X2repo <- rnorm(N, mean = 0)

    sigma <- matrix(c(1, rho, rho, 1), 2, 2)
    Xrepo <- cbind(X1repo, X2repo) %*% chol(sigma)

    X1 <- c(X1, Xrepo[, 1])
    X2 <- c(X2, Xrepo[, 2])
  }

  sample <- 1:(N * 9)

  r_sample <- cor(X1[sample], X2[sample], method = "pearson")
  r_all <- cor(X1, X2, method = "pearson")

  # https://shandou.medium.com/how-to-compute-confidence-interval-for-pearsons-r-a-brief-guide-951445b9cb2d
  n <- length(sample)
  z <- 0.5 * log((1 + r_sample) / (1 - r_sample))
  SE <- 1 / sqrt(n - 3)
  CI <- c(tanh(z - 1.96 * SE), tanh(z + 1.96 * SE))

  # Plot confidence interval based on repository mean.
  points(r_sample, s, pch = 16, col = "gray", cex = 1.4)
  arrows(CI[1], s, CI[2], s, length = 0.05, angle = 90, code = 3, col = "gray", lwd = 2)
  points(r_all, s, pch = 16, col = "black", cex = 1.4)
}
dev.off()