# Number of simulation runs.
nsim <- 40

# Different degree of dependency, we want to check.
# Dependency is given in terms of a standard deviation
# describing the repository-specific variation.
dependencies <- c(0.0, 0.1, 0.21)

# Produce a plot with multiple areas.
par(mfrow = c(1, length(dependencies)))

# Switching between different levels of dependency.
for (dependency in dependencies) {
  # Create a plot that shows the confidence interval vs. the real correlation.
  plot(NULL,
      ylim = c(1 - 0.5, nsim + 0.5),
      xlim = c(0, 0.4),
      ylab = "Simulation Run",
      xlab = "Confidence Interval vs. Sim. rho",
      main = paste0("Dependency = ", dependency))

  # Set a seed to reproduce the exact numbers as in the paper.
  set.seed(1)

  # We do nsim simulation runs.
  for (s in 1:nsim) {

    # SIMULATION CODE:
    X1all <- NULL # X1 collected over repositories.
    X2all <- NULL # X2 collected over repositories.
    N <- 100 # Number of repositories.

    for (repo in 1:N) {
      # The difference between substitution A and B.
      # If dependency = 0, rho is always the same.
      # If dependency > 0, rho is sightly different for each repository.
      rho <- rnorm(1, 0.2, sd = dependency)
      # Note: using the rnorm is over simplistic.
      # If rho is outside [-1,1], the simulation breaks.

      M <- 100 # Number of commits in each repository.

      # Simulating X1 and X2 for a repository.
      X1 <- rnorm(M, mean = 0, sd = 1)
      X2 <- rnorm(M, mean = 0, sd = 1)

      # Producing the correlation using rho.
      # First, we construct a covariance matrix using the correlation
      # and variance keeps 1.
      sigma <- matrix(c(1, rho, rho, 1), 2, 2)
      # We use the Cholesky decomposition to simulate correlation.
      X <- cbind(X1, X2) %*% chol(sigma)

      # Collecting X1 and X2 by appending it to X1all and X2all.
      X1all <- c(X1all, X[, 1])
      X2all <- c(X2all, X[, 2])
    }

    # ANALYSIS METHODOLOGY:

    # We take all commits, part of the first 9 repositories (900 if N = 100),
    # as our random sample. The rest of the commits is kept hidden.
    sample <- 1:(N * 9)

    # We compute the Pearson correlation on the sample and on all commits.
    # The latter cannot be done in practice, as it is cost intensive.
    r_sample <- cor(X1all[sample], X2all[sample], method = "pearson")
    r_all <- cor(X1all, X2all, method = "pearson")

    # We now derive the confidence interval without protection against
    # dependent observations on the sampled commits.
    n <- length(sample)
    z <- 0.5 * log((1 + r_sample) / (1 - r_sample))
    SE <- 1 / sqrt(n - 3)
    CI <- c(tanh(z - 1.96 * SE), tanh(z + 1.96 * SE))

    # Plot a confidence interval, the sample correlation,
    # and the correlation computed on all commits.
    points(r_sample, s, pch = 16, col = "gray", cex = 1.4)
    arrows(CI[1], s, CI[2], s, length = 0.05, angle = 90, code = 3, col = "gray", lwd = 2)
    points(r_all, s, pch = 16, col = "black", cex = 1.4)
  }
}
