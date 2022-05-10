library(lsa) # Needed library for the cosine similarity.
fails <- 0 # Fails of the original method (Wilcox test)
failsB <- 0 # Fails of the alternative method (lr with control).

# We repeat this simulation 100 times to record
# how often a method fails.
for (i in 1:100) {
  # SIMULATION CODE:
  # The number of observations.
  N <- 8000

  # We fix alpha and beta for X.
  alpha <- -3.0
  betaX <- 0.4

  # Collecting variable X and E over
  # all simulated observations.
  X <- NULL
  E <- NULL

  # Simulating a commit's metric computation.
  for (n in 1:N) {
    # The number of terms for the vectors.
    nTerms <- 200

    # Generate two random term vectors.
    # Generate a developer's background knowledge.
    back <- rpois(n = nTerms, lambda = 5.0)
    # Generate the commits term vector.
    file <- rpois(n = nTerms, lambda = 0.1)

    # Compute the similarity defining the novel experience metric.
    E <- c(E, cosine(back, file))
    # Size of the file (already log tranformed).
    X <- c(X, log(sum(file) + 1))
  }

  # Using the assumption of logistic regression
  # to produce the probability of defects, not
  # influenced by E.
  prob <- 1 / (1 + exp(-(alpha + betaX * X)))

  # Simulate final defects Y.
  Y <- rbinom(N, size = 1, prob = prob)

  # ANALYSIS METHODOLOGY:
  # Use the original methodology.
  fails <- sum(fails, wilcox.test(E ~ Y)$p.value < 0.05)

  # Use an adjusted methodology with control.
  failsB <- sum(failsB, coef(summary(glm(Y ~ E + X, family = binomial)))[2, 4] < 0.05)
}

# Print how often a methodology fails.
print(paste0("The original methodology detects and effect in ",fails, "of 100 cases." ))
print(paste0("The adjusted methodology detects and effect in ",failsB, "of 100 cases." ))
