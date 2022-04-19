data <- read.csv("metrics_elasticsearch.csv")

X <- log(data$linesChanged + 1)
N <- length(X)

results <- NULL

for (alpha in seq(-10, 10, length.out = 30)) {
  for (beta in seq(-1, 1, length.out = 30)) {

    prob <- 1 / (1 + exp(- (alpha + beta * X)))

    Y <- rbinom(N, size = 1, prob = prob)

    model <- glm(Y ~ X, family = binomial())

    ndefects <- sum(Y)
    results <- rbind(results, c(coef(model), alpha, beta, ndefects))
  }
}

error <- pmin(abs(results[, 2] - results[, 4]), 1)

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(results[, 3], results[, 4],
    col = rgb(1, 0, 0, error),
    pch = 16,
    xlab = "alpha",
    ylab = "beta",
    main = "")

plot(log(results[, 5]), error, 
    xlab = "number of defects (log)")