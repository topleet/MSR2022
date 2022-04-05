data <- read.csv("metrics_elasticsearch.csv")

X <- log(data$linesChanged + 1)
N <- length(X)

results <- NULL

for (seed in 0:100) {
    set.seed(seed)

    alpha <- -3.0
    beta <- 0.4
    prob <- 1 / (1 + exp(-(alpha + beta * X)))

    Y <- rbinom(N, size = 1, prob = prob)

    model <- glm(Y ~ X, family = binomial())

    results <- rbind(results, coef(model))
}

par(mfrow = c(1, 2))
hist(results[, 1], breaks = 20, main = "Identified Intercepts (alpha)", xlab = "")
hist(results[, 2], breaks = 20, main = "Identified Slopes (beta)", xlab = "")