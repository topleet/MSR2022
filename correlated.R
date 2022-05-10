set.seed(1)

coefZWX <- NULL
coefZX <- NULL
coefWX <- NULL
coefX <- NULL
correlation <- NULL

# Run simulations.
for (sdZ in seq(0, 1, length.out = 40)) {
  N <- 5000

  # Configure correlation between Z and W by setting the sdZ randomly between 0 and 1.

  # Simulating the relationships between W, X, Z and Y
  W <- rnorm(N, mean = 0, sd = 1)
  X <- rnorm(N, mean = -W, sd = 1)
  Z <- rnorm(N, mean = W, sd = sdZ)

  prob <- 1 / (1 + exp(-(W + X)))

  # Substituted observed variable Y.
  Y <- rbinom(N, size = 1, prob = prob)

  # Regular model and mitigated mode.
  modelZWX <- glm(Y ~ Z + W + X, family = binomial())
  modelZX <- glm(Y ~ Z + X, family = binomial())
  modelWX <- glm(Y ~ W + X, family = binomial())
  modelX <- glm(Y ~ X, family = binomial())

  # Computing and appending the correlation between W and Z.
  correlation <- c(correlation, cor(W, Z))
  coefZWX <- rbind(coefZWX, coef(modelZWX))
  coefZX <- rbind(coefZX, coef(modelZX))
  coefWX <- rbind(coefWX, coef(modelWX))
  coefX <- rbind(coefX, coef(modelX))
}

coefZWX <- as.data.frame(coefZWX)
coefZX <- as.data.frame(coefZX)
coefWX <- as.data.frame(coefWX)
coefX <- as.data.frame(coefX)
cairo_pdf("generated/correlated_b.pdf", width = 6, height = 6)
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))

plot(NULL, xlim = c(0.75, 1), ylim = c(0.2, 1.2), ylab = "Identified Effect X", xlab = "Correlation")

abline(h = 1)

points(correlation, coefZWX$X, pch = 16, col = "red")
points(correlation, coefX$X, pch = 6)
points(correlation, coefZX$X, pch = 0)
points(correlation, coefWX$X, pch = 3)

text(0.75, 0.35, "Model using X", pos = 4)
text(0.75, 0.6, "Model using X, Z", pos = 4)
text(0.75, 1.1, "Models using X, W (and Z)", pos = 4)

dev.off()

cairo_pdf("generated/correlated_a.pdf", width = 6, height = 6)
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))
plot(NULL, xlim = c(0.75, 1), ylim = c(-0.5, 2.2),ylab = "Identified Effect W or Z", xlab= "Correlation")
points(correlation, coefZWX$W, pch = 16)
points(correlation, coefZWX$Z, pch = 4)
abline(h = 1)
abline(h = 0)
text(0.75, 1.25, "Identified effect of W (dot)", pos = 4)
text(0.75, 0.25, "Identified effect of Z (cross)", pos = 4)
dev.off()