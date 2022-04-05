set.seed(1)
data <- read.csv("metrics_elasticsearch.csv")
X <- log(data$linesChanged + 1)
N <- length(X)

alpha <- -3.0
beta <- 0.4
prob <- 1 / (1 + exp(- (alpha + beta * X)))

set.seed(1)

Y <- rbinom(N, size = 1, prob = prob)

model <- glm(Y ~ X, family = binomial())

print(coef(model))