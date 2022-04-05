data <- read.csv("metrics_elasticsearch.csv")

X <- log(data$linesChanged + 1)
Y <- as.integer(data$buggy == "true")

model <- glm(Y ~ X, family = binomial())

print(coef(model))
