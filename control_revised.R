library(ggplot2)

# Revising the study using the original data.
data <- read.csv("tufano.csv")
data <- data[!is.na(data$bug), ]
# This data can be downloaded as additional material from the
# original study done by Tufano et al.

# Basic data transformations.
data$bug <- as.numeric(as.logical(data$bug))
data$size_lines <- log(1 + data$size_lines)

# The models revising the original study.
original <- glm(bug ~ exp_ccdbm, data = data, family = binomial())
control <- glm(bug ~ exp_ccdbm + size_lines, data = data, family = binomial())

# Model comparison (also see original$aic and control$aic).
anova(original, control)

# Cross-validation
folds <- 50
results <- NULL
for (i in 1:folds) {
  train <- data[seq_len(nrow(data)) %% folds != (i - 1),]
  test <- data[seq_len(nrow(data)) %% folds != (i - 1),]

  m1 <- glm(bug ~ exp_ccdbm, data = train, family = binomial())
  m2 <- glm(bug ~ size_lines, data = train, family = binomial())
  m3 <- glm(bug ~ exp_ccdbm + size_lines, data = train, family = binomial())

  error1 <- sum((test$bug - predict(m1, newdata =  test, type = "response"))^2)
  error2 <- sum((test$bug - predict(m2, newdata =  test, type = "response"))^2)
  error3 <- sum((test$bug - predict(m3, newdata =  test, type = "response"))^2)

  results <- rbind(results, data.frame(predictors = "Experience", `SSE` = error1))
  results <- rbind(results, data.frame(predictors = "File size", `SSE` = error2))
  results <- rbind(results, data.frame(predictors = "File size + Experience", `SSE` = error3))

}
# Plot the results of the cross validation.
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))
ggplot(results, aes(x = `SSE`, y = predictors)) +
  geom_boxplot(fill="gray") +
  xlab("Sum of Square Errors (SSE)") +
  ylab("") +
  theme_linedraw(base_size = 16) +
  theme(panel.grid.major = element_blank())
