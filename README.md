# Operationalizing Threats to MSR Studies by Simulation-Based Testing (MSR2022)

This repository provides the supplementary material for the
MSR paper on simulation-based testing. We will structure the
presentation consistent with the sequential order of the
paper. The following source code is also provided in separate
R files to easy running.

## Original Methodology in a Nutshell (Paper section 3.1, [source code](nutshell_a.R))
The following R code is typical for an MSR/ESE data analysis
methodology, using logistic regression, but highly simplified. The first part of the code loads the data.
```R
data <- read.csv("metrics_elasticsearch.csv")

X <- log(data$linesChanged + 1)
Y <- as.integer(data$buggy == "true")
```
We borrow the data set from [Falcão et al.](https://github.com/filipefalcaos/saner-2020) We only use the amount of changed lines by a commit, which is stated-log transformed, and whether the commit is buggy or not. We store both observed variables as X and Y.

The next part of the code fits the logistic regression model.
```R
model <- glm(Y ~ X, family = binomial())
```
We use R's build in generalized linear model (glm), specify the relation of interest in terms of an R formula (Y ~ X), and set the output distribution to be binomial.

Next, we inspect how the model fitted the intercept and slope (later called alpha and beta). We ignore confidence estimates in this part of the presentation.
```R
print(coef(model))
```
The coefficients are the same as the rounded number shown in
the paper. They are typically shown in tabular form. The X refers to the corresponding slope.

| (Intercept)   |  X            |
| ------------- |:-------------:|
| -3.2861068    | 0.4543962     |

## Simulation as a Substitution for Reality (Paper section 3.3, [source code](nutshell_b.R))
We will now implement a basic simulation-based 
test, substituting some observed and unobserved
variables with synthetic variables.

We again load the data and do some preparations.
```R
data <- read.csv("metrics_elasticsearch.csv")
X <- log(data$linesChanged + 1)
N <- length(X)
```
We load the data, extract the stated-log transformed changed lines (X), and the number of observations (N) that we have in this data set. However, this time we do not need the final defect (Y) as we will replace it by a synthetic variable.

The following part is critical, as it does the substitution of
observed and unobserved variables with synthetic variables.
```R
alpha <- -3.0
beta <- 0.4
prob <- 1 / (1 + exp(- (alpha + beta * X)))
```
We first set alpha and beta to some invented values (we 
recommend the reader to try alternatives). Next we produce
the corresponding probability of producing defects as a function of alpha, beta and X. The code is vectorized. Since X is a vector, the results of the above arithmetic operations is also a vector. Further, we see the [inverted logistic function](https://en.wikipedia.org/wiki/Logit) (1 / (1 + exp(- x)), which is typically hidden in the internals of software packages for fitting logistic regression models.

The next part simulates the final defects.

```R
set.seed(1)
Y <- rbinom(N, size = 1, prob = prob)
```
We first set the seed in that the generated random number keep the same. Then we draw N random defect classifications
from a binomial distribution, with a single trial (size = 1). Since the
random number generator rbinom is also vectorized, we can pass it the vector of probabilities, and produce a defect
corresponding to the probabilities produced by the individual X entries.

We finally run the original model again, but with the synthetic data.
```R
model <- glm(Y ~ X, family = binomial())
print(coef(model))
```
The results of this code are shown in the next table.
The results correspond to alpha and beta, as we have set them 
in the previous substitution code. We recommend that the reader
tries alternatives.

| (Intercept)   |  X            |
| ------------- |:-------------:|
|  -2.9709721   | 0.3936837     |