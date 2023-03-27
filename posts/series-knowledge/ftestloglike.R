# Project:   quarto-website
# Objective: Explore the relationship between log-likelihood and F statistic
# Author:    Edoardo Costantini
# Created:   2023-02-02
# Modified:  2023-02-02
# Notes:

# Set seed
set.seed(20230222)

# Load packs
library(lmtest) # for likelihood ratio test

# Sample size
n <- 100

# Total number of variables
P <- 50
p <- 2

# Sample predictor values
Z <- matrix(rnorm(n * P), nrow = n, ncol = P)

# Define a matrix of true predictors
X <- Z[, 1:p]

# Generate a dependent variable (true regression line)
y_true <- as.vector(X %*% rep(1, p))

# Generate random error
error <- rnorm(n)

# Define a function to make columns orthogonal
orthmat <- function(X, verbose = FALSE) {
    # Internals -------------------------------------------------------------

    # X    = matrix(rnorm(1e3 * 4), nrow = 1e3, ncol = 4)

    # Body ------------------------------------------------------------------
    for (i in 2:ncol(X)) {
        for (j in 1:(i - 1)) {
            if (verbose == TRUE) {
                print(paste0("Adjusting piar ", i, "-", j))
            }
            A <- X[, j]
            b <- X[, i]

            # Find projection of b on A
            B <- as.vector(b - (t(A) %*% b / t(A) %*% A) %*% A)

            # Replace in original X the orthogonalized columns
            X[, j] <- A
            X[, i] <- B
        }
    }
    return(X)
}

# Make the error orthogonal to the Xs
e_ortho <- orthmat(cbind(X, error))[, "error"]

# Define a proportion of explained variance in the dv
R2 <- .8

# Define a scale for the noise
e_scale <- sqrt(sum(X^2) * (1 - R2) / (R2 * sum(e_ortho^2)))

# Add scaled noise
y <- y_true + e_scale * e_ortho

# Check model fit is correct
summary(lm(y ~ X))

# Fit a null model
lm_null <- lm(y ~ 1)

# Fit a model with only the true predictors
lm_true <- lm(y ~ X)

# Fit a model with all the true predictors
lm_all <- lm(y ~ Z)

# Check that the F statistic is higher when the correct number of predictors is kept
summary(lm_true)$fstatistic
summary(lm_all)$fstatistic

# Check how the likelihood reacts to this
logLik(lm_true)
logLik(lm_all)

# Check how the likelihood ratio test reacts to this
(lrt_ture <- -2 * (logLik(lm_null) - logLik(lm_true)))
(lrt_all <- -2 * (logLik(lm_null) - logLik(lm_all)))

# Is the true better than the all?
lrt_ture > lrt_all

# chi-square values
lrtest(lm_null, lm_true)
lrtest(lm_null, lm_all)

qsd_true <- as.numeric(-2 * (logLik(lm_null) - logLik(lm_true)))
qsd_all <- as.numeric(-2 * (logLik(lm_null) - logLik(lm_all)))

pchisq(-2 * (logLik(lm_null) - logLik(lm_true)), df = 2, lower.tail = FALSE)
pchisq(-2 * (logLik(lm_null) - logLik(lm_all)), df = 5, lower.tail = FALSE)

lines(density(rchisq(1e3, df = 20)))
plot(density(rchisq(1e3, df = 50)), xlim = c(0, 100), ylim = c(0, .1))
lines(density(rchisq(1e3, df = 30)))

# Relationship between chi-square value and F statistic
qsd_true / abs(length(coef(lm_null)[-1]) - length(coef(lm_true)[-1]))

qsd_true / 

# Check how the BIC reacts to this
(length(lm_true$coefficients) + 1) * log(n) - 2 * logLik(lm_true)
(length(lm_all$coefficients) + 1) * log(n) - 2 * logLik(lm_all)

# Check how the MSE reacts
MLmetrics::MSE(y_pred = predict(lm_true), y_true = y) -
    MLmetrics::MSE(y_pred = predict(lm_all), y_true = y)
sum((predict(lm_true) - y)^2) / (n - length(lm_true$coefficients) - 1) - 
    sum((predict(lm_all) - y)^2) / (n - length(lm_all$coefficients) - 1)

