# Project:   quarto-website
# Objective: How to does proportional odds model work?
# Author:    Edoardo Costantini
# Created:   2023-04-12
# Modified:  2023-04-12
# Notes: 

# Prepare data -----------------------------------------------------------------

# Create a copy of the data
mtcars_fact <- mtcars

# Make carb an ordered factor
mtcars_fact$carb <- factor(mtcars_fact$carb, ordered = TRUE)

# Fit a logistic or probit regression model to an ordered factor response.
plor_test <- MASS::polr(
  formula = carb ~ disp + hp,
  data = mtcars_fact,
  method = "logistic" # proportional odds logistic regression
)

# Log-likelihood computation ---------------------------------------------------

# > logLik function ------------------------------------------------------------

ll_R <- as.numeric(logLik(plor_test))

# Based on predicted probabilities ---------------------------------------------

# Define the probability matrix
prob_mat <- predict(plor_test, type = "probs")

# Define a matrix of cumulative probabilities (for P(Y <= j))
cumsum_mat <- t(apply(cbind(0, prob_mat), 1, cumsum))
round(cumsum_mat, 6)

# Define a storing matrix
shelf <- matrix(nrow = nrow(prob_mat), ncol = ncol(prob_mat))

# Create multinomial trial representation of the dv
y <- FactoMineR::tab.disjonctif(mtcars_fact$carb)

# Store the internal computation of the likelihood
for (i in 1:nrow(prob_mat)) {
  # i <- 1
  for (j in 2:ncol(cumsum_mat)) {
    # j <- 2
    shelf[i, j - 1] <- (cumsum_mat[i, j] - cumsum_mat[i, j - 1])^y[i, j - 1]
  }
}

# Compute the log-likelihood
log(prod(apply(shelf, 1, prod)))

# Compute the log-likelihood simplifying the log
sum(log((apply(shelf, 1, prod))))

# Compute the log-likelihood simplifying the log
sum(log((apply(shelf, 1, prod))))

# Compute the log-likelihood simplifying the log
sum(sum(log(shelf)))

# Work the simplification inside the loop
for (i in 1:nrow(prob_mat)) {
  # i <- 1
  for (j in 2:ncol(cumsum_mat)) {
    # j <- 3
    shelf[i, j - 1] <- y[i, j - 1] * log((cumsum_mat[i, j] - cumsum_mat[i, j - 1]))
  }
}

# And then compute the log-likelihood
sum(sum(shelf))

# Based on logits --------------------------------------------------------------

# Compute bx
bx <- as.matrix(mtcars_fact[, c("disp", "hp")]) %*% plor_test$coefficients

# Compute aj - bx (polr uses this parametrization instated of Agresti's aj + bx)
abx <- sapply(plor_test$zeta, function(a) {
    a - bx
})

# Transform into cumulative probabilities
cumsum_man <- cbind(0, exp(abx) / (1 + exp(abx)), 1)
round(cumsum_man, 6)

# Check they are the same!
cumsum_man - cumsum_mat

# Then I can use this transformation into the computation of the log-likelihood
for (i in 1:nrow(prob_mat)) {
    # i <- 1
    for (j in 2:ncol(cumsum_man)) {
        # j <- 2
        shelf[i, j - 1] <- y[i, j - 1] * log(cumsum_man[i, j] - cumsum_man[i, j - 1])
    }
}

# And then I can compute the log-likelihood
sum(sum(shelf))