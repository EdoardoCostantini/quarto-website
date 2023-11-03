# Project:   quarto-website
# Objective: MM algorithm for the elastic net
# Author:    Edoardo Costantini
# Created:   2023-11-03
# Modified:  2023-11-03
# Notes:

library(reshape2)
library(ggplot2)
library(dplyr)

# Data generation --------------------------------------------------------------

# Define sample size
n <- 50

# How many Predictors
p <- 9

# Create a baseline matrix
R <- diag(p)

# Define special correlation blocks
R[1, 2] <- .8
R[1, 3] <- .7
R[2, 3] <- .6
R[7, 8] <- .2
R[7, 9] <- .4
R[8, 9] <- .3

# Make it simmetrical
R[lower.tri(R)] <- t(R)[lower.tri(R)]

# Sample X
X <- MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = R)

# Define regression coefficeints
B <- c(0, 1, 2, 0, 0, 3, 1.5, 1.5, 0)

# Define dependent variable
y <- X %*% B + rnorm(n, 0, 10)

# MM optimization procedure ----------------------------------------------------

EN_MM <- function(X, y, lambda_L, lambda_R) {
    # Define constant objects
    XX <- t(X) %*% X
    Xy <- t(X) %*% y

    # Define convergence criterion
    epsi <- 1e-12

    # Define starting values for iterative procedure
    conv <- FALSE
    iter <- 1

    # Initialize beta (i minus 1 = 0)
    B_im1 <- rnorm(p)

    # Define l1 penalty
    penalty_L <- sum(abs(B_im1))

    # Define l2 penalty
    penalty_R <- sum((B_im1)^2)

    # Loss function at i minus 1
    loss_im1 <- sum((y - X %*% B_im1)^2) + lambda_L * penalty_L + lambda_R * penalty_R

    while (conv == FALSE) {
        # Define D
        D <- diag((abs(B_im1) + epsi)^(-1))

        # Update Beta
        B_i <- solve(XX + lambda_L / 2 * D + diag(lambda_R, p)) %*% Xy

        # Update l1 penalty
        penalty_L <- colSums(abs(B_i))

        # Update l2 penalty
        penalty_R <- colSums(B_i^2)

        # Update loss function
        loss_i <- sum((y - X %*% B_i)^2) + lambda_L * penalty_L + lambda_R * penalty_R

        # Check convergence
        if (loss_im1 - loss_i < epsi) {
            conv <- TRUE
            B_hat <- B_i
            B_hat[(abs(B_i) < 100 * epsi)] <-  0
        }

        # Update iteration counter
        iter <- iter + 1

        # Update
        B_im1 <- B_i
        loss_im1 <- loss_i
    }

    # result
    as.vector(B_hat)
}

# Define possible values for Ridge
lambda_R_vector <- c(0, 1e-4, .5, 1)
lambda_L_vecotr <- c(20, 15, 11, 10, 8, 6, 5, 4, 3, 2, 1, 0.9, 0.8, 0.7, 0.65, 0.6, 0.5, 0.4, 0.35, 0.25, 0.2, 0.15, 0.125, 0.10, 0.09, 0.08, 0.05, 0.02, 0.015, 0.001, 0.0001, 0.00001, 0)

# Define experimental conditions
conds <- expand.grid(Lambda = lambda_L_vecotr, Ridge = lambda_R_vector)

# Empty matrix for storing estimated parameters
shelf <- matrix(NA, nrow = nrow(conds), ncol = p, dimnames = list(NULL, paste0("B", 1:p)))

# Loop over conditions
for(i in 1:nrow(conds)){
    shelf[i, ] <- EN_MM(X, y, lambda_L = conds[i, "Lambda"], lambda_R = conds[i, "Ridge"])
}

# Attach to conditions
results <- cbind(conds, shelf)

# Melt results
results_ggplot <- melt(results, id.var = c("Lambda", "Ridge"))
head(results_ggplot)

# Make plot
results_ggplot %>%
    filter(variable == "B2") %>% 
    ggplot(
        aes(
            x = Lambda,
            y = value,
            group = variable
        )
    ) +
    geom_line() +
    facet_grid(
        col = vars(Ridge)
    )
