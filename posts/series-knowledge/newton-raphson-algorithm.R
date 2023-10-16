# Project:   quarto-website
# Objective: Estimate poisson regression with NR algorithm
# Author:    Edoardo Costantini
# Created:   2023-10-16
# Modified:  2023-10-16
# Notes:     Based on Exercise 1 Chapter 6 from the IOPS optimization course

# Define predictor
x <- c(1, 1, 1, 0, 0, 0)

# Define DV
y <- c(12, 15, 17, 8, 11, 5)

# Store the sample size
N <- length(x)

# Estimate a Poisson regression model
glm_poisson <- glm(
    formula = y ~ x,
    family = poisson
)

# Create a design matrix
xdesign <- cbind(intercept = 1, x)

# define number of iterations for the NR algorithm
maxiter <- 20

# Define storing object for number of iterations
beta.iters <- matrix(NA, maxiter, 2)

# define starting values
beta.iters[1, ] <- c(0, 0)

# Iterations
for (i in 2:maxiter) {
    # Store current estimates for the parameter
    beta0 <- beta.iters[i - 1, 1]
    beta1 <- beta.iters[i - 1, 2]

    # Compute the linear predictor
    theta <- beta0 + beta1 * x

    # Calculate the gradient based on the first derivative
    grad <- (y - exp(theta)) %*% xdesign

    # Compute the hessian matrix based on the second derivative
    hesss <- matrix(0, 2, 2)
    for (j in 1:N) {
        hesss <- hesss + (-lambda[j] * (cbind(xdesign[j, ]) %*% xdesign[j, ]))
    }

    # Generate updated parameter estimate based on the NR step
    beta.iters[i, ] <- beta.iters[i - 1, ] - solve(hesss) %*% t(grad)
}

# Result
rbind(
    R = coef(glm_poisson),
    NS = beta.iters[maxiter, ]
)
