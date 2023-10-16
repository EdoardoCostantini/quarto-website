# Project:   quarto-website
# Objective: showcase how the taylor series expansion works
# Author:    Edoardo Costantini
# Created:   2023-10-16
# Modified:  2023-10-16
# Notes: 

# Univariate function ----------------------------------------------------------

# Original function f(x)
f <- function(x) {
    exp(x) / (1 + exp(x))
}

# R function to compute values of the derivatives at given x value
df <- function(x) {
    # First derivative
    df1 <- exp(x) / (1 + exp(x))^2
    # Second derivative
    df2 <- -exp(x) * (exp(x) - 1) / (1 + exp(x))^3
    # Third Derivative
    df3 <- exp(x) * (-4 * exp(x) + exp(2 * x) + 1) / (exp(x) + 1)^4
    # Results
    u <- list(df1, df2, df3)
    return(u)
}

# Some range of X
x <- seq(-10, 10, .001)

# Decide a point for the approximation
x0 <- 2

# Evaluate derivatives at x0
u0 <- df(x0)

# Compute the ingredients for approximation at x0
tx <- lapply(
    1:3,
    function(k) {
            u0[[k]] / factorial(k) * ((x - x0)^k)
    }
)

# Plot the function
plot(
    x = x,
    y = f(x),
    type = "l",
    lwd = 2,
    xlab = expression(italic(x)),
    ylab = expression(italic(f)(italic(x))),
    ylim = c(-1.5, 1.5)
)

# Add Talyor series approximations at x0
lines(x, f(x0) + tx[[1]], col = "#ff000085")
lines(x, f(x0) + tx[[1]] + tx[[2]], col = "blue")
lines(x, f(x0) + tx[[1]] + tx[[2]] + tx[[3]], col = "red")

# Multivariate function --------------------------------------------------------

fff <- function(x1, x2) {
    # Original Function
    f <- exp(x1) * (4 * x1^2 + 2 * x2^2 + 4 * x1 * x2 + 2 * x2 + 1)
    # Gradient
    f1 <- c(
        # First derivative with respect to x1
        f + exp(x1) * (8 * x1 + 4 * x2),
        # First derivative with respect to x2
        exp(x1) * (4 * x2 + 4 * x1 + 2)
    )
    # Hessian Matrix
    f2 <- matrix(
        c(
            # Second derivative with respect to x1
            f + 2 * exp(x1) * (8 * x1 + 4 * x2) + 8 * exp(x1),
            # First derivative with respect to x1, second derivative with respect to x2
            exp(x1) * (4 * x2 + 4 * x1 + 2) + 4 * exp(x1),
            # First derivative with respect to x1, second derivative with respect to x2
            exp(x1) * (4 * x2 + 4 * x1 + 2) + 4 * exp(x1), 
            # Second derivative with respect to x2
            4 * exp(x1)
        ), 2, 2, byrow = TRUE
    )
    u <- list(f = f, f1 = f1, f2 = f2)
    return(u)
}

# Check (0.5,-1)
Hmat <- fff(0.5, -1)$f2

# All eigen values are negative: positive definite!
all(- eigen(Hmat)$values < 0)

# Determinant version: positive definite!
(Hmat[1, 1] * Hmat[2, 2] - Hmat[1, 2] * Hmat[2, 1]) > 0

# Check (-1.5,1)
Hmat <- fff(-1.5, 1)$f2

# Note all eigen values are negative: degenerate!
all(-eigen(Hmat)$values < 0)

# Determinant version: degenerate!
(Hmat[1, 1] * Hmat[2, 2] - Hmat[1, 2] * Hmat[2, 1]) > 0
