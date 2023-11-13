
# packages
library(plotly)
library(dplyr)

# Generate data
X <- cbind(1, rnorm(1e3))
B <- c(1, 5)
y <- X %*% B + rnorm(1e3)

# Define the objective function (OLS)
minOLS <- function(b1, b2, x, y) {
    as.numeric(t(y - x %*% c(b1, b2)) %*% (y - x %*% c(b1, b2)))
}

# Give support for the function
b1 <- b2 <- sort(c(seq(-50, 50, length = 50), B))

# Store values of function evaluated at the possible values of the inputs
shelf <- matrix(NA, ncol = length(b1), nrow = length(b1))
for (i in 1:length(b1)) {
    for (j in 1:length(b2)) {
        shelf[i, j] <- minOLS(b1 = b1[i], b2 = b2[j], x = X, y = y)
    }
}

# Git it meaningful names
rownames(shelf) <- b1
colnames(shelf) <- b2

# Plot the 3d curve
ols <- plot_ly() %>%
    add_surface(
        x = b1,
        y = b2,
        z = shelf,
        colorscale = list(c(0, 1), c("tan", "black"))
    )

ols

# Solution
coordinates <- as.vector(which(shelf == min(shelf), arr.ind = T))
shelf[coordinates[1], coordinates[2]]

# Add solution to plot
ols %>% add_trace(
    y = b1[coordinates[1]],
    x = b2[coordinates[2]],
    z = f(
        b1 = b1[coordinates[1]],
        b2 = b2[coordinates[2]],
        x = X,
        y = y
    )
)

# Penalties --------------------------------------------------------------------

b1 <- b2 <- sort(c(seq(-5, 10, length = 50), B))

# Define objective function for Lasso
minEN <- function(b1, b2, x, y, lambda1, lambda2) {
    as.numeric(t(y - x %*% c(b1, b2)) %*% (y - x %*% c(b1, b2)) + lambda1 * sum(abs(b)) + lambda2 * sum(abs(b)^2))
}

# Compute values for Lasso curve
Lasso <- matrix(NA, ncol = length(b1), nrow = length(b1))
for (i in 1:length(b1)) {
    for (j in 1:length(b2)) {
        Lasso[i, j] <- minEN(b1 = b1[i], b2 = b2[j], x = X, y = y, lambda1 = 20, lambda2 = 0)
    }
}

# Compute values for Lasso curve
Ridge <- matrix(NA, ncol = length(b1), nrow = length(b1))
for (i in 1:length(b1)) {
    for (j in 1:length(b2)) {
        Ridge[i, j] <- minEN(b1 = b1[i], b2 = b2[j], x = X, y = y, lambda1 = 0, lambda2 = 20)
    }
}

# Combine plots

plot_ly() %>% 
    add_surface(
        x = b1,
        y = b2,
        z = Lasso,
        colorscale = list(c(0, 1), c("blue", "black"))
    ) %>% 
    add_surface(
        x = b1,
        y = b2,
        z = Ridge,
        colorscale = list(c(0, 1), c("green", "black"))
    )


# Compare penalties with simple linear regression ------------------------------

# Generate data
n <- 10
X <- matrix(rnorm(n), ncol = 1)
B <- 5
y <- X %*% B + rnorm(n)
x <- X

# Define the objective function (OLS)
minOLS <- function(b, x, y) {
    as.numeric(t(y - x %*% b) %*% (y - x %*% b))
}

# Define objective function for Lasso
minLasso <- function(b, x, y, lambda) {
    as.numeric(t(y - x %*% b) %*% (y - x %*% b) + lambda * sum(abs(b)))
}

# Define objective function for Lasso
minRidge <- function(b, x, y, lambda) {
    as.numeric(t(y - x %*% b) %*% (y - x %*% b) + lambda * sum(abs(b)^2))
}

# Give support for the function
b1 <- sort(c(seq(-5, 10, length = 1e3), B))

# Store values of function evaluated at the possible values of the inputs
shelf <- matrix(NA, ncol = 3, nrow = length(b1))
for (i in 1:length(b1)) {
        shelf[i, 1] <- minOLS(b = b1[i], x = X, y = y)
        shelf[i, 2] <- minRidge(b = b1[i], x = X, y = y, lambda = 10)
        shelf[i, 3] <- minLasso(b = b1[i], x = X, y = y, lambda = 10)
}

# Plot
plot(
    x = b1,
    y = shelf[, 1],
    type = "l",
    ylab = "Objective function value",
    xlab = "Beta"
)
lines(x = b1, y = shelf[, 2], col = "blue")
lines(x = b1, y = shelf[, 3], lty = 2, col = "blue")

legend(min(b1), 200,
    legend = c("OLS", "Ridge", "Lasso"),
    col = c("black", "blue", "blue"), 
    lty = c(1, 1, 2), 
    cex = 0.8
)
