
# packages
library(plotly)
library(dplyr)

# Generate data
X <- cbind(1, rnorm(1e3))
B <- c(1, 5)
y <- X %*% B + rnorm(1e3)

# Define the objective function (OLS)
f <- function(b1, b2, x, y) {
    as.numeric(t(y - x %*% c(b1, b2)) %*% (y - x %*% c(b1, b2)))
}

# Give support for the function
b1 <- b2 <- sort(c(seq(-50, 50, length = 50), B))

# Store values of function evaluated at the possible values of the inputs
shelf <- matrix(NA, ncol = length(b1), nrow = length(b1))
for (i in 1:length(b1)) {
    for (j in 1:length(b2)) {
        shelf[i, j] <- f(b1 = b1[i], b2 = b2[j], x = X, y = y)
    }
}

# Git it meaningful names
rownames(shelf) <- b1
colnames(shelf) <- b2

# Plot the 3d curve
fig <- plot_ly(
    x = b1,
    y = b2,
    z = shelf
) %>%
    add_surface()

# Solution
coordinates <- as.vector(which(shelf == min(shelf), arr.ind = T))
shelf[coordinates[1], coordinates[2]]

# Add solution to plot
fig %>% add_trace(
    y = b1[coordinates[1]],
    x = b2[coordinates[2]],
    z = f(
        b1 = b1[coordinates[1]],
        b2 = b2[coordinates[2]],
        x = X,
        y = y
    )
)