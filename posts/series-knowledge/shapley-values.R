# Project:   quarto-website
# Objective: Understand Shapley values
# Author:    Edoardo Costantini
# Created:   2022-11-23
# Modified:  2022-11-23
# Notes: 

# A general understanding of feature importance -------------------------------

# Consider a linear model
lmfit <- lm(mpg ~ cyl + disp + hp, data = mtcars)

# Define an observation of interest
i <- 1

# Define ivs and dvs
x <- c("cyl", "disp", "hp")
y <- "mpg"

# Compute individual feature contributions
phi_1 <- coef(lmfit)[x[1]] * mtcars[i, x[1]] - coef(lmfit)[x[1]] * mean(mtcars[, x[1]])
phi_2 <- coef(lmfit)[x[2]] * mtcars[i, x[2]] - coef(lmfit)[x[2]] * mean(mtcars[, x[2]])
phi_3 <- coef(lmfit)[x[3]] * mtcars[i, x[3]] - coef(lmfit)[x[3]] * mean(mtcars[, x[3]])

# Sum them (Efficiency)
phi_1 + phi_2 + phi_3

# It's the same as the prediction minus the average prediction
predict(lmfit)[i] - mean(predict(lmfit))

# Therefore we can interpret the effect of hp to be more than twice as 
# important as the effect of cyl.
# The "importance" here is how much every feature contributes to the difference 
# from the average prediction.
phi_3 / phi_1

# Monte Carlo approximation algorithm ------------------------------------------

# Prep packages
library("shapr")
library("randomForest")

# Load some data
data("Boston", package = "MASS")

# Divide variable roles
x_var <- c(
    "lstat",# lower status of the population (percent).
    "rm",   # average number of rooms per dwelling.
    "dis",  # distances to Boston employment centers.
    "indus" # proportion of non-retail business acres per town
)
y_var <- "medv" # median value of owner-occupied homes in $1000s.

# Fit a random forest
X <- Boston[, x_var]
y <- Boston[, y_var]
model <- randomForest(x = X, y = y, importance = TRUE)

# Shapley values computation
n <- nrow(X)
p <- length(x_var)

# Set up:
shap_val <- rep(NA, p) # Storing object for shapely values (1 per feature)
M <- 1e2 # iterations

for (j in x_var) {
    print(j)
    # j <- x_var[2] # index of the feature of interest

    # Pick an observation of interest
    x <- X[sample(1:n, 1), ] # instance of interest

    # Store shapely values for each feature
    phimj <- rep(NA, M)

    # Iterate
    for (m in 1:M) {
        # Draw a row
        z <- X[sample(1:n, 1), ]

        # Random order of the features
        o <- sample(x_var, size = p, replace = FALSE)

        # Define monsters
        xmj <- xpj <- x[, o]

        # Replace values in monsters
        position <- which(j == o)
        xmj[, 1:p < position] <- z[, 1:p < position]
        xpj[, 1:p <= position] <- z[, 1:p <= position]

        # Store contribution
        phimj[m] <- predict(model, newdata = xpj) - predict(model, newdata = xmj)
    }

    shap_val[which(j == x_var)] <- mean(phimj)
}

# Efficiency
sum(shap_val)
predict(model)[1] - mean(predict(model))

# Correct way to do it from the internet ---------------------------------------

# Algorithm to Estimate it
sample.shap <- function(model, R, x, feature, X) {
    phi <- numeric(R) # to store Shapley values
    N <- nrow(X) # sample size
    p <- ncol(X) # number of features
    b1 <- b2 <- x
    for (m in seq_len(R)) {
        w <- X[sample(N, size = 1), ] # randomly drawn instance
        ord <- sample(names(w)) # random permutation of features
        swap <- ord[seq_len(which(ord == feature) - 1)]
        b1[swap] <- w[swap]
        b2[c(swap, feature)] <- w[c(swap, feature)]
        phi[m] <- predict(model, newdata = b1) - predict(model, newdata = b2)
    }
    mean(phi)
}

# Estimate Shapley values for all variables
shap_values <- sapply(colnames(X), function(j){
    sample.shap(model = model, R = 1e2, x = X[2, ], feature = j, X = X)
})

# Interpretation
predict(model)[2] - mean(predict(model))
sum(shap_values)

# Estimate Shapley values for a single variable across observations
shap_values <- sapply(sample(1:n, 100), function(i) {
    print(i)
    sample.shap(model = model, R = 1e2, x = X[i, ], feature = colnames(X)[4], X = X)
})