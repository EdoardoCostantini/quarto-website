# Project:   quarto-website
# Objective: Explore log-likelihood extraction function
# Author:    Edoardo Costantini
# Created:   2023-02-02
# Modified:  2023-02-02
# Notes: 

# Set up -----------------------------------------------------------------------

library(nnet) # for multinomial regression

# Functions to compute different R^2 alternatives/generalizations --------------

# McFadden’s R2

R2.McF <- function(L0, Lm){
    # Example arguments
    # L0 <- as.numeric(exp(logLik(multinom(Species ~ 1, data = iris))))
    # Lm <- as.numeric(exp(logLik(multinom(Species ~ Petal.Width, data = iris))))

    # Body
    1 - log(Lm) / log(L0)
}

# Cox and Snell R2

R2.CN <- function(L0, Lm, n) {
    # Example arguments
    # L0 <- as.numeric(exp(logLik(multinom(Species ~ 1, data = iris))))
    # Lm <- as.numeric(exp(logLik(multinom(Species ~ Petal.Width, data = iris))))
    # n <- nrow(iris)

    # Body
    1 - (L0 / Lm)^(2/n)
}

# Fit linear regression models -------------------------------------------------

# Define Dv name
DV <- "Sepal.Length"

# Define univariate predictor's names
IVs <- colnames(iris)[colnames(iris) != DV]

# Fit models
lm.fits <- lapply(c(1, IVs), function(j) {
    lm(as.formula(paste0(DV, " ~ ", j)), data = iris)
})

# Extract regular R2 values
sapply(lm.fits[-1], function(j) { summary(j)$r.squared })

# Extract likelihood values
lvs <- sapply(lm.fits, function(j) {
    exp(as.numeric(logLik(j)))
})

# Define the null model likelihood
L0 <- lvs[1]

# Sample size
n <- nrow(iris)

# Compute all types of R2
R2.types <- data.frame(
    R2 = sapply(lm.fits[-1], function(j) {
        summary(j)$r.squared
    }),
    R2.CN = R2.CN(L0, lvs[-1], n),
    R2.McF = R2.McF(L0, lvs[-1])
)

# Meaning of R2
data.frame(
    sqrtR2 = sqrt(R2.types$R2.CN),
    abscor = c(abs(apply(iris[, IVs[1:3]], 2, cor, iris[, DV])), Species = NaN)
)

# Multinomial Logistic regression ----------------------------------------------

# Define Dv name
DV <- "Species"

# Define univariate predictor's names
IVs <- colnames(iris)[colnames(iris) != DV]

# Fit models
glm.fits <- lapply(c(1, IVs), function(j) {
    multinom(as.formula(paste0("Species ~ ", j)), data = iris)
})

# Give meaningful names
names(glm.fits) <- c("NULL", IVs)

# Extract likelihood values
lvs <- sapply(glm.fits, function(j) {
    exp(as.numeric(logLik(j)))
})

# Define the null model likelihood
L0 <- lvs[1]

# Sample size
n <- nrow(iris)

# Compute all types of R2
R2.types <- data.frame(
    R2.CN = R2.CN(L0, lvs[-1], n),
    R2.McF = R2.McF(L0, lvs[-1])
)

# Square root the CN R2
uni.strength <- sqrt(R2.types$R2.CN)

# And give names again
names(uni.strength) <- rownames(R2.types)

# Create a threshold value range
nt <- 10
thrsh <- seq(min(uni.strength), max(uni.strength), length = nt)

# Keep predictors that are smaller than
active <- list()
for (i in 1:nt) {
    active[[i]] <- names(uni.strength)[uni.strength >= thrsh[i]]
}