# Project:   quarto-website
# Objective: Understand the relationship between the TP and XWP notation
# Author:    Edoardo Costantini
# Created:   2022-11-25
# Modified:  2022-11-25
# Notes: 

#

library(RegularizedSCA)

# Generate some data with PC structure -----------------------------------------

# Fixed parameters
I <- 1e3 # sample size
J <- 4 # final number of items
K <- 2 # target number of components
rho <- .8 # Fixed correlation between observed items

# Define correlation matrix with blocks
Sigma_K <- lapply(1:K, function(x) {
    D <- matrix(rho,
        nrow = J / K,
        ncol = J / K
    )
    diag(Sigma) <- 1
    Sigma
})
Sigma <- Matrix::bdiag(Sigma_K)

# Define vector of observed item means
mu <- rep(5, J)

# Sample data from multivaraite normal distribution
set.seed(1235)
X <- MASS::mvrnorm(I, mu, Sigma)

# Scale it for future use in PCA
X <- scale(X)

# Perform SVD ------------------------------------------------------------------

# Perform SVD
uSVt <- svd(X)

# Extract objects
U <- uSVt$u
D <- diag(uSVt$d)
V <- uSVt$v

# Check orthonormality
round(t(U) %*% U, 3)
round(t(V) %*% V, 3)

# Formulation 2: standard --------------------------------------------------------

# Component loadings
P_hat <- P_stand <- V

# Component weights
W_hat <- W_stand <- V

# Compute the PC scores
T_hat <- T_stand <- U %*% D
T_hat <- T_stand_xw <- X %*% W_hat

# Constrains: PtP = diag(J)
all.equal(
    t(P_stand) %*% P_stand,
    diag(J)
)

# Formulation 3: Psychometrics ---------------------------------------------------

# Component loadings
P_hat <- P_psych <- (I - 1)^{-1 / 2} * V %*% D

# Component weights
W_hat <- W_psych <- (I - 1)^{1 / 2} * V %*% solve(D)

# Component scores
T_hat <- T_psych <- (I - 1)^{1 / 2} * U
T_hat <- T_psych_xw <- X %*% W_hat

# Formulation 4: classical (EVD) -------------------------------------------------

# Eigenvalue decomposition (EVD)
eigenvalues <- eigen(t(X) %*% X)$values
eigenvectors <- eigen(t(X) %*% X)$vectors

# Component loadings
P_hat <- P_class <- eigenvectors

# Component weights
W_hat <- W_class <- eigenvectors

# Component scores
T_hat <- T_class <- X %*% W_hat

# Constrains: WtW = diag(J)
all.equal(
    t(W_class) %*% W_class,
    diag(J)
)

# Equivalences -----------------------------------------------------------------

# > Component scores -----------------------------------------------------------
# The same component scores are obtained with all formulations

# Standard
TuckerCoef(
    T_stand,
    T_stand_xw
)$tucker_value

# Psychometrics
TuckerCoef(
    T_psych, 
    T_psych_xw
)$tucker_value

# Standard and Psychometrics
TuckerCoef(
    T_stand,
    T_psych
)$tucker_value

# Standard and classical
TuckerCoef(
    T_stand,
    T_class
)$tucker_value

# > Component loadings and component weights -----------------------------------

# Formulations 2 and 3 (standard and classical) are the same
all.equal(
    W_stand,
    W_class
)

# Formulations 2 and 3: W = P
all.equal(
    P_stand,
    W_stand
)

# Formulation 3: W != P (because constraint imposed on T, not P)
all.equal(
    P_psych,
    W_psych
)

# Formulation 3: W propto P
TuckerCoef(
    P_psych,
    W_psych
)$tucker_value

# PCovR special case -----------------------------------------------------------

# Define some Y to be predicted
y <- X %*% rep(1, J) + rnorm(I)

# Define an alpha level
alpha <- 0.5

# Estimate PCovR (Vervolet version)
Hx <- X %*% solve(t(X) %*% X) %*% t(X)
G_vv <- alpha * X %*% t(X) / sum(X^2) + (1 - alpha) * Hx %*% y %*% t(y) %*% Hx / sum(y^2)
EG_vv <- eigen(G_vv) # eigen-decomposition of matrix
T_vv <- EG_vv$vectors[, 1:J]

# Estimate PCovR quantities
out <- PCovR::pcovr_est(
    X = X,
    Y = y,
    a = alpha,
    r = J # fixed number of components
)

# Constrain is applied on T!
round(t(T_vv) %*% T_vv, 3)

# Therefore, like in formulation 2, W != Px (and Py)
out$W
out$Px
out$Py