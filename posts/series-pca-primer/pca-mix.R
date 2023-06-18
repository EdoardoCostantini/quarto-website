### Object: Performing PCAmix in R
### Source: ChaventEtAl2017 - Multivariate Analysis of Mixed Data:
###         The R Package PCAmixdata

  rm(list = ls())

# Load Package ------------------------------------------------------------

  library(PCAmixdata)
  library("FactoMineR") # for indicator matrix

# Load Data ---------------------------------------------------------------

  data(wine)
  str(wine)

# Example Use -------------------------------------------------------------

  X.quanti <- splitmix(wine)$X.quanti
  X.quali <- splitmix(wine)$X.quali

  pca <- PCAmix(X.quanti[, 1:27], X.quali, ndim=4, graph=FALSE)
  pca$eig
  pca$ind$coord

# PCA as GSVD -------------------------------------------------------------
# Following ChaventEtAl2017
  PCAmix.dt <- wine[, c(1, 2, 16, 22, 29, 28, 30, 31)]
  k <- 3 # number of components to extract

  # Consider Numerical Predictors
  X <- as.matrix(splitmix(PCAmix.dt)$X.quanti)
  pca <- PCAmix(X.quanti = X,
                X.quali = NULL,
                ndim = k, graph=FALSE)
  pca$Z
  ls(pca)

  # Define Z yourself
  n <- nrow(X)
  p <- ncol(X)
  Z <- apply(X, 2, function(x){
    sd_x <- sqrt(sum((x-mean(x))^2)/(length(x))) # not n-1!
    x_cs <- (x - mean(x)) / sd_x
    return(x_cs)
  })
  round(Z - pca$Z, 3) # Standardization check
  round(1/(n)*(t(Z) %*% Z) - cor(X), 3) # correlation check

  # Matrices (or weights)
  N <- diag(rep(1/(n), n)) # Weights for the rows (observations)
  M <- diag(rep(1, p)) # Weights for the columns (variables)
    # note that this M indicates the distance btw two observations is the
    # standard euclidean distance
  diag(pca$M) - M # correct choice

  # Regular SVD of Z
  svdZ <- svd(Z)
  L <- svdZ$d
  U <- svdZ$u
  V <- svdZ$v

  # General SVD of Z
  Z_g <- sqrt(N) %*% Z %*% sqrt(M)
  gsvdZ   <- svd(Z_g)
  L_g <- gsvdZ$d
  U_g <- gsvdZ$u
  V_g <- gsvdZ$v

  # Singular Values
  L_g / L

  # Factor Coordinates
  round(U - U_g, 3)

  # Factor Loadings
  round(V - V_g, 3)

  # Factor Coordinates of Observations "PCs" (eq. 5 and 6)
  all.equal(U %*% diag(L), # standard way of computing them
            Z %*% M %*% V_g, # computing with the GSVD framework
            check.attributes = FALSE)

  Z %*% M %*% V_g
  U_g %*% diag(L_g)

  # Check PCAmix function uses the same thing you use
  round(pca$Z - Z, 3)
  round(diag(pca$M) - M)
  round(pca$V - V_g[, 1:k], 3)
  round(pca$scores - (Z %*% M %*% V)[, 1:k], 3)
  round(pca$scores - (U_g %*% diag(L_g))[, 1:k], 3)

  # Perform with FactorMiner PCA function
  FCM.PCA <- FactoMineR::PCA(Z, ncp = k)
  FCM.PCA$svd$U %*% diag(FCM.PCA$svd$vs[1:k])
  FCM.PCA$ind$coord

# MCA as GSVD -------------------------------------------------------------

PCAmix.dt <- wine[,c(1,2, 16, 22, 29, 28, 30, 31)]

# Consider for a moment only the numerical predictors
X <- as.matrix(splitmix(PCAmix.dt)$X.quali)
n <- nrow(X)
p <- ncol(X)
G <- tab.disjonctif(X)
Z <- scale(G, center = TRUE, scale = FALSE)
N <- diag(1/(n), ncol = n, nrow = n) # Weights for the rows (observations)
ns <- colSums(G) # number of observations that belong to level s
M <- diag(n/ns, ncol = ncol(G), nrow = ncol(G)) # Weights for the columns (variables)

# Regular SVD of Z
svdZ <- svd(Z)
L <- svdZ$d
U <- svdZ$u
V <- svdZ$v

res.mca <- FactoMineR::MCA(X = X, ncp = 2, graph = FALSE)
res.mca$ind$coord

all.equal(U %*% diag(L), # standard way of computing them
          Z %*% M %*% V, # computing with the GSVD framework
          check.attributes = FALSE)

# General SVD of Z
Z_g <- sqrt(N) %*% Z %*% sqrt(M)
gsvdZ   <- svd(Z_g)
L_g <- gsvdZ$d
U_g <- gsvdZ$u
V_g <- gsvdZ$v
all.equal(L_g, L)
all.equal(U, sqrt(solve(N)) %*% U_g)
all.equal(V, sqrt(solve(M)) %*% V_g)

# PCAMIX ------------------------------------------------------------------
# Followig ChaventEtAl2012

  df <- wine[, c(1, 2, 16, 22, 29, 28, 30, 31)]
  df.quant <- splitmix(df)$X.quanti
  df.quali <- splitmix(df)$X.quali
  n <- nrow(df)
  p <- ncol(df)
  p1 <- ncol(df.quant)
  p2 <- ncol(df.quali)
  m <- sapply(df.quali, nlevels)
  G <- tab.disjonctif(df.quali)
  D <- diag(colSums(G))
  J <- diag(rep(1, n)) - rep(1, n) %*% t(rep(1, n))/n

  Z1 <- apply(df.quant, 2, function(x){
    sd_x <- sqrt(sum((x-mean(x))^2)/(length(x))) # not n-1!
    return((x-mean(x))/sd_x)
  })
  Z2 <- J %*% G %*% sqrt(solve(D))
  Z <- cbind(Z1, Z2) / sqrt(n)
  k <- 3 # number of components

  # General SVD
  svdZ   <- svd(Z)
  L <- svdZ$d
  U <- svdZ$u
  V <- svdZ$v

  # Check expectations
  round(t(U) %*% U, 3)
  round(t(V) %*% V, 3)

  # Calculate Standardized Component Scores
  X <- sqrt(n) * U[, 1:k]

  # Compare results to function
  pca <- PCAmix(df.quant, df.quali, ndim = k, graph=FALSE)
  ls(pca)
  round(pca$Z - Z, 3)
  round(pca$scores.stand - X, 3)
  round(pca$scores - X, 3)

# PCAMIX ------------------------------------------------------------------
# Followig ChaventEtAl2017

  df <- wine[, c(1, 2, 16, 22, 29, 28, 30, 31)]
  n <- nrow(df)
  p <- ncol(df)
  X1 <- splitmix(df)$X.quanti
  X2 <- splitmix(df)$X.quali

  # Perform PCAMIX with PCAmix package
  k <- 3 # number of components
  out_pcamix <- PCAmix(X.quanti = X1,
                       X.quali = X2,
                       ndim = k, graph = FALSE)

  # Prepare data for doing it yourself
  p1 <- ncol(X1)
  p2 <- ncol(X2)
  m <- sum(sapply(X2, nlevels))
  G <- tab.disjonctif(X2)
  D <- diag(colSums(G))
  J <- diag(rep(1, n)) - rep(1, n) %*% t(rep(1, n))/n

  # Centered Versions
  Z1 <- apply(X1, 2, function(x){
    sd_x <- sqrt(sum((x-mean(x))^2)/(length(x))) # not n-1!
    return((x-mean(x))/sd_x)
  })
  Z2 <- J %*% G %*% solve(sqrt(D))
  # Z2 <- scale(G, T, F)
  Z <- cbind(Z1, Z2)
  out_pcamix$Z[, 1:p1]
  out_pcamix$Z[, -(1:p1)]

  # General SVD
  svdZ   <- svd(Z)
  L <- svdZ$d
  U <- svdZ$u
  V <- svdZ$v

  # Check expectations
  round(t(U) %*% U, 3)
  round(t(V) %*% V, 3)

  # Calculate Standardized Component Scores
  X <- sqrt(n) * U[, 1:k]

  # Compare results to function
  pca <- PCAmix(df.quant, df.quali, ndim = k, graph=FALSE)
  ls(pca)
  round(pca$Z - Z, 3)
  round(pca$scores.stand - X, 3)
  round(pca$scores - X, 3)
