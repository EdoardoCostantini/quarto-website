## SVD following Linear Algebra course ------------------------------------
## You can reference your notes on lienar algebra and the JamesEtAl2014 and
## HastieEtAl2017 books for SVD PCA relationships

  A <- matrix(c(4, -3, 4, 3), nrow = 2, ncol = 2)

  # SVD directly
  uSVt <- svd(A)
  u     <- uSVt$u # orthonormal basis in the column space of A
  Sigma <- diag(uSVt$d) # diagonal scaling matrix
  V    <- uSVt$v # orthonormal basis in the row space of A

  A %*% V == u %*% Sigma
  A
  u %*% Sigma %*% t(V)

  # SVD as eigendecomposition of a symmetric matrix

  AtA <- t(A) %*% A # Corss-prodcut matrix
  vSvt <- eigen(AtA)
  Sigma2 <- diag(vSvt$values)
  Sigma <- sqrt(Sigma2)
  V <- vSvt$vectors
    V * sqrt(2)

  AAt <- A %*% t(A)
  uSut <- eigen(AAt)
  u <- uSut$vectors * (-1) # take the positive

  A
  u %*% Sigma %*% t(V)

# Data Set ---------------------------------------------------------------------
  p <- 4
  n <- 1e3
  blocks <- 2
  rho <- .8
  Sigma_blocks <- lapply(1:blocks, function (x){
    Sigma <- matrix(rho,
                    nrow = p/blocks,
                    ncol = p/blocks)
    diag(Sigma) <- 1
    Sigma
  })
    Sigma <- Matrix::bdiag(Sigma_blocks)
    mu <- rep(5, p)
    set.seed(1235)
    X <- MASS::mvrnorm(n, mu, Sigma)
  X <- scale(X)

# PCA with prcomp --------------------------------------------------------------
  PCX <- prcomp(X)
  PCX$x         # PC scores
  PCX$sdev
  PCX$rotation  # PC loadings / V / Eigenvectors of XtX

# PCA from SVD -----------------------------------------------------------------
  uSVt  <- svd(X)
  u     <- uSVt$u # orthonormal basis in the column space of X
  Sigma <- diag(uSVt$d) # diagonal scaling matrix
  V     <- uSVt$v # orthonormal basis in the row space of X

  # Scores are the same
  head(X %*% V)
  head(u %*% Sigma)
  head(PCX$x)
  head(prcomp(X, rank. = 3)$x)
  head(prcomp(X, tol = .9)$x)

  # PVE is the same
  apply(X %*% V, 2, var)
  apply(PCX$x, 2, sd)**2
  prop.table(PCX$sdev^2)
  prop.table(diag(Sigma)^2)

# PCA from eigendecomposition -------------------------------------------------

  XtX <- t(X) %*% X
  V <- eigen(XtX)$vectors # PC loadings / V / Eigenvectors of XtX  
  V <- eigen(cor(X))$vectors # PC loadings / V / Eigenvectors of XtX

  # Scores
  head(X %*% V)
  head(PCX$x)

  # PVE
  cumsum(prop.table(eigen(XtX)$values))
  prop.table(PCX$sdev)

# Summary of relationships
  # PC loadings / V / Eigenvectors of XtX
  eigen(XtX)$vectors  # from eigen(XtX)
  uSVt$v              # from svd(X)
  PCX$rotation        # from prcomp(X)

# Data Generated X = TP + E -----------------------------------------------
# Function to orthogonalise and normalise matrices
orthmat <- function(X, verbose = TRUE) {
    for (i in 2:ncol(X)) {
        for (j in 1:(i - 1)) {
            if (verbose == TRUE) {
                print(paste0("Adjusting piar ", i, "-", j))
            }
            A <- X[, j]
            b <- X[, i]

            # Step 1: find projection of b on A
            B <- as.vector(b - (t(A) %*% b / t(A) %*% A) %*% A)

            # Replace in original X the orthogonalized columns
            X[, j] <- A
            X[, i] <- B
        }
    }
    return(X)
}

normmat <- function(X){
    # Step 2: Divide both by l2-norm (length)
    X <- apply(X, 2, function(j) j / sqrt(sum(j^2)))
    return(X)
}

# Fixed Parameters
I <- 1e2 # sample size
J <- 9  # number of variables 
R <- 3   # number of components

# Random sample U
U <- matrix(
  data = rnorm(I * R),
  nrow = I,
  ncol = R
)
U <- scale(U, center = TRUE, scale = FALSE)
U <- orthmat(U, verbose = FALSE)
U <- normmat(U)
  
# Random sample P
V <- matrix(
  data = runif(J * R),
  nrow = J,
  ncol = R
)
V <- orthmat(V, verbose = FALSE)
P <- normmat(V)
round(t(P) %*% P, 3)

# Create D
D <- diag(100 * c(0.5, 0.4, 0.1))

# or
eigenvalues <- c(100, 70, 20)
D <- diag(sqrt(eigenvalues))

# or
D <- diag(c(100, 70, 20))

# Create X
T <- U %*% D
Xtrue  <- T %*% t(P)

# Cumulative Proportion of Explained and unexplained variance
CPVE <- .90

# sample from normal distribution (Ex = Error of X)
Ex <- MASS::mvrnorm(n = I, mu = rep(0, J), Sigma = diag(J))

# centering and scaling the Ex matrix
Ex <- scale(Ex, center = TRUE, scale = FALSE)

# sum of squares
ssqXtrue <- sum(Xtrue^2)
ssqEx <- sum(Ex^2)

# Rescale noise to desired level
Escale <- sqrt(ssqXtrue * (1 - CPVE) / (CPVE * ssqEx))

# Add noise
X  <- Xtrue + Escale * Ex

# Scale for estimation
X <- scale(X)

# Recovring the original elements 
PCA_stats <- prcomp(X)
PCA_psych <- psych::principal(
  X,
  nfactors = ncol(X),
  cor = "cor",
  rotate = "none"
)

# Eigen values
diag(D) / 100
cbind(
  ed = round(prop.table(sqrt(eigen(crossprod(X))$values)), 3),
  svd = round(prop.table(svd(X)$d), 3),
  PCA_stats = round(prop.table(PCA_stats$sdev), 3),
  PCA_psych = round(prop.table(sqrt(PCA_psych$values)), 3)
)

# Eigen values
egnv <- list(
    eigen = eigen(crossprod(X))$values,
    svd = svd(X)$d^2,
    prcomp = (PCA_stats$sdev^2)*I,
    princi = (PCA_psych$values)*I
)

# Explained Variance
diag(D) / 100
round(sapply(egnv, prop.table), 3)

# True Component Scores (T)
data.frame(
  true = head(U %*% D),
  svd = head(svd(Xtrue)$u %*% diag(svd(Xtrue)$d))[, 1:R]
)

# Estimated Component Scores (T_hat)
list(
  svd = head(svd(X)$u %*% diag(svd(X)$d))[, 1:R],
  PCA_stats = head(PCA_stats$x)[, 1:R],
  PCA_stats_scaled = head(scale(PCA_stats$x))[, 1:R],
  PCA_psych = head(PCA_psych$scores)[, 1:R]
)

# Inverse formula
# X = TP'
# X %*% P = T
# T = X %*% P
round(T - Xtrue %*% P, 5)

# Variance of a single T
var((U %*% D)[, 1])
(D[1, 1]^2)/I

# Variance of T_hat
(svd(X)$d^2)/I
apply(svd(X)$u %*% diag(svd(X)$d), 2, var)

# Component Loadings (P)
list(
  P = head(P),
  svd_true = head(svd(Xtrue)$v[, 1:R]),
  svd = head(svd(X)$v[, 1:R]),
  pca = head(PCA_stats$rotation[, 1:R])
)