# Project:   blogdown
# Objective: Study Supervised PCR a la BairEtAl
# Author:    Edoardo Costantini
# Created:   2022-07-27
# Modified:  2022-07-27
# Notes:     https://tibshirani.su.domains/superpc/tutorial.html

# Load automatic package -------------------------------------------------------

library("superpc")

# Run example functions --------------------------------------------------------

# Set seed
set.seed(464)

# Define a matrix of data
x <- matrix(rnorm(1000*100), ncol = 100)

# Compute a component
v1 <- svd(x[1:80, ])$v[, 1]

# Compute an outcome variable based on this component
y <- 2 + 5 * v1 + .05 * rnorm(100)

# Define feature names
featurenames <- paste("X", as.character(1:1000), sep = "")

# Define a train data
data.train <- list(x = x, y = y, featurenames = featurenames)

# Define a test data
xtest <- x
ytest <- 2 + 5 * v1 + .05 * rnorm(100)
data.test <- list(x = xtest, y = ytest, featurenames = featurenames)

# Train the model (computes the scores for each feature)
train.obj <- superpc.train(
  data = data.train,
  type = "regression"
)

# Cross-validate the model
cv.obj <- superpc.cv(
  fit = train.obj,
  data = data.train
)

# Plot the cross-validation curves
superpc.plotcv(cv.obj)

# Note: From this plot we see that the 1st principal component is significant 
#       and the best threshold is around 0.7

# What is in the cross-validation object
ls(cv.obj)

# 
cv.obj$v.preval

# Obtain predictions on test data
fit.cts <- superpc.predict(train.obj,
                           data.train,
                           data.test,
                           threshold = 0.7,
                           n.components = 3,
                           prediction.type = "continuous")

# Manual approach --------------------------------------------------------------

# Compute the cross-validation MSE for a given number of predictors
.spcrCVE <- function(dv, pred, part, K = 10, npcs = 1) {
  # Input examples
  # dv   = as.matrix(mtcars[, 1])
  # pred = as.matrix(mtcars[, -1])
  # K    = 10
  # npcs = 5
  # part = sample(rep(1 : K, ceiling(nrow(mtcars) / K)))[1 : nrow(mtcars)]

  # Define a safe number of pcs
  q <- min(npcs, ncol(pred))

  # Create an empty storing object
  mse <- rep(NA, K)

  # Loop over K folds
  for (k in 1:K) {
    # Partition data:
    Xtr <- pred[part != k, , drop = FALSE]
    Xva <- pred[part == k, , drop = FALSE]
    ytr <- dv[part != k]
    yva <- dv[part == k]

    # Calibrate PCR on training datest
    pcr_out <- pls::pcr(
      ytr ~ Xtr,
      ncomp = q,
      scale = TRUE,
      center = TRUE,
      validation = "none"
    )

    # Get prediction on validation data set
    yva_hat <- predict(pcr_out, newdata = Xva, ncomp = q, type = "response")

    # Save MSE
    mse[k] <- MLmetrics::MSE(
      y_pred = yva_hat,
      y_true = yva
    )
  }

  # Return the CVE:
  cve <- sum(mse * (table(part) / length(part)))

  # Return
  return(list(
    cve = cve,
    npcs = q
  ))
}

# Prepare train data
x_m <- t(x)
  colnames(x_m) <- featurenames

# Prepare test data
x_m_test <- matrix(rnorm(25*1000), ncol = 1e3)
  colnames(x_m_test) <- featurenames

y_m <- y
theta = seq(0.01, .99, by = .01)
npcs <- 5
nfolds <- 10

# Obtain R-squared for all simple linear regression models
r2_vec <- apply(x_m, 2, function(j) {
  sqrt(summary(lm(y_m ~ j))$r.squared)
})

# DEfine predictor groups (pred groups) based on different theta
pred_groups <- lapply(theta, function(m) {
  preds <- colnames(x_m)[r2_vec >= m]
  if (length(preds) >= 1) {
    preds
  } else {
    NULL
  }
})

# If theta used lead only to empty pred groups, say so
if (all(sapply(pred_groups, is.null)) == TRUE) {
  stop(
    paste0(
      "The threshold values used are too high. Try using a lower range."
    )
  )
}

# Drop empty pred_groups slots
pred_groups <- pred_groups[!sapply(pred_groups, is.null)]

# Drop possible duplicated pred_groups slots
pred_groups <- unique(pred_groups)

# Drop preds groups that are smaller than required npcs
pred_groups <- pred_groups[sapply(pred_groups, length) >= npcs]

# If there is no pred group with enough predictors for the required npcs, say so
if (length(pred_groups) == 0) {
  stop(
    paste0(
      "There is no threshold value that can select enough predictors to extract ",
      npcs, " PCs. Try using a smaller npcs or lower theta."
    )
  )
}

# Create a partition vector
part <- sample(rep(1:nfolds, ceiling(nrow(x_m) / nfolds)))[1:nrow(x_m)]

# Obtain Cross-validation error
cve_obj <- lapply(pred_groups, function(set) {
  .spcrCVE(
    dv = y_m,
    pred = x_m[, set, drop = FALSE],
    K = nfolds,
    part = part,
    npcs = npcs
  )
})

# Extract CVEs
cve <- sapply(cve_obj, "[[", 1)
preds_active <- pred_groups[[which.min(cve)]]

# Train PCR on dot xobs sample
pcr_out <- pls::pcr(
  y_m ~ x_m[, preds_active, drop = FALSE],
  ncomp = npcs,
  scale = TRUE,
  center = TRUE,
  validation = "none"
)

# Define sigma
RSS <- sqrt(sum(pcr_out$residuals^2))
sigma <- RSS / (nrow(x_m) - npcs - 1)

# Get prediction on (active) missing part
yhat <- predict(
  object = pcr_out,
  newdata = x_m_test[, preds_active, drop = FALSE],
  ncomp = npcs,
  type = "response"
)

# Add noise for imputation uncertainty
imputes <- yhat + rnorm(sum(wy)) * sigma

# Cross-validation -------------------------------------------------------------

# Define a matrix of data
x <- t(as.matrix(mtcars[1:15, -1]))

# Compute an outcome variable based on this component
y <- as.matrix(mtcars[1:15, 1])

# Define feature names
featurenames <- colnames(x)

# Define a train data
data.train <- list(x = x, y = y, featurenames = featurenames)

# Define a test data
xtest <- t(as.matrix(mtcars[-c(1:15), -1]))
ytest <- as.matrix(mtcars[-c(1:15), 1])
data.test <- list(x = xtest, y = ytest, featurenames = featurenames)

# 1. process the data with the superpc.train function --------------------------

# Look at the code
superpc.train

# Set up the arguments
data <- data.train
type <- "regression"
s0.perc <- NULL

# Sample size
n <- length(y)

# Compute vector of feature means
xbar <- x %*% rep(1 / n, n)

# Same as computing the row means
cbind(xbar, rowMeans(x))

# Compute the diagonal of the cross-product matrix between variables
sxx <- ((x - as.vector(xbar))^2) %*% rep(1, n)

# Which is the mid step for variance
cbind(sxx, apply(x - as.vector(xbar), 1, var) * (n - 1))

# Compute the cross-product matrix between X and Y
sxy <- (x - as.vector(xbar)) %*% (y - mean(y))

# Which is the mid step for covariance between the two
cbind(sxx, apply(x - as.vector(xbar), 1, var) * (n - 1))

# Total sum of squares
syy <- sum((y - mean(y))^2)

# Ratio of the two
numer <- sxy / sxx

# Compute sd?
sd <- sqrt((syy / sxx - numer^2) / (n - 2))

# add "fudge"(?) to the denominator
if (is.null(s0.perc)) {
  fudge <- median(sd)
}
if (!is.null(s0.perc)) {
  if (s0.perc >= 0) {
    fudge <- quantile(sd, s0.perc)
  }
  if (s0.perc < 0) {
    fudge <- 0
  }
}

# Ratio between numerator and sd
tt <- numer / (sd + fudge)

# Compute normalized correlation between y and every x
feature.scores <- tt

# Compute all of the bivariate correlations
cor_easy <- apply(x, 1, function(j) {cor(y, j)})

# How similar are the two?
cor(feature.scores, cor_easy)

# 2. Use cross-validation procedure for theta ----------------------------------

# Look at the code
superpc.cv

# Set up the same arguments
fit = train.obj
data = data.train
n.threshold = 10 
n.fold = NULL
folds = NULL
n.components = 3
min.features = 5 
max.features = nrow(data.train$x)
compute.fullcv = TRUE
compute.preval = TRUE
xl.mode = c(
  "regular",
  "firsttime", 
  "onetime", 
  "lasttime"
)[1]
xl.time = NULL
xl.prevfit = NULL

# Load a special SVD function
mysvd <- function(x,
                  n.components = NULL) {
  # finds PCs of matrix x

  p <- nrow(x)
  n <- ncol(x)

  # center the observations (rows)
  feature.means <- rowMeans(x)
  x <- t(scale(t(x), center = feature.means, scale = FALSE))

  if (is.null(n.components)) {
    n.components <- min(n, p)
  }
  if (p > n) {
    a <- eigen(t(x) %*% x)
    v <- a$vec[, 1:n.components, drop = FALSE]
    d <- sqrt(a$val[1:n.components, drop = FALSE])
    u <- scale(x %*% v, center = FALSE, scale = d)

    return(list(
      u = u,
      d = d,
      v = v,
      feature.means = feature.means
    ))
  } else {
    junk <- svd(x)
    nc <- min(ncol(junk$u), n.components)

    return(list(
      u = junk$u[, 1:nc],
      d = junk$d[1:nc],
      v = junk$v[, 1:nc],
      feature.means = feature.means
    ))
  }
}

# Type of fit
type <- fit$type

# Number of components
n.components <- min(5, n.components)

# Sample size
n <- ncol(data$x)

# Store the normalized correlation scores
cur.tt <- fit$feature.scores

# Define upper and lower bounds of the normalized correlation
lower <- quantile(abs(cur.tt), 1 - (max.features / nrow(data$x)))
upper <- quantile(abs(cur.tt), 1 - (min.features / nrow(data$x)))

# Number of folds
n.fold <- 3
folds <- vector("list", n.fold)
breaks <- round(seq(from = 1, to = (n + 1), length = (n.fold + 1)))
cv.order <- sample(1:n)
for (j in 1:n.fold) {
  folds[[j]] <- cv.order[(breaks[j]):(breaks[j + 1] - 1)]
}

# Storing objects
featurescores.folds <- matrix(nrow = nrow(data$x), ncol = n.fold)

# Thresholds
thresholds <- seq(from = lower, to = upper, length = n.threshold)

# Other important objects
nonzero <- rep(0, n.threshold)
scor <- array(NA, c(n.components, n.threshold, n.fold))
scor.preval <- matrix(NA, nrow = n.components, ncol = n.threshold)
scor.lower <- NULL
scor.upper <- NULL
v.preval <- array(NA, c(n, n.components, n.threshold))

# Define objects for the CV procedure
first <- 1
last <- n.fold

# Then, let's go through the folds
for (j in first:last) {
  # For the j-th fold
  # j <- 1

  # Create a temporary training data
  data.temp <- list(
    x = data$x[, -folds[[j]]],
    y = data$y[-folds[[j]]],
    censoring.status = data$censoring.status[-folds[[j]]]
  )

  # What are the normalized correlations for this fold
  cur.tt <- superpc.train(data.temp, type = type, s0.perc = fit$s0.perc)$feature.scores

  # Store that
  featurescores.folds[, j] <- cur.tt

  # For every threshold check
  for (i in 1:n.threshold) {
    # For the i-th threshold
    # i <- 1

    # Define active set (check which normalized correlations are smaller)
    cur.features <- (abs(cur.tt) > thresholds[i])

    # If there are more then 1 variables, we do this
    if (sum(cur.features) > 1) {
      # Store this number
      nonzero[i] <- nonzero[i] + sum(cur.features) / n.fold

      # Compute the SVD of the active set
      cur.svd <- mysvd(
        x = data$x[cur.features, -folds[[j]]],
        n.components = n.components
      )

      # Scale unseen data
      xtemp <- data$x[cur.features, folds[[j]], drop = FALSE]
      xtemp <- t(scale(t(xtemp),
        center = cur.svd$feature.means,
        scale = FALSE
      ))
      cur.v.all <- scale(t(xtemp) %*% cur.svd$u,
        center = FALSE,
        scale = cur.svd$d
      )

      # Check how many components are available (effective number)
      n.components.eff <- min(sum(cur.features), n.components)

      # Select the PC scores that are available
      cur.v <- cur.v.all[, 1:n.components.eff, drop = FALSE]

      # Store them for this threshold value
      v.preval[folds[[j]], 1:n.components.eff, i] <- cur.v

      # Compute the F-statistic for the possible additive PCRs
      for (k in 1:ncol(cur.v)) {
        # For the k-th PCs
        # k <- 2

        # Compute the linear model
        junk <- summary(lm(data$y[folds[[j]]] ~ cur.v[, 1:k]))

        # Store the F statistic (used as a scaled value of the chi-square stat)
        scor[k, i, j] <- junk$fstat[1]
      }
    }
  }
}

# Define two functions that compute the mean and the sd ignoring the NAs
mean.na <- function(x) {
  mean(x[!is.na(x)])
}
se.na <- function(x) {
  val <- NA
  if (sum(!is.na(x)) > 0) {
    val <- sqrt(var(x[!is.na(x)]) / sum(!is.na(x)))
  }
  return(val)
}

# Compute the log-likelihood scores?
lscor <- apply(log(scor), c(1, 2), mean.na) # average on a more symmetrical scale
se.lscor <- apply(log(scor), c(1, 2), se.na)
scor.lower <- exp(lscor - se.lscor)
scor.upper <- exp(lscor + se.lscor)
scor <- exp(lscor)

# Compute the pre-validation
if (compute.preval) {
  for (i in 1:n.threshold) {
    # i <- 1
    for (j in 1:n.components) {
      # j <- 1
      if (sum(is.na(v.preval[, 1:j, i])) == 0) {
        junk <- summary(lm(data$y ~ v.preval[, 1:j, i]))
        scor.preval[j, i] <- junk$fstat[1]
      }
    }
  }
}

# Store objects
junk <- list(
  thresholds = thresholds, 
  n.threshold = n.threshold,
  nonzero = nonzero, 
  scor.preval = scor.preval, 
  scor = scor,
  scor.lower = scor.lower, 
  scor.upper = scor.upper, 
  folds = folds,
  n.fold = n.fold, 
  featurescores.folds = featurescores.folds,
  v.preval = v.preval, 
  compute.fullcv = compute.fullcv,
  compute.preval = compute.preval, 
  type = type, 
  call = NULL
)

class(junk) <- "superpc.cv"

# 2.1 Plot the likelihood ratio test statistics --------------------------------

# Look at the function
superpc.plotcv

# Define the arguments
object = junk
cv.type = c("full", "preval")[1]
smooth = TRUE
smooth.df = 10
call.win.metafile = FALSE

# Define an internal function used for plotting
error.bars <- function(x, upper, lower, width = 0.005, ...) {
  xlim <- range(x)
  barw <- diff(xlim) * width
  segments(x, upper, x, lower, ...)
  segments(x - barw, upper, x + barw, upper, ...)
  segments(x - barw, lower, x + barw, lower, ...)
  range(upper, lower)
}

# Define the scor object based on the cv.type
if (cv.type == "full") {
    scor <- object$scor
    smooth <- FALSE
} else {
    scor <- object$scor.preval
}

# How many PCs where computed?
k <- nrow(scor)

# Smooth values if required
if (smooth) {
    for (j in 1:nrow(scor)) {
        if (is.null(smooth.df)) {
            om <- !is.na(scor[j, ])
            junk <- smooth.spline(object$th[om], scor[j, 
              om])
            scor[j, om] <- predict(junk, object$th[om])$y
        }
        if (!is.null(smooth.df)) {
            om <- !is.na(scor[j, ])
            junk <- smooth.spline(object$th[om], scor[j, 
              om], df = smooth.df)
            scor[j, om] <- predict(junk, object$th[om])$y
        }
    }
}

# Sample size
n.mean <- 0
for (i in 1:object$n.fold) {
  # i <- 1
  n.mean <- n.mean + length(object$folds[[i]]) / object$n.fold
}

# Degrees of freedom
denom.df <- n.mean - 1 - nrow(scor)

# Define y maximum value for plot
if (cv.type == "full") {
  ymax <- max(
    object$scor.upper[!is.na(object$scor.upper)],
    qf(0.95, nrow(scor), denom.df)
  )
}
if (cv.type == "preval") {
  ymax <- max(
    scor[!is.na(scor)],
    qf(0.95, nrow(scor), denom.df)
  )
}


# Define y-label for plot
ylab <- "Likelihood ratio test statistic"

# Make scatter-plot
matplot(object$th, t(scor),
  xlab = "Threshold", ylab = ylab,
  ylim = c(0, ymax), lty = rep(1, k)
)

# Add lines
matlines(object$th, t(scor), lty = rep(1, k))

# Add references
for (j in 1:k) {
  #
  abline(h = qf(0.95, j, denom.df), lty = 2, col = j)

  # Add error bars
  if (cv.type == "full") {
    delta <- ((-1)^j) * diff(object$th)[1] / 4
    error.bars(object$th + delta * (j > 1), t(object$scor.lower[j, ]), t(object$scor.upper[j, ]), lty = 2, col = j)
  }
}