# Project:   quarto-website
# Objective: spcr cv functions
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-03-16
# Notes: 

# Closest version to spcr package possible -------------------------------------

.spcr.cv.pkg <- function(dv, ivs, fam = "gaussian", nthrs = 10, maxnpcs = 3, K = 5) {
  # Example inputs
  # dv <- mtcars[, 1]
  # ivs <- t(mtcars[, -1])
  # nthrs = 10
  # fam <- c("gaussian", "binomial", "poisson")[2]
  # maxnpcs <- 3
  # K = 5

  # Process data
  x = t(as.matrix(ivs))
  y = dv
  featurenames = colnames(ivs)

  #
  s0.perc <- NULL

  # Sample size
  n <- length(y)

  # Compute vector of feature means
  xbar <- rowMeans(x)

  # Compute the diagonal of the cross-product matrix between variables
  sxx <- ((x - xbar)^2) %*% rep(1, n)

  # Compute the cross-product matrix between X and Y
  sxy <- (x - xbar) %*% (y - mean(y))

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

  # Set up the same arguments
  fit <- train.obj
  data <- data.train
  n.threshold <- nthrs
  n.fold <- K
  folds <- NULL
  n.components <- maxnpcs
  min.features <- 5
  max.features <- nrow(data.train$x)
  compute.fullcv <- TRUE
  compute.preval <- TRUE
  xl.mode <- c(
    "regular",
    "firsttime",
    "onetime",
    "lasttime"
  )[1]
  xl.time <- NULL
  xl.prevfit <- NULL

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

  # Check selected predictors
  map_active_pkg <- sapply(1:n.threshold, function(a) sort(cur.tt) > thresholds[a])

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

  # Compute the log-likelihood scores?
  lscor <- apply(log(scor), c(1, 2), mean.na) # average on a more symmetrical scale
  se.lscor <- apply(log(scor), c(1, 2), se.na)
  scor.lower <- exp(lscor - se.lscor)
  scor.upper <- exp(lscor + se.lscor)
  scor <- exp(lscor)

  # Give names that make sense
  colnames(scor) <- round(thresholds, 3)

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

  # K-fold Cross-Validation Choice
  coord_KCVC <- which(
    scor == max(scor, na.rm = TRUE), # TODO: min or max?
    arr.ind = TRUE
  )

  # Which threshold has been selected?
  thr.cv <- thresholds[coord_KCVC[, "col"]]

  # How many npcs have been selected?
  Q.cv <- coord_KCVC[, "row"]

  # Return
  return(
    list(
      thr.cv = thr.cv,
      # pred.active = pred.map[, coord_KCVC[, "col"]],
      thr = as.numeric(thresholds),
      Q.cv = Q.cv,
      scor = scor,
      # pred.map = pred.map,
      pred.active = rownames(cur.tt)[cur.tt > thr.cv]
    )
  )
}

# My own take on the spcr package version --------------------------------------

.spcr.cv <- function(
  dv, 
  ivs, 
  fam = "gaussian",
  thrs = c("LLS", "pseudoR2", "normalized")[1],
  nthrs = 10,
  maxnpcs = 3,
  K = 5,
  test = c("LRT", "F", "MSE")[2],
  max.features = ncol(ivs),
  min.features = 5
  ) {

  # Example inputs
  # dv <- mtcars[, 1]
  # ivs <- mtcars[, -1]
  # thrs = c("LLS", "pseudoR2", "normalized")[3]
  # nthrs = 10
  # fam <- c("gaussian", "binomial", "poisson")[1]
  # maxnpcs <- 5
  # K = 2
  # test = c("LRT", "F", "MSE")[3]
  # max.features = ncol(ivs)
  # min.features = 1

  # Sample size
  n <- nrow(ivs)

  # Fit null model
  glm0 <- glm(dv ~ 1, family = fam)

  # Fit univariate models
  glm.fits <- lapply(1:ncol(ivs), function(j) {
    glm(dv ~ ivs[, j], family = fam)
  })

  # Extract Log-likelihood values
  ll0 <- as.numeric(logLik(glm0))
  lls <- sapply(glm.fits, function(m) as.numeric(logLik(m)))

  # Create active sets based on threshold type

  if(thrs == "LLS"){

    # Use the logLikelihoods as bivariate association scores
    ascores <- lls

    # Give it good names
    names(ascores) <- colnames(ivs)

    # Define the upper and lower bounds of the association
    lower <- min(ascores)
    upper <- max(ascores)

  }

  if(thrs == "pseudoR2"){

    # Compute pseudo R-squared
    CNR2 <- 1 - exp(-2 / n * (lls - ll0))

    # Give it good names
    names(CNR2) <- colnames(ivs)

    # Make them correlation coefficients
    ascores <- sqrt(CNR2)

    # Define upper and lower bounds of the association
    lower <- quantile(ascores, 1 - (max.features / ncol(ivs)))
    upper <- quantile(ascores, 1 - (min.features / ncol(ivs)))

  }

  if (thrs == "normalized") {
    
    # Set objects to the required dimension
    x <- t(as.matrix(ivs))
    y <- dv
    featurenames <- colnames(ivs)

    # Empty
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

    # Store the normalized correlation scores
    ascores <- abs(tt)[, 1]

    # Define upper and lower bounds of the normalized correlation
    lower <- quantile(abs(ascores), 1 - (max.features / nrow(x)))
    upper <- quantile(abs(ascores), 1 - (min.features / nrow(x)))

  }

  # Define threshold values
  thrs_values <- seq(from = lower, to = upper, length.out = nthrs)

  # Create a map of active predictors based on threshold values
  pred.map <- sapply(1:nthrs, function(a) ascores > thrs_values[a])

  # Use thresholds as names
  colnames(pred.map) <- round(thrs_values, 3)

  # If two thresholds are giving the same result reduce the burden
  pred.map <- pred.map[, !duplicated(t(pred.map))]

  # Get rid of thresholds that are keeping too few predictors
  pred.map <- pred.map[, colSums(pred.map) >= min.features]

  # Get rid of thresholds that are keeping too many predictors
  pred.map <- pred.map[, colSums(pred.map) <= max.features]

  # And update the effective number of the thresholds considered
  nthrs.eff <- ncol(pred.map)
  
  # Create an object to store k-fold cross-validation log-likelihoods
  map_kfcv <- array(
    dim = c(maxnpcs, nthrs.eff, K),
    dimnames = list(NULL, colnames(pred.map), NULL)
  )

  # Create a fold partitioning object
  part <- sample(rep(1:K, ceiling(nrow(ivs) / K)))[1:nrow(ivs)]

  # Loop over K folds
  for (k in 1:K) {
    # k <- 1

    # Create fold data:
    Xtr <- ivs[part != k, , drop = FALSE]
    Xva <- ivs[part == k, , drop = FALSE]
    ytr <- dv[part != k]
    yva <- dv[part == k]

    # Null model
    glm_null_va <- glm(yva ~ 1, family = fam)

    # Loop over threshold values
    for (thr in 1:nthrs.eff) {
      # thr <- 1
      # Define the active set of predictors based on the current threshold value
      aset <- pred.map[, thr]

      # If there is more than 1 active variable
      if (sum(aset) > 1) {
        # Scale Xs
        Xtr_thr <- scale(Xtr[, aset], center = TRUE, scale = TRUE)
        Xva_thr <- scale(Xva[, aset],
          center = attributes(Xtr_thr)$`scaled:center`,
          scale = attributes(Xtr_thr)$`scaled:scale`
        )

        # Perform PCA
        cur.svd <- mysvd(t(Xtr[, aset]), n.components = maxnpcs)

        # Make a temp validation object
        xtemp <- t(Xva)[aset, , drop = FALSE]

        # Center the temp data
        xtemp <- t(scale(t(xtemp), center = cur.svd$feature.means, scale=FALSE))

        # Produce components on the validation data
        cur.v.all <- scale(t(xtemp) %*% cur.svd$u, center=FALSE, scale=cur.svd$d)

        # Check how many components are available (effective number)
        q.eff <- min(sum(aset), maxnpcs)

        # Keep only the effective components
        cur.v <- cur.v.all[, 1:q.eff, drop = FALSE]

        # svd_Xtr <- svd(Xtr_thr)

        # # Project training and validation data on the PCs
        # PCtra <- Xtr_thr %*% svd_Xtr$v
        # PCsva <- Xva_thr %*% svd_Xtr$v


        # # Select the available PC scores
        # PCtra.eff <- PCtra[, 1:q.eff, drop = FALSE]
        # PCsva.eff <- PCsva[, 1:q.eff, drop = FALSE]

        # Compute the F-statistic for the possible additive PCRs
        for (Q in 1:q.eff) {
          # Q <- 1

          glmfit <- glm(yva ~ cur.v[, 1:Q], family = fam)

          # # Train GLM model
          # glm_fit_tr <- glm(ytr ~ PCtra.eff[, 1:Q], family = fam)
          # glm_null_tr <- glm(ytr ~ 1, family = fam)
          
          # # Obtain prediction based on new data
          # yva_hat <- cbind(1, PCsva.eff[, 1:Q]) %*% coef(glm_fit_tr)

          # # Obtain validation residuals
          # r_va <- (yva - yva_hat)

          # # Store the estimate of the sigma
          # s <- sqrt(sum(resid(glm_fit_tr)^2) / (length(ytr))) # maximum likelihood version
          # # s <- sqrt(sum(r_va^2) / (length(yva)))              # based on va

          # # Compute validation data log-likelihood
          # loglik_mod <- -nrow(PCsva) / 2 * log(2 * pi) - nrow(PCsva) / 2 * log(s^2) - 1 / (2 * s^2) * sum(r_va^2)

          # # Compute null model log-likelihood on validation data
          # s_null <- sqrt(sum(resid(glm_null_tr)^2) / (length(ytr))) # maximum likelihood version
          # r_null <- yva - mean(ytr)
          # loglik_null <- -nrow(PCsva) / 2 * log(2 * pi) - nrow(PCsva) / 2 * log(s_null^2) - 1 / (2 * s_null^2) * sum(r_null^2)

          # Extract desired statistic
          if (test == "F") {
            # F statistic
            map_kfcv[Q, thr, k] <- anova(glmfit, test = "F")$F[2]
          }
          if (test == "LRT") {
            # Chi-square value is the likelihood ratio test
            map_kfcv[Q, thr, k] <- lmtest::lrtest(glmfit)$Chi[2]
          }
          if (test == "AIC") {
            # AIC from glmfit
            map_kfcv[Q, thr, k] <- glmfit$aic
          }
          if (test == "PR2") {
            map_kfcv[Q, thr, k] <- as.numeric(1 - exp(-2 / nobs(glmfit) * (logLik(glmfit) - logLik(glm_null_va))))
          }
          if (test == "MSE") {
            map_kfcv[Q, thr, k] <- MLmetrics::MSE(y_pred = yva_hat, y_true = yva)
          }
          if (test == "BIC") {
            map_kfcv[Q, thr, k] <- as.numeric(log(nobs(glmfit)) * (Q + 1 + 1) - 2 * logLik(glmfit))
          }
        }
      }

    }
  }

  # Average selected score across folds

  if(test == "F"){
    # average F scores on a more symmetrical scale
    lscor <- apply(log(map_kfcv), c(1, 2), mean, na.rm = FALSE)

    # revert to correct scale
    scor <- exp(lscor)

    # K-fold Cross-Validation solution
    kfcv_sol <- which(
      scor == max(scor, na.rm = TRUE), # TODO: min or max?
      arr.ind = TRUE
    )
  }

  if(test == "LRT" | test == "PR2"){
    # Mean of the likelihood ratio test statistics
    scor <- apply(map_kfcv, c(1, 2), mean, na.rm = FALSE)

    # K-fold Cross-Validation solution
    kfcv_sol <- which(
      scor == max(scor, na.rm = TRUE), # TODO: min or max?
      arr.ind = TRUE
    )

  }

  if (test == "MSE" | test == "BIC" | test == "AIC") {
    # Mean of the likelihood ratio test statistics
    scor <- apply(map_kfcv, c(1, 2), mean, na.rm = FALSE)

    # K-fold Cross-Validation solution
    kfcv_sol <- which(
      scor == min(scor, na.rm = TRUE),
      arr.ind = TRUE
    )
  }

  # Which threshold has been selected?
  thr.cv <- as.numeric(names(scor[kfcv_sol[1], kfcv_sol[2]]))

  # How many npcs have been selected?
  Q.cv <- as.numeric(kfcv_sol[, "row"])

  # Return
  return(
    list(
      thr.cv = thr.cv,
      thr = thrs_values,
      Q.cv = Q.cv,
      scor = scor,
      pred.map = pred.map,
      pred.active = rownames(pred.map)[pred.map[, kfcv_sol[, "col"]]]
    )
  )
}

# Full CV approach -------------------------------------------------------------

.spcr.cv.full <- function(
  dv, 
  ivs, 
  fam = "gaussian",
  thrs = c("LLS", "pseudoR2", "normalized")[1],
  nthrs = 10,
  maxnpcs = 3,
  K = 5,
  test = c("LRT", "F", "MSE")[2],
  max.features = ncol(ivs),
  min.features = 5,
  oneSE = TRUE
  ) {

  # Example inputs
  # dv <- mtcars[, 1]
  # ivs <- mtcars[, -1]
  # thrs = c("LLS", "pseudoR2", "normalized")[3]
  # nthrs = 5
  # fam <- c("gaussian", "binomial", "poisson")[1]
  # maxnpcs <- 10
  # K = 2
  # test = c("LRT", "F", "MSE")[2]
  # max.features = ncol(ivs)
  # min.features = 1

  # Sample size
  n <- nrow(ivs)

  # Fit null model
  glm0 <- glm(dv ~ 1, family = fam)

  # Fit univariate models
  glm.fits <- lapply(1:ncol(ivs), function(j) {
    glm(dv ~ ivs[, j], family = fam)
  })

  # Extract Log-likelihood values
  ll0 <- as.numeric(logLik(glm0))
  lls <- sapply(glm.fits, function(m) as.numeric(logLik(m)))

  # Create active sets based on threshold type

  if(thrs == "LLS"){

    # Use the logLikelihoods as bivariate association scores
    ascores <- lls

    # Give it good names
    names(ascores) <- colnames(ivs)

    # Define the upper and lower bounds of the association
    lower <- min(ascores)
    upper <- max(ascores)

  }

  if(thrs == "pseudoR2"){

    # Compute pseudo R-squared
    CNR2 <- 1 - exp(-2 / n * (lls - ll0))

    # Give it good names
    names(CNR2) <- colnames(ivs)

    # Make them correlation coefficients
    ascores <- sqrt(CNR2)

    # Define upper and lower bounds of the association
    lower <- quantile(ascores, 1 - (max.features / ncol(ivs)))
    upper <- quantile(ascores, 1 - (min.features / ncol(ivs)))

  }

  if (thrs == "normalized") {
    
    # Set objects to the required dimension
    x <- t(as.matrix(ivs))
    y <- dv
    featurenames <- colnames(ivs)

    # Empty
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

    # Store the normalized correlation scores
    ascores <- abs(tt)[, 1]

    # Define upper and lower bounds of the normalized correlation
    lower <- quantile(abs(ascores), 1 - (max.features / nrow(x)))
    upper <- quantile(abs(ascores), 1 - (min.features / nrow(x)))

  }

  # Define threshold values
  thrs_values <- seq(from = lower, to = upper, length.out = nthrs)

  # Create a map of active predictors based on threshold values
  pred.map <- sapply(1:nthrs, function(a) ascores > thrs_values[a])

  # Use thresholds as names
  colnames(pred.map) <- round(thrs_values, 3)

  # If two thresholds are giving the same result reduce the burden
  pred.map <- pred.map[, !duplicated(t(pred.map))]

  # Get rid of thresholds that are keeping too few predictors
  pred.map <- pred.map[, colSums(pred.map) >= min.features]

  # Get rid of thresholds that are keeping too many predictors
  pred.map <- pred.map[, colSums(pred.map) <= max.features]

  # And update the effective number of the thresholds considered
  nthrs.eff <- ncol(pred.map)
  
  # Create an object to store k-fold cross-validation log-likelihoods
  map_kfcv <- array(
    dim = c(maxnpcs, nthrs.eff, K),
    dimnames = list(NULL, colnames(pred.map), NULL)
  )

  # Create a fold partitioning object
  part <- sample(rep(1:K, ceiling(nrow(ivs) / K)))[1:nrow(ivs)]

  # Loop over K folds
  for (k in 1:K) {
    # k <- 1

    # Create fold data:
    Xtr <- ivs[part != k, , drop = FALSE]
    Xva <- ivs[part == k, , drop = FALSE]
    ytr <- dv[part != k]
    yva <- dv[part == k]

    # Loop over threshold values
    for (thr in 1:nthrs.eff) {
      # thr <- 1
      # Define the active set of predictors based on the current threshold value
      aset <- pred.map[, thr]

      # If there is more than 1 active variable
      if (sum(aset) > 1) {

        # Scale Xs
        Xtr_thr <- scale(Xtr[, aset], center = TRUE, scale = TRUE)
        Xva_thr <- scale(Xva[, aset],
          center = attributes(Xtr_thr)$`scaled:center`,
          scale = attributes(Xtr_thr)$`scaled:scale`
        )

        # Perform PCA on the training data
        svd_Xtr <- svd(Xtr_thr)

        # Project training and validation data on the PCs
        PC_tr <- Xtr_thr %*% svd_Xtr$v
        PC_va <- Xva_thr %*% svd_Xtr$v

        # Check how many components are available (effective number)
        q.eff <- min(sum(aset), maxnpcs)

        # Select the available PC scores
        PC_tr.eff <- PC_tr[, 1:q.eff, drop = FALSE]
        PC_va.eff <- PC_va[, 1:q.eff, drop = FALSE]

        # Compute the F-statistic for the possible additive PCRs
        for (Q in 1:q.eff) {
          # Q <- 1

          # Train GLM model and baseline model
          glm_fit_tr <- glm(ytr ~ PC_tr.eff[, 1:Q], family = fam)

          # Store the baseline GLM model
          glm_null_tr <- glm(ytr ~ 1, family = fam)
          
          # Obtain prediction based on new data
          yhat_va <- cbind(1, PC_va.eff[, 1:Q]) %*% coef(glm_fit_tr)

          # Obtain validation residuals
          r_va_mod <- (yva - yhat_va)
          r_va_null <- yva - mean(ytr)

          # Store the estimate of the sigma
          s_va_mod <- sqrt(sum(resid(glm_fit_tr)^2) / (length(ytr))) # maximum likelihood version
          s_va_null <- sqrt(sum(resid(glm_null_tr)^2) / (length(ytr))) # maximum likelihood version

          # Compute validation data log-likelihood under the null model
          loglik_va_null <- loglike_norm(r = r_va_null, s = s_va_null)

          # Compute validation data log-likelihood under the model
          loglik_va_mod <- loglike_norm(r = r_va_mod, s = s_va_mod)

          # Extract desired statistic
          if (test == "F") {
            # Compute residuals
            Er <- TSS <- sum((yva - mean(ytr))^2) # baseline prediction error
            Ef <- SSE <- sum((yva - yhat_va)^2) # model prediction error

            # Compute degrees of freedom
            dfR <- (n - 0 - 1) # for the restricted model
            dfF <- (n - Q - 1) # for the full model

            # Compute the f statistic
            Fstat <- ((Er - Ef) / (dfR - dfF)) / (Ef / dfF)

            # Store the F stats
            map_kfcv[Q, thr, k] <- Fstat
          }
          if (test == "LRT") {
            map_kfcv[Q, thr, k] <- 2 * (loglik_va_mod - loglik_va_null)
          }
          if (test == "AIC") {
            map_kfcv[Q, thr, k] <- 2 * (Q + 1 + 1) - 2 * loglik_va_mod
          }
          if (test == "BIC") {
            map_kfcv[Q, thr, k] <- log(length(r_va_mod)) * (Q + 1 + 1) - 2 * loglik_va_mod
          }
          if (test == "PR2") {
            map_kfcv[Q, thr, k] <- 1 - exp(-2 / length(r_va_mod) * (loglik_va_mod - loglik_va_null))
          }
          if (test == "MSE") {
            map_kfcv[Q, thr, k] <- MLmetrics::MSE(y_pred = yhat_va, y_true = yva)
          }
        }
      }
    }
  }

  # Average selected score across folds
  scor.list <- average.scores(cv_array = map_kfcv, test = test)

  # Make a decision based on the CV measures
  cv_sol <- cv.choice(
    scor = scor.list$scor,
    scor.lwr = scor.list$scor.lwr,
    scor.upr = scor.list$scor.upr,
    K = K,
    test = test
  )

  # Return
  list(
    thr         = thrs_values,
    thr.cv      = thrs_values[cv_sol$default[2]],
    thr.cv.1se  = thrs_values[cv_sol$oneSE[2]],
    Q.cv        = cv_sol$default[1],
    Q.cv.1se    = cv_sol$oneSE[1],
    scor        = scor.list$scor,
    scor.lwr    = scor.list$scor.lwr,
    scor.upr    = scor.list$scor.upr,
    pred.map    = pred.map
  )

}

# helper functions -------------------------------------------------------------

# mean ignoring NAs
mean.na <- function(x) {
  mean(x[!is.na(x)])
}

# sd ignore NAs
se.na <- function(x) {
  val <- NA
  if (sum(!is.na(x)) > 0) {
    val <- sqrt(var(x[!is.na(x)]) / sum(!is.na(x)))
  }
  return(val)
}

# Special SVD function used by the superpc function
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

# Normal log-likelihood
loglike_norm <- function(r, s){
  # Define n based on the residuals
  n <- length(r)

  # Compute the log-likelihood
  -n / 2 * log(2 * pi) - n / 2 * log(s^2) - 1 / (2 * s^2) * sum(r^2)
}

# Averaging results from CV
average.scores <- function(cv_array, test) {
  # Description: given an array of npcs * thrsh * K dimensions, returns its average
  # Example internals:
  # - cv_array = array(abs(rnorm(10 * 3 * 2)), dim = c(10, 3, 2))
  # - test = "F"

  # How many folds?
  K <- tail(dim(cv_array), 1)

  # Average selected score across folds
  if (test == "F") {
    # Average the log for a more symmetrical scale
    lscor <- apply(log(cv_array), c(1, 2), mean, na.rm = FALSE)

    # Compute standard error for each
    lscor.sd <- apply(log(cv_array), c(1, 2), sd, na.rm = FALSE) / sqrt(K)

    # Revert to original scale and compute upper lower bounds
    scor <- exp(lscor)
    scor.upr <- exp(lscor + lscor.sd)
    scor.lwr <- exp(lscor - lscor.sd)
  } else {
    # Average normal results
    scor <- apply(cv_array, c(1, 2), mean, na.rm = FALSE)

    # Compute the standard errors
    scor.sd <- apply(cv_array, c(1, 2), sd, na.rm = FALSE) / sqrt(K)

    # Compute the upper and lower bounds
    scor.upr <- scor + scor.sd
    scor.lwr <- scor - scor.sd
  }

  # Return
  list(
    scor = scor,
    scor.upr = scor.upr,
    scor.lwr = scor.lwr
  )
}

# Extracting CV the choices
cv.choice <- function(scor, scor.lwr, scor.upr, K, test) {
  # Description: given an matrix of npcs * thrsh, returns the best choice based
  #              on the type of test (best overall and 1se rule versions)
  # Example internals:
  # scor = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  # test = "F"
  # K = 10
  # scor.lwr = matrix(c(1, 2, 3, 4, 5, 6) - 1.5, nrow = 3, ncol = 2)
  # scor.upr = matrix(c(1, 2, 3, 4, 5, 6) + 1.5, nrow = 3, ncol = 2)

  # Decide if you need the max or the min
  if (test == "F" | test == "LRT" | test == "PR2") {
    maxmin <- "max"
  }
  if (test == "AIC" | test == "BIC" | test == "MSE") {
    maxmin <- "min"
  }

  # Extract the max or min value in the matrix
  choice <- eval(parse(text = paste0(maxmin, "(scor, na.rm = TRUE)")))

  # Return the coordinates of the choice
  cv.default <- which(scor == choice, arr.ind = TRUE)

  # Reverse engineer the standard error of the decision CV
  cv.default.se <- (scor - scor.lwr)[cv.default]

  # Logical matrix storing which values bigger than sol - 1SE
  if (test == "F" | test == "LRT" | test == "PR2") {
    scor.s1se <- scor >= choice - cv.default.se
  }
  # Logical matrix storing which values smaller than sol + 1SE
  if (test == "MSE" | test == "BIC" | test == "AIC") {
    scor.s1se <- scor <= choice + cv.default.se
  }

  # Logical matrix excluding solution
  scor.ns <- scor != scor[cv.default]

  # Create a list of candidate models that are within 1 standard error of the best
  candidates <- which(scor.s1se & scor.ns, arr.ind = TRUE)

  # Attach value
  candidates <- cbind(candidates, values = scor[candidates[, 1:2]])

  # Are there such solutions?
  if (nrow(candidates) >= 1) {
    # Select the solutions with highest threshold (smallest number of predictors)
    candidates <- candidates[candidates[, "col"] == max(candidates[, "col"]), , drop = FALSE]

    # Select the solutions with lowest npcs (smallest number of components)
    candidates <- candidates[candidates[, "row"] == min(candidates[, "row"]), , drop = FALSE]

    # Select the solution with the smallest measure out of the candidate models
    cv.1se <- candidates[, -3]
  } else {
    cv.1se <- cv.default
  }

  return(
    list(
      default = cv.default,
      oneSE = cv.1se
    )
  )
}
