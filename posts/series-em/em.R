# Project:   blogdown
# Objective: Explore a simple case of EM algorithm with missing values
# Author:    Edoardo Costantini
# Created:   2021-11-17
# Modified:  2022-07-04
# Notion:
# Other ref:

rm(list = ls())

# Packages ----------------------------------------------------------------

  library(mvtnorm)           # for likelihood functions
  library(norm)              # for alternative EM implementation
  library(ISR3)              # for SWP functions
  library(fastmatrix)        # alternative sweep
  library(mice)              # for NA pattern assesment
  library(rbenchmark)        # to benchmark code
  source("emSchafer.R") # main sweep function
  source("emFast.R")    # main sweep function
  source("emWeight.R")  # main sweep function

# Likelihood summary ------------------------------------------------------

# Review a few concepts you might need
  # Given some dataset (e.g. Little Rubin 2002 example 7.7, p. 152)
    Y_full <- matrix(c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10,
                       26, 29, 56, 31, 52, 55, 71 ,31, 54, 47,40,66,68,
                       6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8,
                       60,52, 20, 47, 33, 22,6,44,22,26,34,12,12,
                       78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,
                       115.9,83.8,113.3,109.4), ncol = 5)
    n <- nrow(Y_full)
    mu <- colMeans(Y_full) # sample, but imagine it's population
    Sigma <- cov(Y_full)   # sample, but imagine it's population
    y_i_1ton <- mvtnorm::rmvnorm(n, mu, Sigma) # y_i ~ MN(mu, Sigma)

  # Single row likelihood
    lkl_rowi <- det(2*pi*Sigma)^(-1/2) * exp((-1/2)*t(Y_full[2, ] - mu) %*% solve(Sigma) %*% (Y_full[2, ] - mu)) # for i = 2
    mvtnorm::dmvnorm(Y_full[2,], mu, Sigma) # same
    log(lkl_rowi)

  # Complete-data likelihood
    log(det(Sigma)^(-n/2) * exp((-1/2)* sum(diag((t(t(Y_full)-mu) %*% solve(Sigma)) %*% (t(Y_full)-mu)))))
    # disregarding the proportionality constant
    lkl_comp <- sum(dmvnorm(Y_full, mu, Sigma))
    log(lkl_comp)

  # Maximum-likelihood estiamtes
    # Define sufficient statistics T1 and T2
    T1 <- colSums(Y_full)
    T2 <- t(Y_full) %*% Y_full # cross-product matrix

    # Re-write complete-data loglikelihood based on sufficient statistics
    - n/2*log(det(Sigma)) - n/2*t(mu)%*%solve(Sigma)%*%mu + t(mu)%*%solve(Sigma)%*%T1 - 1/2*sum(diag(solve(Sigma)%*%T2))

    # Estimates
    y_bar <- n^(-1) * T1
    S <- n^(-1) * (t(Y_full)-y_bar) %*% t(t(Y_full)-y_bar)

# Datasets ----------------------------------------------------------------

# Multivariate Monotone example 7.7 in Little Rubin 2002 (p.153) ####
  dat_mm <- matrix(data = c(7,1, 11, 11, 7, 11, 3, 1, 2, NA, NA, NA, NA,
                            26,29, 56, 31, 52, 55, 71 ,31, 54, NA,NA,NA,NA,
                            6,15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8,
                            60,52, 20, 47, 33, 22,NA,NA,NA,NA,NA,NA,NA,
                            78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,
                            93.1,115.9,83.8,113.3,109.4),
                   ncol = 5,
                   dimnames = list(NULL, c("X1", "X2", "X3", "X4", "X5"))
  )

  # NAs pattern
  md.pattern(dat_mm)

# Multivariate General pattern example 5.3.7 in Schafer 1997 (p.230) ####
  dat_mg <- matrix(data = c(16,20,16,20,-6,-4, 12,24,12,-6,4,-8,
                            8,8,26,-4,4,8, 20,8,NA,NA,20,-4,
                            8,4,-8,NA,22,-8, 10,20,28,-20,-4,-4,
                            4,28,24,12,8,18, -8,20,24,-3,8,-24,
                            NA,20,24,8,12,NA),
                   ncol = 6,
                   byrow = TRUE,
                   dimnames = list(NULL, c("15P","15L","15H", "90P", "90L", "90H")))

  # NAs pattern
  md.pattern(dat_mg)

# Prepare data ------------------------------------------------------------

  # EM Estimate of descriptives for data with missing values
  Y <- dat_mg # chose dataset here
  n <- nrow(Y)
  p <- ncol(Y)

  # Starting value
  theta0 <- matrix(rep(NA, (p+1)^2 ), ncol = (p+1),
                  dimnames = list(c("int", colnames(Y)),
                                  c("int", colnames(Y))
                  ))
  theta0[, 1]   <- c(-1, colMeans(Y, na.rm = TRUE)) # T1 CC
  theta0[1, ]   <- c(-1, colMeans(Y, na.rm = TRUE))
  theta0[-1,-1] <- cov(Y, use = "pairwise.complete.obs") * (n - 1)/n # T2 CC

  # Run function

  # Manual slow
  theta_hat <- emSchafer(Y = Y, iters = 500, theta0 = theta0)

  # Manual fast
  theta_hat_f <- emFast(Y = Y, iters = 500, theta0 = theta0)
  
  # norm package version
  s <- prelim.norm(Y)     # do preliminary manipulations
  thetahat <- em.norm(s, maxits = 500) # compute mle
  getparam.norm(s, thetahat, corr = TRUE)$r
  getparam.norm(s, thetahat)$mu
  getparam.norm(s, thetahat, corr = TRUE)$sdv

  # Assess results

  # Means
  data.frame(
    cc = colMeans(Y, na.rm = TRUE),
    Schafer = drop(theta_hat[1, -1]),
    Fast = drop(theta_hat_f[1, -1]),
    em.norm = getparam.norm(s, thetahat)$mu
  )

  # Standard deviations
    data.frame(
      cc = apply(Y, 2, sd, na.rm = TRUE),
      Schafer = sqrt(diag(theta_hat)[-1]),
      Fast = sqrt(diag(theta_hat_f)[-1]),
      em.norm = getparam.norm(s, thetahat, corr = TRUE)$sd
    )

  # Covariance matrix
  round(theta_hat_f[-1, -1] - theta_hat[-1, -1], 3)
  round(theta_hat[-1, -1] - getparam.norm(s, thetahat)$sigma, 3)
  theta0[-1, -1] - getparam.norm(s, thetahat)$sigma

  # Correlation
  round(cov2cor(theta_hat[-1, -1]) - getparam.norm(s, thetahat, corr = TRUE)$r, 3)

  # Compare speed
  benchmark(
    "Scahfer" = { emSchafer(Y = Y, iters = 500, theta0 = theta0) },
    "Fast" = { emFast(Y = Y, iters = 500, theta0 = theta0) },
    "em.norm" = { em.norm(s, maxits = 500) },
    replications = 10,
    columns = c("test", "replications", "elapsed",
                "relative", "user.self", "sys.self")
  )

# RNG data ---------------------------------------------------------------------

  set.seed(20220327)
  n <- 1e4
  p <- 3
  Sig <- matrix(rep(1.5, p*p), ncol = p)
    diag(Sig) <- 3
  Y <- MASS::mvrnorm(n, rep(10, p), Sig)
  colnames(Y) <- paste0("y", 1:p)

# Estimate covmat on the original data

  cov_og <- cov(Y) * (n-1) / n # ML version
  muv_og <- colMeans(Y)

# Impose missingness

  amputeY <- mice::ampute(Y,
                          prop = .5,
                          patterns = matrix(c(1, 1, 0,
                                              1, 0, 1,
                                              1, 0, 0), ncol = 3, byrow = TRUE),
                          mech = "MAR",
                          type = "RIGHT"
  )

  Ymiss <- as.matrix(amputeY$amp)

# Estimate covmat on the listwise deletion data

  cov_cc <- cov(Ymiss, use = "complete.obs") * (n-1) / n # ML version
  muv_cc <- colMeans(Ymiss, na.rm = TRUE)

# Estimate covmat with EM
  # Give a bad starting value
  theta0 <- matrix(rep(NA, (p+1)^2 ), ncol = (p+1),
                   dimnames = list(c("int", colnames(Y)),
                                   c("int", colnames(Y))
                   ))
  theta0[, 1]   <- c(-1, rep(0, p)) # T1 CC
  theta0[1, ]   <- c(-1, rep(0, p))
  theta0[-1,-1] <- diag(p)

  # Run function
  theta_hat <- emSchafer(Y = Ymiss, iters = 50, theta0 = theta0)
  theta_hat_f <- emFast(Y = Ymiss, iters = 50, theta0 = theta0)

  # Extract results
  cov_EM <- theta_hat_f[-1, -1]
  muv_EM <- theta_hat_f[-1, 1]

# Compare results

  # function to simplify comparison between covmats
  vectomat <- function (comat){
          names_vec <- unlist(lapply(rownames(comat), function (i){
            paste0(i, "-", rownames(comat))
          }))
          comat_vec <- as.vector(comat)
          names(comat_vec) <- names_vec
          # Only unique
          return(comat_vec[!duplicated(comat_vec)])
        }

  # compare covmats
  cbind(OG = vectomat(cov_og),
        cc = vectomat(cov_cc),
        EM = vectomat(cov_EM),
        diff_cc = round(vectomat(cov_og) - vectomat(cov_cc), 3),
        diff_EM = round(vectomat(cov_og) - vectomat(cov_EM), 3)
  )

# RNG data: weighted version ---------------------------------------------------

# Generate data

  # Set seed
  set.seed(20220327)

  # Define data generation model parameters
  n <- 1e4
  p <- 3
  Sig <- matrix(rep(1.5, p*p), ncol = p)
  diag(Sig) <- 3


  # Sample data
  Y <- as.data.frame(MASS::mvrnorm(n, rep(10, p), Sig))

  # Fix column names
  colnames(Y) <- paste0("y", 1:p)

  # Devide in groups
  nstates <- 4
  Y$state <- factor(rep(1:nstates, n/nstates))

  # Create different variances per group
  Y[Y$state == 1, c(1:3)] <- Y[Y$state == 1, c(1:3)] * 2
  Y[Y$state == 2, c(1:3)] <- Y[Y$state == 1, c(1:3)] * .5
  Y[Y$state == 3, c(1:3)] <- Y[Y$state == 1, c(1:3)] * 1.5

  # New true covariance matrix
  cov(Y[, 1:3])

  # Sample different sizes of groups
  Ys <- NULL
  sample_sizes <- c(.3, .5, .8, .2)
  for (i in 1:nstates){
    subsamp_index <- sample(1:(n/nstates), n/nstates * sample_sizes[i])
    Ys[[i]] <- Y[Y$state == i, ][subsamp_index, ]
    Ys[[i]]$wi <- sample_sizes[i]
  }
  Ys <- do.call(rbind, Ys)

  # Ww need cov.wt to get a good estimate of the population covariance matrix
  cov(Y[, 1:3])
  cov(Ys[, 1:3])
  cov.wt(Ys[, 1:3], wt = 1/(Ys$wi))$cov

  # Impose missingness at random
  amputeY <- mice::ampute(Ys[, 1:3],
                          prop = .5,
                          patterns = matrix(c(1, 1, 0,
                                              1, 0, 1,
                                              1, 0, 0), ncol = 3, byrow = TRUE),
                          mech = "MAR",
                          type = "RIGHT"
  )
  Ymiss <- as.matrix(amputeY$amp)

  # Define wegihts
  wi <- 1/Ys$wi

# Estimate covmat on the original data

  covw_og <- cov.wt(Ys[, 1:3], wt = wi, method = "ML")$cov
  muvw_og <- cov.wt(Ys[, 1:3], wt = wi, method = "ML")$center

# Estiamte covmat on listwise deletion
  cc_index <- rowSums(is.na(Ymiss)) == 0
  covw_cc <- cov.wt(Ymiss[cc_index, ], wt = wi[cc_index], method = "ML")$cov
  muvw_cc <- cov.wt(Ymiss[cc_index, ], wt = wi[cc_index], method = "ML")$center

# Estimate covmat with EM

  # Bad starting theta
  theta0 = matrix(rep(NA, (p+1)^2 ), ncol = (p+1),
                  dimnames = list(c("int", colnames(Ymiss)),
                                  c("int", colnames(Ymiss))
                  ))
  theta0[, 1]   = c(-1, rep(5, p)) # T1 CC
  theta0[1, ]   = c(-1, rep(5, p))
  theta0[-1,-1] = diag(p)

  # Estimate with EM not weighted
  theta_nw <- emFast(Y = Ymiss, iters = 10, theta0 = theta0)
  # theta_nw <- emWeight(Y = Ymiss, wi = rep(1, p), iters = 25, theta0 = theta0)
  covnw_EM <- theta_nw[-1, -1]
  muvnw_EM <- theta_nw[-1, 1]

  # Estimate with EM and actual weights
  theta_w  <- emWeight(Y = Ymiss, wi = wi, iters = 25, theta0 = theta0)
  covw_EM <- theta_w[-1, -1]
  muvw_EM <- theta_w[-1, 1]

# Comparison

  # The weighted version
  cbind(pop = c(colMeans(Y[, 1:3]), vectomat(cov(Y[, 1:3]))),
        sample_OGw = c(muvw_og, vectomat(covw_og)),
        sample_EMw = c(muvw_EM, vectomat(covw_EM)),
        sample_ccw = c(muv_cc, vectomat(covw_cc)),
        sample_OG = c(colMeans(Ys[, 1:3]), vectomat(cov(Ys[, 1:3]))),
        sample_EM = c(muvnw_EM, vectomat(covnw_EM)),
        diff_cc = round(c(muvw_og, vectomat(covw_og)) -
                          c(muv_cc, vectomat(covw_cc)), 3),
        diff_EMw = round(c(muvw_og, vectomat(covw_og)) -
                           c(muvw_EM, vectomat(covw_EM)), 3)
  )