# Project:   knowledgeBase
# Objective: function to estimate descriptives on data w/ miss fast R style
# Author:    Edoardo Costantini
# Created:   2021-12-02
# Modified:  2022-03-30
# Notion:
# Other ref:
# Note:      This function implements the EM psuedo code for normal data
#            with missing values in a general patter presented by
#            Schafer 1997 (figure 5.2 under section 5.3.3), but tries
#            to use R vectorisation to make the code faster and easier to read.

emWeight <- function (Y, wi, iters, theta0){
  ## Example Inputs
  # Y = Y
  # wi <- runif(nrow(Y))
  # iters = 5e2
  # theta0 = matrix(rep(NA, (p+1)^2 ), ncol = (p+1),
  #                 dimnames = list(c("int", colnames(Y)),
  #                                 c("int", colnames(Y))
  #                                 ))
  # theta0[, 1]   = c(-1, colMeans(Y, na.rm = TRUE)) # T1 CC
  # theta0[1, ]   = c(-1, colMeans(Y, na.rm = TRUE))
  # theta0[-1,-1] = cov(Y, use = "pairwise.complete.obs") * (n - 1)/n # T2 CC

# > Preliminaries --------------------------------------------------------------

  # Define Missing data patterns
  patts <- mice::md.pattern(Ymiss, plot = FALSE)
    R <- patts[-nrow(patts),-ncol(patts), drop = FALSE]
    R <- R[, colnames(Ymiss), drop = FALSE]

  # Data dimensionality
  n <- nrow(Ymiss)

  # Number of missing data patterns
  S <- nrow(R)

  # Columns observed for a given pattern
  O <- apply(R, 1, function(x) {colnames(R)[x == 1]})

  # Columns missings for a given pattern
  M <- apply(R, 1, function(x) {colnames(R)[x == 0]})

  # Define I matrices (which obs in which pattern)
  ry <- !is.na(Ymiss)
  R_logi <- R == 1 # pattern config saved as True and False
  I <- vector("list", S)
  for (s in 1:S) {
    # s <- 1
    index <- NULL
    for (i in 1:n) {
      # i <- 1
      if(all.equal(ry[i, ], R_logi[s, ]) == TRUE) {
        index <- c(index, i)
      }
    }
    I[[s]] <- index
  }

  # Define sufficient statistics matrix (observed)
  Tobs_s <- vector("list", S)

  for(s in 1:S){
    # Define what you are working with in this miss patt
    Y_s <- Ymiss[I[[s]], , drop = FALSE]
    wi_s <- wi[I[[s]]]

    # Weigthed augmented design matrix
    dat_aug <- as.matrix(sqrt(wi_s) * cbind(1, Y_s))

    # Obtain matrix of sufficient statistics (Tobs) w/ cross-product shortcut
    Tobs_s[[s]] <- t(dat_aug) %*% dat_aug

    # Replace NAs with 0 contributions
    Tobs_s[[s]][is.na(Tobs_s[[s]])] <- 0
  }

  Tobs <- Reduce("+", Tobs_s)
  dimnames(Tobs) <- dimnames(theta0)

# > EM algorithm ---------------------------------------------------------------

  # Starting values
  theta <- theta0
  ith_theta <- NULL

  # Iterations
  for(it in 1:iters){
    print(it)

    # Reset T to info in the data
    Tmat <- Tobs

    # > E step -----------------------------------------------------------------

    for(s in 2:S){
      # For every missing data patter, add exceptation
      # s <- 2
      obs   <- I[[s]]
      v_obs <- O[[s]]
      v_mis <- M[[s]]
      v_all <- colnames(Ymiss)

      # Sweep theta over predictors for this missing data pattern
      theta <- ISR3::SWP(theta, v_obs)

      # Define expectations (individual contributions)
      betas <- theta[c("int", v_obs), v_mis]
      cjs <- cbind(1, Ymiss[obs, v_obs, drop = FALSE]) %*% betas

      # Update Tmat matrix ##
      for(i in seq_along(obs)){
        # i <- 1
        for(j in seq_along( v_mis ) ){
          # j <- 1
          # Update for mean
          J <- which(v_all == v_mis[j])
          Tmat[1, J+1] <- Tmat[1, J+1] + cjs[i, j] * wi[obs[i]]
          Tmat[J+1, 1] <- Tmat[1, J+1]

          # Update for covariances w/ observed covariates for this id
          # (for Ks observed for this id)
          for(k in seq_along( v_obs )){
            # k <- 1
            K <- which(v_all == v_obs[k])
            Tmat[K+1, J+1] <- Tmat[K+1, J+1] + cjs[i, j] * Ymiss[obs[i], K] * wi[obs[i]]
            Tmat[J+1, K+1] <- Tmat[K+1, J+1]
          }

          # Update for covariances w/ unobserved covariates for this id
          # (both j and k missing, includes covariances with itself k = j)
          for(k in seq_along( v_mis )){
            # k <- 1
            K <- which(v_all == v_mis[k])
            if(K >= J){
              Tmat[K+1, J+1] <- Tmat[K+1, J+1] + (theta[K+1, J+1] + cjs[i, j] * cjs[i, k]) * wi[obs[i]]
              Tmat[J+1, K+1] <- Tmat[K+1, J+1]
            }
          }
        }
      }
      theta <- ISR3::RSWP(theta, v_obs)
    }

    # > M step -----------------------------------------------------------------

    ith_theta[[it]] <- ISR3::SWP((sum(wi)^(-1) * Tmat), 1)
    theta <- ith_theta[[it]]
  }

  return(theta)

}