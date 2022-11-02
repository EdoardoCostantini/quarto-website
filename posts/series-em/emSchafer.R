# Project:   knowledgeBase
# Objective: function to estimate descriptives on data w/ miss Schafer style
# Author:    Edoardo Costantini
# Created:   2021-12-02
# Modified:  2021-12-02
# Notion:
# Other ref:
# Note:      This function implements (almost) exactly the psuedo code
#            Schafer 1997 presents in figure 5.2 under section 5.3.3
#            One key difference is in how I achive simmetry.

emSchafer <- function (Y, iters, theta0){
  ## Example Inputs
  # Y = Y
  # iters = 5e2
  # theta0 = matrix(rep(NA, (p+1)^2 ), ncol = (p+1),
  #                 dimnames = list(c("int", colnames(Y)),
  #                                 c("int", colnames(Y))
  #                                 ))
  # theta0[, 1]   = c(-1, colMeans(Y, na.rm = TRUE)) # T1 CC
  # theta0[1, ]   = c(-1, colMeans(Y, na.rm = TRUE))
  # theta0[-1,-1] = cov(Y, use = "pairwise.complete.obs") * (n - 1)/n # T2 CC

  ## Preliminaries:

  # Define Missing data patterns
  patts <- mice::md.pattern(Y, plot = FALSE)
    R <- patts[-nrow(patts), -ncol(patts), drop = FALSE]
    R <- R[, colnames(Y), drop = FALSE]

  # Data dimensionality
  n <- nrow(Y)

  # Number of missing data patterns
  S <- nrow(R)
  # Columns observed for a given pattern
  O <- apply(R, 1, function(x) {colnames(R)[x == 1]})
  # Columns missings for a given pattern
  M <- apply(R, 1, function(x) {colnames(R)[x == 0]}) #

  # Define I matrices (which obs in which pattern)
  ry <- !is.na(Y)
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

  for (s in 1:S) {
    id_obs  <- I[[s]]
    dat_aug <- as.matrix(cbind(int = 1, Y[id_obs, , drop = FALSE]))
    Tobs_s[[s]] <- crossprod(dat_aug)

    # Fix NAs
    Tobs_s[[s]][is.na(Tobs_s[[s]])] <- 0
  }

  Tobs <- Reduce("+", Tobs_s)

  ## EM algorithm
  # Starting values
  theta <- theta0

  # Iterations
  for(it in 1:iters){
    # Reset T to info in the data
    # (will be updated based on new theta at every new iteration)
    T <- Tobs

    #> E-step ####
    for(s in 2:S){
      # Description: For every missing data patter, except the first one (complete data
      # missing data pattern)
      # s <- 2
      obs   <- I[[s]]
      v_obs <- O[[s]]
      v_mis <- M[[s]]
      v_all <- colnames(Y)

      # Sweep theta over predictors for this missing data pattern
      for(j in 1:p){
        if(v_all[j] %in% v_obs & theta[j + 1, j + 1] > 0){
          theta <- ISR3::SWP(theta, j+1)
        }
        if(v_all[j] %in% v_mis & theta[j + 1, j + 1] < 0){
          theta <- ISR3::RSWP(theta, j+1)
        }
      }

      # Updating T matrix ##
      # Computing individual contributions (expectations)
      for(i in seq_along(obs)){
        # Description: For every individual in this missing data pattern
        cj <- rep(NA, p)

        for(j in seq_along( v_mis ) ){
          # j <- 1
          J <- which(v_all == v_mis[j])

          cj[J] <- theta[1, J+1] # a_0j, intercept for missing variable j of M(s)

          for(k in seq_along( v_obs )){
            # k <- 1
            K <- which(v_all == v_obs[k])
            b <- theta[K+1, J+1] # regression coefficient for observed variables K in predicting missing variable J
            cj[J] <- cj[J] + b * Y[obs[i], K] # dataset value for individual i, observed in variable K
          }
        }

        for(j in seq_along( v_mis ) ){
          # j <- 1
          J <- which(v_all == v_mis[j])
          T[1, J+1] <- T[1, J+1] + cj[J]
          T[J+1, 1] <- T[1, J+1]

          for(k in seq_along( v_obs )){
            # k <- 1
            # adding expectation of y_ij * y_ik when
            # k is observed for this iID
            K <- which(v_all == v_obs[k])
            T[K+1, J+1] <- T[K+1, J+1] + cj[J] * Y[obs[i], K]
            T[J+1, K+1] <- T[K+1, J+1]
          }

          for(k in seq_along( v_mis )){
            # k <- 1
            # adding expectation of y_ij * y_ik when
            # both j and k are unobserved for this iID
            K <- which(v_all == v_mis[k])
            if(K >= J){
              T[K+1, J+1] <- T[K+1, J+1] + theta[K+1, J+1] + cj[J]*cj[K]
              T[J+1, K+1] <- T[K+1, J+1]
            }
          }
        }
      }
    }

    # > M-step ####
    theta <- SWP((n^(-1) * T), 1)

  }

  return(theta)

}
