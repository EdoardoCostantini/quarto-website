# Project:   knowledgeBase
# Objective: function to estimate descriptives on data w/ miss fast R style
# Author:    Edoardo Costantini
# Created:   2021-12-02
# Modified:  2021-12-02
# Notion:
# Other ref:
# Note:      This function implements the EM psuedo code for normal data
#            with missing values in a general patter presented by
#            Schafer 1997 (figure 5.2 under section 5.3.3), but tries
#            to use R vectorisation to make the code faster and easier to read.

emFast <- function (Y, iters, theta0){
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
    R <- patts[-nrow(patts),-ncol(patts), drop = FALSE]
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
    #> E-step ####
    T <- Tobs
    if(S > 1){
      # We only need to do this if there are missing data patterns other than
      # the fully observed pattern (s = 1)
      for(s in 2:S){
        # Description: For every missing data patter, except the first one (complete data
        # missing data pattern)
        # s <- 3
        obs   <- I[[s]]
        v_obs <- O[[s]]
        v_mis <- M[[s]]
        v_all <- colnames(Y)

        # Sweep theta over predictors for this missing data pattern
        theta <- ISR3::SWP(theta, v_obs)

        # Define expectations (individual contributions)
        betas <- theta[c("int", v_obs), v_mis]
        cjs <- cbind(1, Y[obs, v_obs, drop = FALSE]) %*% betas

        # Update T matrix ##
        for(i in seq_along(obs)){
          for(j in seq_along( v_mis ) ){
            # j <- 1
            # Update for mean
            J <- which(v_all == v_mis[j])
            T[1, J+1] <- T[1, J+1] + cjs[i, j]
            T[J+1, 1] <- T[1, J+1]

            # Update for covariances w/ observed covariates for this id
            # (for Ks observed for this id)
            for(k in seq_along( v_obs )){
              # k <- 1
              K <- which(v_all == v_obs[k])
              T[K+1, J+1] <- T[K+1, J+1] + cjs[i, j] * Y[obs[i], K]
              T[J+1, K+1] <- T[K+1, J+1]
            }

            # Update for covariances w/ unobserved covariates for this id
            # (both j and k missing, includes covariances with itself k = j)
            for(k in seq_along( v_mis )){
              # k <- 1
              K <- which(v_all == v_mis[k])
              if(K >= J){
                T[K+1, J+1] <- T[K+1, J+1] + theta[K+1, J+1] + cjs[i, j] * cjs[i, k]
                T[J+1, K+1] <- T[K+1, J+1]
              }
            }
          }
        }
        theta <- ISR3::RSWP(theta, v_obs)
        # Note: this corresponds to the reverse sweep in the first
        # loop performed in the algorithm proposed by Schafer 1997.
        # It basically replaces the "if r_sj = 0 and theta_jj < 0".
        # For one E step, the covariance matrix used to compute individual
        # contirbutions in each missing data pattern is the same!
      }
    }

    # > M-step ####
    theta <- SWP((n^(-1) * T), 1)

  }

  return(theta)

}