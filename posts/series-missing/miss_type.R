### Title:    MCAR, MAR, and MNAR
### Author:   Edoardo Costantini
### Created:  2020-07-01
### Modified: 2021-07-17

  rm(list = ls())
  library(mvtnorm)
  library(mice)

# MAR types ---------------------------------------------------------------

  # Generate TRUE data
  set.seed(20200804)
  n     <- 1e4 # Sample Size
  p     <- 3
  pm    <- 0.5 # Proportion Missing
  Sigma <- matrix(.8, nrow = p, ncol = p)
    diag(Sigma) <- 1
  X <- as.data.frame( rmvnorm(n, rep(0, p), Sigma) )
  
  # Center 
  types <- c("LEFT", "RIGHT", "MID", "TAIL")
  mech <- c("MCAR", "MAR", "MNAR")[2]
  conds <- as.matrix(expand.grid(types = types, mech = mech))
  
  amp_list <- lapply(1:nrow(conds), function(i){
    X_amp <- ampute(X,
                    patterns = c(0, 1, 1),
                    mech = conds[i, "mech"], 
                    type = conds[i, "types"])
    return(X_amp$amp)
  })

  # Proabiblity of missing as a function of covariate plots

  par(mfrow = c(2, 2))
  plot_PM <- lapply(1:length(amp_list), function(id) {
    # Extract data
    X_temp <- amp_list[[id]]

    # Create vector of non-response indicator
    X_temp$WY <- is.na(X_temp[, 1])

    # Fit logistic regression model
    log_fit <- glm(WY ~ V2, data = X_temp, family = binomial)

    # Add predictions to the data
    X_temp$WY_hat <- predict(
      log_fit,
      type = "response"
    )

    # Sort data for plot based on values of V2
    X_temp <- X_temp[order(X_temp$V2), ]

    # Plot Predicted data and original data points
    plot(WY ~ V2, data = X_temp)
    lines(WY_hat ~ V2, X_temp, lwd = 2, col = "blue")
  })
  plot_PM

  # Plot density of MAR predictor for variable under imputation
  par(mfrow = c(2, 2))
  plot_list <- lapply(1:length(amp_list), function(id) {
    densityplot(~V2,
      data = amp_list[[id]],
      groups = factor(is.na(V1), labels = c("V1 observed", "V1 missing")),
      par.settings = list(superpose.line = list(lty = 1:2)),
      main = paste0(conds[id, "mech"], " ", conds[id, "types"]),
      auto.key = TRUE
    )
  })

# Effects of missingness --------------------------------------------------

  # Gen data  
  set.seed(20200804)
  n     <- 1e4 # Sample Size
  p     <- 4
  pm    <- 0.3 # Proportion Missing
  Sigma <- matrix(c(1.0, .8, .8, 0,
                    .8, 1.0, .8, 0,
                    .8, .8, 1.0, 0,
                    0,   0,   0,   1), ncol = p)
  Z <- as.data.frame( rmvnorm(n, rep(0, p), Sigma) )
  colnames(Z) <- paste0("Z", 1:p)
  
  # Create Copy of original data
  Z$Z1_mnar <- Z$Z1_mar <- Z$Z1_mcar <- Z$Z1
  Z$Z1_unr <- Z$Z1
  Z$Z1_mnar_ind <- Z$Z1
  Z$Z2_mnar_ind <- Z$Z2
  
  # Gen non-response vectors
  set.seed(1234)
  nR_mcar <- sample(c(TRUE, FALSE), n, 
                   prob = c(pm, 1-pm), # TRUE = missing
                   replace = TRUE)
  nR_mar  <- pnorm(Z$Z2) < pm # TRUE = missing
  nR_mnar <- pnorm(Z$Z1) < pm
  
  # Special cases:
  # 1.predictor of missigness is unrelated to target var
  nR_unr <- pnorm(Z$Z4) < pm
    cor(Z$Z1, Z$Z4)
  # 2. Circular MNAR
  nR_Z1 <- pnorm(Z$Z2) < pm # TRUE = missing
  nR_Z2 <- pnorm(Z$Z1) < pm # TRUE = missing
    # When I umpute Z2, I will use: 
    # - observed part of Z2
    # - observed part of Z1
    # - imputed part of Z1(!), that depends on Z2!
    # So the imputation of Z2, depends somehow on Z2.
    # Missingness on Z2 still does not depend on Z2,
    # but the response model I'm using does not reflect this.
    # Remember that MNAR is in the imputation model, not the data.
    
  # 3. Inderect MNAR
  nR_Z1 <- pnorm(Z$Z2) < pm # TRUE = missing
  nR_Z2 <- pnorm(Z$Z3) < pm # TRUE = missing
  
  # Impose Missingness
  Z$Z1_mcar[nR_mcar] <- NA
  Z$Z1_mar[nR_mar]   <- NA
  Z$Z1_mnar[nR_mnar] <- NA
  Z$Z1_unr[nR_unr]   <- NA
  Z$Z1_mnar_ind[nR_Z1] <- NA
  Z$Z2_mnar_ind[nR_Z2] <- NA
  
  # Comapre fully observed and miss variables
  # Densities
  plot(density(Z$Z1),
       main = "Z1 Density Plot", 
       xlab = "score", ylab = "", ylim = c(0, .75), col = "gray")
    lines(density(Z$Z1_mcar, na.rm = TRUE), col = "blue", lwd = 2)
    lines(density(Z$Z1_mar, na.rm = TRUE), col = "darkorange", lwd = 2)
    lines(density(Z$Z1_mnar, na.rm = TRUE), col = "darkgreen", lwd = 2)
    lines(density(Z$Z1_unr, na.rm = TRUE), col = "yellow", lty = 3, lwd = 2)
    
  # Correlations
  round(
    c(FULL = cor(Z$Z1, Z$Z2),
      MCAR = cor(Z$Z1_mcar, Z$Z2, use = "pairwise"),
      UNR = cor(Z$Z1_unr, Z$Z2, use = "pairwise"),
      MAR  = cor(Z$Z1_mar, Z$Z2, use = "pairwise"),
      MNAR = cor(Z$Z1_mnar, Z$Z2, use = "pairwise")),
    3
  )

# Imputing w/ MI ----------------------------------------------------------

  # Create an empty predictor matrix for mice to perform MAR, MNAR etc imps
  predMat <- matrix(0, 
                    nrow = ncol(Z), ncol = ncol(Z),
                    dimnames = list(colnames(Z), colnames(Z)))
  md_vec <- c(Z1 = "", 
              Z2 = "", 
              Z3 = "", 
              Z4 = "",
              Z1_mcar = "",
              Z1_mar  = "",
              Z1_mnar = "",
              Z1_unr  = "",
              Z1_mnar_ind = "",
              Z2_mnar_ind = "")
    
  # MAR
  mar_md_vec <- md_vec
    mar_md_vec["Z1_mar"] <- "norm"
    
  mar_predMat <- predMat
    mar_predMat["Z1_mar", c("Z2")] <- 1
    
  MI_MAR <- mice(data = Z, m = 10, maxit = 1,
                 predictorMatrix = mar_predMat,
                 method = mar_md_vec)
  MI_MAR_dt <- complete(MI_MAR, "all")
  
  # MNAR (simple, univariate)
  mnar_md_vec <- md_vec
    mnar_md_vec["Z1_mnar"] <- "norm"
    
  mnar_predMat <- predMat
    mnar_predMat["Z1_mnar", c("Z2")] <- 1
    
  MI_MNAR <- mice(data = Z, m = 10, maxit = 1,
                 predictorMatrix = mnar_predMat,
                 method = mnar_md_vec)
  MI_MNAR_dt <- complete(MI_MNAR, "all")
    
  # Plots
  par(mfrow = c(1,1))
  plot(density(Z$Z1), col = "black",
       main = "Density",
       xlab = "Z1", ylab = "",
       ylim = c(0,.6), lwd = 3)
  # MAR: MI fixes the missingness
  lines(density(Z$Z1_mar, na.rm = TRUE), col = "darkorange", lwd = 3)
  lapply(MI_MAR_dt, function(x) lines(density(x$Z1_mar), 
                                      lty = 2,
                                      col = "darkorange")
  )
  # MNAR: MI does not fix the missingness
  lines(density(Z$Z1_mnar, na.rm = TRUE), col = "blue", lwd = 3)
  lapply(MI_MNAR_dt, function(x) lines(density(x$Z1_mnar), 
                                       lty = 2,
                                       col = "blue")
  )
  legend(-4, .5,
         legend = c("Z1", "Z1 MAR", "Z1 MAR imputed", "Z1 MNAR", "Z1 MNAR imputed"),
         col = c("black", "darkorange", "darkorange", "blue", "blue"), 
         lwd = c(3, 3, 1, 3, 1),
         lty = c(1, 1, 2, 1, 2), 
         cex = 1)
    
  # Multivariate (Inderect MNAR on Z1)
  mnar_md_vec <- md_vec
    mnar_md_vec[c("Z1_mnar_ind", "Z2_mnar_ind")] <- "norm"
  
  mnar_predMat <- predMat
    mnar_predMat["Z2_mnar_ind", "Z3"] <- 1
    mnar_predMat["Z1_mnar_ind", "Z2_mnar_ind"] <- 1
  
  MI_MNAR_mv <- mice(data = Z, m = 10, maxit = 1,
                  predictorMatrix = mnar_predMat,
                  method = mnar_md_vec)
  MI_MNAR_mv_dt <- complete(MI_MNAR_mv, "all")
  
  # Plot
  par(mfrow = c(1, 2) )
  plot(density(Z$Z1),
       main = "Z1 w/ MNAR mechanism",
       xlab = "", ylab = "",
       ylim = c(0,.6), lwd = 3)
  lines(density(Z$Z1_mnar_ind, na.rm = TRUE), col = "darkorange", lwd = 3)
  lapply(MI_MNAR_mv_dt, function(x) lines(density(x$Z1_mnar_ind), 
                                          col = "darkorange",
                                          lty = 2)
  )
  legend(-4, .6,
         legend = c("Z1", "Z1 inderect MNAR", "Z1 MNAR imputed"),
         col = c("black", "darkorange", "darkorange"), 
         lwd = c(3, 3, 1),
         lty = c(1, 1, 2), 
         cex = 1)
  
  plot(density(Z$Z2),
       main = "Z2 w/ MAR mechanism",
       xlab = "", ylab = "",
       ylim = c(0,.6), lwd = 3)
  lines(density(Z$Z2_mnar_ind, na.rm = TRUE), col = "darkorange", lwd = 3)
  lapply(MI_MNAR_mv_dt, function(x) lines(density(x$Z2_mnar_ind), 
                                          col = "darkorange",
                                          lty = 2)
  )
  legend(-4, .6,
         legend = c("Z2", "Z2 MAR", "Z2 MAR imputed"),
         col = c("black", "darkorange", "darkorange"), 
         lwd = c(3, 3, 1),
         lty = c(1, 1, 2), 
         cex = 1)
