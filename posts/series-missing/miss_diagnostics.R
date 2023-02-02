### Title:    Missing data mechanisms diagnostics
### Author:   Edoardo Costantini
### Created:  2021-07-18
### Modified: 2021-07-18

  rm(list = ls())
  library(mvtnorm)
  library(mice)
  library(lattice)

# MAR types ---------------------------------------------------------------

  # Generate TRUE data
  set.seed(20200804)
  n     <- 1e3 # Sample Size
  p     <- 3
  pm    <- 0.5 # Proportion Missing
  Sigma <- matrix(.8, nrow = p, ncol = p)
  diag(Sigma) <- 1
  X <- as.data.frame( rmvnorm(n, rep(0, p), Sigma) )
  
  # Impose missing data
  types <- c("LEFT", "RIGHT", "MID", "TAIL")[2]
  mech <- c("MCAR", "MAR", "MNAR")
  conds <- as.matrix(expand.grid(types = types, mech = mech))
  my_patterns <- matrix(c(0, 1, 0,
                          1, 0, 1), nrow = 2, byrow = TRUE)
  
  amp_list <- lapply(1:nrow(conds), function(i){
    X_amp <- ampute(X, 
                    prop = pm,
                    mech = conds[i, "mech"], 
                    type = conds[i, "types"])
    return(X_amp$amp)
  })
  
  # Perform Imputation
  imp_predmat <- mice(amp_list[[1]], maxit = 0)
  predmat <- imp_predmat$predictorMatrix
  predmat[, 3] <- 0
  imp_list <- lapply(1:nrow(conds), function(i){
    print(i)
    X_mids <- mice(amp_list[[i]], 
                   predictorMatrix = predmat,
                   m = 5,
                   maxit = 50,
                   print = FALSE)
    return(X_mids)
  })
  lapply(imp_list, plot)

# Flags -------------------------------------------------------------------

  # Density plots V2 density for observed and missing V1
  plot_list <- lapply(1:length(amp_list), function(id){
    densityplot(~ V1, data = amp_list[[id]],
                groups =  factor(is.na(V2), labels = c("V2 observed", "V2 missing")),
                par.settings = list(superpose.line = list(lty = 1:2)),
                main = paste0(conds[id, "mech"], " ", conds[id, "types"]),
                auto.key = TRUE)
  })
  plot_list

  # Density plots of imputed and observed data for a given variable
  lapply(imp_list, densityplot, xlim = c(-4, 4), ylim = c(0, .75))
  lapply(imp_list, bwplot, ylim = c(-4, 4))
  