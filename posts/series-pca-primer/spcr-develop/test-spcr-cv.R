# Project:   quarto-website
# Objective: Compare different versions of corss-validation for SPCR
# Author:    Edoardo Costantini
# Created:   2023-03-16
# Modified:  2023-03-16
# Notes: 

# Load packages ----------------------------------------------------------------

library(superpc)
library(reshape2)
library(ggplot2)
library(PCovR)
library(nFactors)
library(patchwork)
library(dplyr)

# Load custom functions --------------------------------------------------------

source("funs-data-gen.R")
source("funs-spcr-cv.R")

# Generate data ----------------------------------------------------------------

# Define the desired number of components for X
Q <- 5

# Define the desired number of components predicting y
Qy <- 2

# Desired sample size
N <- 1e3

# Desired number of variables
P <- 50

# Sed seed
set.seed(2026)

# Generate data
XTP <- generateXTP(
    I <- N, # sample size
    J <- P, # number of variables
    VAFr <- diff(seq(0, 1, len = Q + 1)), # relative variance of each components
    VAFsum <- 100, # total variance of the components
    CPVE <- 0.3 # proportion of explained variance by the R components
)
X <- XTP$X

# Compute PCA with prcomp
PCX <- prcomp(X)

# Extract eigenvalues
eigenvalues <- PCX$sdev^2

# Cumulative proportion of explained variance
cumsum(prop.table(eigenvalues))[1:Q]

# Non-graphical solutions
nScree(x = eigenvalues)

# Screeplot
plotuScree(x = eigenvalues)

# # # Attach junk predictors to X?
# X <- cbind(X, junk = matrix(rnorm(N * P), nrow = N, ncol = P))

# generate DV based on the component scores
y <- generateDV(
    X = XTP$T[, 1:Qy, drop = FALSE],
    R2 = 0.80,
    beta = 1
)

# Save example datasets

GSPCRexdata <- data.frame(y = y, X)
saveRDS(GSPCRexdata, "./GSPCRexdata.rds")

# Check the model is as expected
summary(lm(y ~ -1 + XTP$T))

# Load PCovR data (alternative data set) ---------------------------------------

# # Load data
# data(alexithymia)

# # Define X
# X <- alexithymia$X

# # Define y
# y <- alexithymia$Y[, 2]

# Perform PCA for checks -------------------------------------------------------

# PCA
PCX <- prcomp(X)

# Extract eigenvalues
eigenvalues <- PCX$sdev^2

# Cumulative proportion of explained variance
cumsum(prop.table(eigenvalues))[1:Q]

# Screeplot
plotuScree(x = eigenvalues)

# Non-graphical solutions
nScree(x = eigenvalues)

# Estimate PCovR model ---------------------------------------------------------

# Fit PCovR
PCovR_out <- pcovr(
    X = X,
    Y = as.data.frame(y),
    rot = "none",
    modsel = "seq" # fastest option
)

# Look at the summary
summary(PCovR_out)

# Look at the plot
plot(PCovR_out)

# Npcs selected by PCovR
PCovR_out$R

# Value of alpha selected
PCovR_out$alpha

# Estimate SPCR (package) ------------------------------------------------------

# Define a train data
data.train <- list(x = t(as.matrix(X)), y = y, featurenames = colnames(X))

# Train the model (computes the scores for each feature)
train.obj <- superpc.train(
    data = data.train,
    type = "regression"
)

# Cross-validate the model
cv.obj <- superpc.cv(
    fit = train.obj,
    data = data.train,
    n.fold = 10,
    n.threshold = 20,
    n.components = 5
)

# Objects 
cv.obj$thresholds
cv.obj$scor

# Plot the cross-validation curves
superpc.plotcv(cv.obj)

# Estimate SPCR (my version of the superpc method) -----------------------------

# Obtain CV estimates
out_F <- .spcr.cv(
    dv = y,
    ivs = X,
    fam = "gaussian",
    nthrs = 20,
    maxnpcs = 10,
    K = 10,
    test = c("LRT", "F", "MSE")[2],
    thrs = c("LLS", "pseudoR2", "normalized")[3],
    min.features = 1,
    max.features = ncol(X)
)

# Obtain CV estimates
out_LRT <- .spcr.cv(
    dv = y,
    ivs = X,
    fam = "gaussian",
    nthrs = 20,
    maxnpcs = 10,
    K = 5,
    test = c("LRT", "F", "AIC", "BIC", "PR2")[1],
    thrs = c("LLS", "pseudoR2", "normalized")[3],
    min.features = 1,
    max.features = ncol(X)
)

# Obtain CV estimates
out_BIC <- .spcr.cv(
    dv = y,
    ivs = X,
    fam = "gaussian",
    nthrs = 20,
    maxnpcs = 10,
    K = 5,
    test = c("LRT", "F", "AIC", "BIC")[4],
    thrs = c("LLS", "pseudoR2", "normalized")[3],
    min.features = 1,
    max.features = ncol(X)
)

# Check scores
t(out_F$scor)
t(out_LRT$scor)

# Check thresholds
data.frame(
    F = c(
        thr = which(out_F$thr.cv == round(out_F$thr, 3)),
        Q = out_F$Q.cv
    ),
    LRT = c(
        thr = which(out_LRT$thr.cv == round(out_LRT$thr, 3)),
        Q = out_LRT$Q.cv
    ),
    BIC = c(
        thr = which(out_BIC$thr.cv == round(out_BIC$thr, 3)),
        Q = out_BIC$Q.cv
    )
)

# Plot trends in a similar way to the package
df <- reshape2::melt(out_F$scor) # the function melt reshapes it from wide to long
plot_norm <- ggplot(df, aes(Var2, value, group = factor(Var1), label = factor(Var1))) +
    geom_line() +
    geom_point() +
    geom_label() +
    theme_bw()

# Plot trends in a similar way to the package
df <- reshape2::melt(out_LRT$scor) # the function melt reshapes it from wide to long
plot_LRT <- ggplot(df, aes(Var2, value, group = factor(Var1), label = factor(Var1))) +
    geom_line() +
    geom_point() +
    geom_label() +
    theme_bw()

# Plot trends in a similar way to the package
df <- reshape2::melt(out_BIC$scor) # the function melt reshapes it from wide to long
plot_BIC <- ggplot(df, aes(Var2, value, group = factor(Var1), label = factor(Var1))) +
    geom_line() + 
    geom_point() + 
    geom_label() + 
    # coord_cartesian(ylim = c(min(out_BIC$scor, na.rm = TRUE), 0)) + 
    theme_bw()

plot_norm / plot_LRT + plot_BIC

# Function based on PCKG

spcr.out.pkg <- .spcr.cv.pkg(
    dv = y,
    ivs = X,
    fam = "gaussian",
    nthrs = 20,
    maxnpcs = 5,
    K = 10
)

# Check thresholds are the same
cbind(
    pack = cv.obj$thresholds,
    aspack = spcr.out.pkg$thr,
    mine = out$thr
)

# Check scores are the same
t(cv.obj$scor)
t(spcr.out.pkg$scor)
t(out$scor)

spcr.out.pkg$thr

t(out_LRT$scor)
t(out_F$scor)

df <- reshape2::melt(spcr.out.pkg$scor) # the function melt reshapes it from wide to long
ggplot(df, aes(Var2, value, group = factor(Var1), label = factor(Var1))) +
    geom_line() + 
    geom_point() + 
    geom_label() + 
    theme_bw()

# Estimate SPCR (full CV method) -----------------------------------------------

# Use the functions with a given method
out1 <- .spcr.cv.full(
    dv = y,
    ivs = X,
    fam = "gaussian",
    nthrs = 20,
    maxnpcs = 5,
    K = 10,
    test = "F",
    thrs = "normalized",
    min.features = 1,
    max.features = ncol(X)
)

# Vector of desired methods
vmeth <- c("LRT", "F", "AIC", "BIC", "PR2", "MSE")#[1:2]

# Estimate all of the methods desired
out <- lapply(
    vmeth, 
    function(meth) {
    .spcr.cv.full(
        dv = y,
        ivs = X,
        fam = "gaussian",
        nthrs = 20,
        maxnpcs = 5,
        K = 10,
        test = meth,
        thrs = "normalized",
        min.features = 1,
        max.features = ncol(X),
        oneSE = TRUE
    )
})

# Give names for simplicity
names(out) <- vmeth

# Create plots for all of the desired methods
plots <- lapply(
    1:length(vmeth),
    # meth <- 2
    function(meth) {

        # Plot trends in a similar way to the package
        df <- reshape2::melt(out[[meth]]$scor) # the function melt reshapes it from wide to long

        # Add error bars
        df$low <- reshape2::melt(out[[meth]]$scor.lwr)[, "value"]
        df$high <- reshape2::melt(out[[meth]]$scor.upr)[, "value"]

    # Make plot
    store_plot <- df %>%
        filter(Var1 %in% unique(Var1)) %>%
        ggplot(
            aes(Var2, value, group = factor(Var1), label = factor(Var1))
        ) +
        geom_line() +
        # geom_point() +
        geom_errorbar(aes(ymin = low, ymax = high),
            width = .2
        ) +
        geom_label() +
        ggtitle(paste0("Measure used for cv: ", vmeth[meth])) +
        theme_bw()

        # And return the plot
        return(store_plot)
    }
)

# Plots
(plots[[1]] + plots[[2]] + plots[[3]]) / (plots[[4]] + plots[[5]] + plots[[6]])

# default Solutions
res <- lapply(
    1:length(vmeth),
    function(meth) {
        c(
            thr_value = out[[meth]]$thr.cv,
            thr_number = which(out[[meth]]$thr.cv == round(out[[meth]]$thr, 3)),
            Q = out[[meth]]$Q.cv
        )
    }
)

# 1se solutions
res.1se <- lapply(
    1:length(vmeth),
    function(meth) {
        c(
            thr_value = out[[meth]]$thr.cv.1se,
            thr_number = which(out[[meth]]$thr.cv.1se == round(out[[meth]]$thr, 3)),
            Q = out[[meth]]$Q.cv.1se
        )
    }
)

# Present them neatly
res <- do.call(rbind, res)
rownames(res) <- vmeth
res.1se <- do.call(rbind, res.1se)
rownames(res.1se) <- vmeth

# Compare results
res
res.1se