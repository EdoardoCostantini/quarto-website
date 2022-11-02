
library(mice)

set.seed(20220920)

# Define parameters
n <- 50
b0 <- 10
b1 <- 5
b2 <- 5
b3 <- 0

# Storing objects
B1 <- CIC <- CIW <- RMSE <- T1E <- data.frame(reg = NA, bayes = NA)

# Simulation
for (s in 1:5e3) {
    print(s)

    # Sample X data
    X <- MASS::mvrnorm(
        n = n,
        mu = rep(0, 3),
        Sigma = matrix(c(
            1, .5, 0,
            .5, 1, 0,
            0, 0, 1
        ), nrow = 3)
    )
    
    # Sample Y from DGM
    y <- b0 + b1 * X[, 1] + b2 * X[, 2] + b3 * X[, 3] + rnorm(n)

    # Put together in a data frame
    dt <- data.frame(y = y, X = X)

    # Compute the probabilities of nonresponse:
    probs <- plogis(X[, 2])

    # Return a logical nonresponse vector:
    wy <- (as.logical(rbinom(n = n, size = 1, prob = probs)))

    # Impose missingness
    dt$y[wy] <- NA

    # Impute with regression imputation
    imp.pred <- mice(dt, method = "norm.predict", m = 5, print = FALSE)

    # Impute with Bayesian imputation
    imp.bays <- mice(dt, method = "norm", m = 5, print = FALSE)

    # Fit a model to the multiple imputations
    fit.pred <- with(imp.pred, lm(y ~ X.1 + X.2 + X.3))
    fit.bays <- with(imp.bays, lm(y ~ X.1 + X.2 + X.3))

    # Pool statistics
    est.pred <- summary(pool(fit.pred), conf.int = TRUE)
    est.bays <- summary(pool(fit.bays), conf.int = TRUE)

    # Store B1
    B1[s, ] <- c(est.pred$estimate[2], est.bays$estimate[2])

    # Extract confidence intervals
    ci.pred <- est.pred[2, c("2.5 %", "97.5 %")]
    ci.bays <- est.bays[2, c("2.5 %", "97.5 %")]

    # Confidence interval includes true values?
    CIC[s, ] <- c(
        ci.pred$"2.5 %" < b1 & b1 < ci.pred$"97.5 %",
        ci.bays$"2.5 %" < b1 & b1 < ci.bays$"97.5 %"
    )
    
    # Confidence interval width
    CIW[s, ] <- c(
        ci.pred$"97.5 %" - ci.pred$"2.5 %",
        ci.bays$"97.5 %" - ci.bays$"2.5 %"
    )

    # Check type I error rate
    T1E[s, ] <- c(est.pred$p.value[4] < .05, est.bays$p.value[4] < .05)

    # Generate imputation-specific predictions:
    preds.pred <- sapply(fit.pred$analyses, function(f) {
        predict(f, newdata = dt[wy, ])
    })

    preds.bays <- sapply(fit.bays$analyses, function(f) {
        predict(f, newdata = dt[wy, ])
    })

    # Compute (pooled) RMSE
    RMSE[s, ] <- c(
        mean(
            apply(preds.pred, 2, function(col) {
                sqrt((sum(y[wy] - col)^2) / n)
            })
        ),
        mean(
            apply(preds.bays, 2, function(col) {
                sqrt((sum(y[wy] - col)^2) / n)
            })
        )
    )
}

# Results
res <- round(
    cbind(
        B1 = colMeans(B1),
        CIC = colMeans(CIC),
        CIW = colMeans(CIW),
        T1E = colMeans(T1E),
        RMSE = colMeans(RMSE)
    ),
    3
)
res

# Save results
saveRDS(res, "rmse-imputation-criteria.rds")