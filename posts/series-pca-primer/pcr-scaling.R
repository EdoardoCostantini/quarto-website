
# Scaling of a dataset based on another ----------------------------------------

dotxobs <- mtcars[1:20, ]
xmis <- mtcars[-(1:20), ]

#
dotxobs <- scale(dotxobs, center = TRUE, scale = TRUE)

#
colMeans(dotxobs)
apply(dotxobs, 2, var)

# Center the data
xmis_s <- scale(xmis,
    center = attributes(dotxobs)$`scaled:center`,
    scale = FALSE
)

round(
    cbind(
        colMeans(xmis) - colMeans(mtcars[1:20, ]),
        colMeans(xmis_s)
    ), 3
)

# Scale accordingly
xmis_s <- scale(xmis_s,
    center = FALSE,
    scale = attributes(dotxobs)$`scaled:scale`
)

# Do it manually
sapply(1:ncol(mtcars), function(j){
    (xmis[, j] - mean(mtcars[1:20, j])) / sd(mtcars[1:20, j])
})

xmis_s

prova <- sapply(1:ncol(mtcars), function(j) {
    (mtcars[1:20, j] - mean(mtcars[1:20, j])) / sd(mtcars[1:20, j])
})

tail(dotxobs)
tail(prova)

colMeans(prova)
apply(prova, 2, var)

# Vars
round(cbind(
    apply(xmis, 2, var),
    apply(xmis_s, 2, var)
), 3)

# PCR scaling ------------------------------------------------------------------

# Define dependent variable
y <- mtcars[, 1]

# Define predictor set
X <- as.matrix(mtcars[, -1])

# Define training and test data
train <- 1:20
test <- -c(1:20)

# Define how many pcs to use
npcs <- 2

# Train PCR on dotxobs sample
pcr_out <- pls::pcr(
    y[train] ~ X[train, ],
    ncomp = npcs,
    scale = TRUE,
    center = TRUE,
    validation = "none"
)

# Predict on test data
predict(pcr_out, newdata = X[test, ], ncomp = npcs, type = "response")

# > Fit PCR manually (scale train, scale test to match) ------------------------

# Scale train data in the usual way
Xtrain_s <- sapply(1:ncol(X), function(j) {
    (X[train, j] - mean(X[train, j])) / sd(X[train, j])
})

# Scale the test data based on the training data
Xtest_s <- sapply(1:ncol(X), function(j) {
    (X[test, j] - mean(X[train, j])) / sd(X[train, j])
})

# Train PCA
svdXtrain_s <- svd(Xtrain_s)

# Compute PC scores
T <- svdXtrain_s$u %*% diag(svdXtrain_s$d)[, 1:npcs]

# Fit linear model
lm.train <- lm(y[train] - mean(y[train]) ~ -1 + T)
lm.train.int <- lm(y[train] ~ T)

summary(lm.train)
summary(lm.train.int)

sigma(lm.train)
sigma(lm.train.int)

mean(y[train])

# The weights in this case should be the v, not transposed
(Xtrain_s %*% svdXtrain_s$v)[, 1:npcs]

# We apply the weights to the test data scaled appropriately
Ttest <- (Xtest_s %*% svdXtrain_s$v)[, 1:npcs]

# Predict values
mean(y[train]) + Ttest %*% coef(lm.train)

# check predictions are correctly scaled
data.frame(
    pls_pack = predict(pcr_out, newdata = X[test, ], ncomp = npcs, type = "response"),
    scale_train = mean(y[train]) + Ttest %*% coef(lm.train)
)

# > Fit PCR manually (scale train and test together) ---------------------------

# Scale together
Xs <- scale(X)

# Train PCA
svdXs <- svd(Xs)

# Compute PC scores
TXs <- svdXs$u %*% diag(svdXs$d)[, 1:npcs]

# Fit linear model
lm.trainXs <- lm(y[train] - mean(y[train]) ~ -1 + TXs[train, ])

# check predictions are correctly scaled
data.frame(
    pls_pack = as.vector(predict(pcr_out, newdata = X[test, ], ncomp = npcs, type = "response")),
    scale_X = mean(y[train]) + TXs[test, 1:npcs] %*% coef(lm.trainXs)
)

# Degrees of freedom -----------------------------------------------------------

# Degrees of freedom with or without intercept?
lm.train <- lm(y[train] - mean(y[train]) ~ -1 + T)
lm.train.int <- lm(y[train] ~ T)

# Compare sigmas
data.frame(
    noint = c(
        lm = sigma(lm.train)^2,
        man = sum(lm.train$residuals^2) / (length(train) - npcs)
    ),
    int = c(
        lm = sigma(lm.train.int)^2,
        man = sum(lm.train.int$residuals^2) / (length(train) - npcs - 1)
    )
)

# Compare model fits
summary(lm.train)$r.square
summary(lm.train.int)$r.square
