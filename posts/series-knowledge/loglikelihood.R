# Project:   quarto-website
# Objective: Explore log-likelihood extraction function
# Author:    Edoardo Costantini
# Created:   2023-02-02
# Modified:  2023-02-02
# Notes: 

# insert header here --------------------------------------------------------- 

# Store mtcars as a data.frame
cars <- as.data.frame(mtcars)

# Fit models
lm0 <- lm(mpg ~ 1, data = mtcars)
lm1 <- lm(mpg ~ cyl, data = mtcars)
lm2 <- lm(mpg ~ ., data = mtcars)

# Loglikelihood
logLik(lm0)
logLik(lm0, REML = TRUE)
logLik(lm1)
logLik(lm2)

# Compute by hand

# Decide on a model fit
lm.fit <- lm2

# Define sample size
n <- nrow(mtcars)

# Extract the residuals
r <- resid(lm.fit)
cars$mpg - fitted(lm.fit)

# Define the sigma
sigma(lm.fit)
sd(resid(lm.fit))
(s <- sqrt(sum(resid(lm.fit)^2) / (n))) # maximum likelihood version

# Compute data likelihood
(lik <- 1 / sqrt(2 * pi * s^2)^n * exp(-1 / (2 * s^2) * sum(r^2)))
(lik <- prod(dnorm(cars$mpg, mean = fitted(lm.fit), sd = s)))

# Compute log-likelihood
log(lik)

# Compute log-likelihood directly
-n / 2 * log(2 * pi) - n / 2 * log(s^2) - 1 / (2 * s^2) * sum(r^2)

# R function
logLik(lm.fit)

# Logistic regression ----------------------------------------------------------

# Load package for data
library(AER)

# Load Home Mortgage Disclosure Act Data
data(HMDA)

# Check first rows
head(HMDA)

# Fit null logistic model
glm0 <- glm(
    formula = deny ~ 1,
    family = binomial(link = "logit"),
    data = HMDA
)

# Fit logistic with a single predictor
glm1 <- glm(
    formula = deny ~ pirat,
    family = binomial(link = "logit"),
    data = HMDA
)

# Fit logistic with two predictors
glm2 <- glm(
    formula = deny ~ pirat + chist,
    family = binomial(link = "logit"),
    data = HMDA
)

# Define an active model to compute the log-likelihood on
logi.fit <- glm0

# Define sample size
n <- nrow(HMDA)

# Define y
y <- as.numeric(HMDA$deny) - 1

# Define the linear term
bx <- predict(logi.fit, type = "link")

# Probability based on the linear term
cbind(
    exp(bx) / (1 + exp(bx)),
    predict(logi.fit, type = "response")
)
pixi <- exp(bx) / (1 + exp(bx))

# Compare
c(
    R = logLik(logi.fit),
    semi = sum(dbinom(y, size = 1, prob = pixi, log = TRUE)),
    man = sum(y * bx - log(1 + exp(bx)))
)

# Multinomial logistic ---------------------------------------------------------
# see: http://users.stat.ufl.edu/~aa/cda/Thompson_manual.pdf p.123 pdf

# Load package
library(nnet) # for multinomial regression

# Recreate data displayed in table 7.1 Agresti 2002
food <- factor(c("fish", "invert", "rep", "bird", "other"),
    levels = c("fish", "invert", "rep", "bird", "other")
)
size <- factor(c("<2.3", ">2.3"),
    levels = c(">2.3", "<2.3")
)
gender <- factor(c("m", "f"),
    levels = c("m", "f")
)
lake <- factor(c("hancock", "oklawaha", "trafford", "george"),
    levels = c("george", "hancock", "oklawaha", "trafford")
)

# Create all combinations
table.7.1 <- expand.grid(
    food = food,
    size = size,
    gender = gender,
    lake = lake
)

# Store unstructured values
temp <- c(
    7, 1, 0, 0, 5, 4, 0, 0, 1, 2, 16, 3, 2, 2, 3, 3, 0, 1, 2, 3, 2, 2, 0, 0, 1, 13, 7, 6, 0,
    0, 3, 9, 1, 0, 2, 0, 1, 0, 1, 0, 3, 7, 1, 0, 1, 8, 6, 6, 3, 5, 2, 4, 1, 1, 4, 0, 1, 0, 0,
    0, 13, 10, 0, 2, 2, 9, 0, 0, 1, 2, 3, 9, 1, 0, 1, 8, 1, 0, 0, 1
)

# Structure the values
table.7.1 <- structure(
    .Data = table.7.1[rep(1:nrow(table.7.1), temp), ],
    row.names = 1:219
)

# Fit models
fit0 <- multinom(food ~ 1, data = table.7.1)                # null
fit1 <- multinom(food ~ gender, data = table.7.1)           # G
fit2 <- multinom(food ~ size, data = table.7.1) 
fit3 <- multinom(food ~ size + lake, data = table.7.1)      # <- selected model
fitS <- multinom(food ~ lake*size*gender, data = table.7.1) # saturated model

# CHeck log-likelihood values
logLik(fit0)
logLik(fit1)
logLik(fit2)
logLik(fit3)
logLik(fitS)

# Proportional Odds Model for ordered responses --------------------------------
# Agresti section 7.2.2

# Load mass package for the polr function
library(MASS)

# Get the data
table7.5 <- matrix(c(
    1, 1, 1, 1, 1, 9, 1, 1, 4, 1, 1, 3, 1, 0, 2,
    1, 1, 0, 1, 0, 1, 1, 1, 3, 1, 1, 3, 1, 1, 7,
    1, 0, 1, 1, 0, 2, 2, 1, 5, 2, 0, 6, 2, 1, 3,
    2, 0, 1, 2, 1, 8, 2, 1, 2, 2, 0, 5, 2, 1, 5,
    2, 1, 9, 2, 0, 3, 2, 1, 3, 2, 1, 1, 3, 0, 0,
    3, 1, 4, 3, 0, 3, 3, 0, 9, 3, 1, 6, 3, 0, 4,
    3, 0, 3, 4, 1, 8, 4, 1, 2, 4, 1, 7, 4, 0, 5,
    4, 0, 4, 4, 0, 4, 4, 1, 8, 4, 0, 8, 4, 0, 9
), ncol = 3, byrow = T)

# Attribute meaningful names
colnames(table7.5) <- c("mental", "ses", "life")

# Make it a data.frame
table7.5 <- as.data.frame(table7.5)

# Make the mental variable an ordered factor
table7.5$mental <- ordered(
    table7.5$mental,
    levels = 1:4,
    labels = c("well", "mild", "moderate", "impaired")
)

# Fit proportional odds model
polr0 <- polr(mental ~ 1, data = table7.5)
polr1 <- polr(mental ~ ses, data = table7.5)
polr2 <- polr(mental ~ ses + life, data = table7.5)
polr3 <- polr(mental ~ ., data = table7.5)

# Active model
polr.fit <- polr1

# Check summary
summary(polr.fit)

# Compute the logLikelihood value
as.numeric(logLik(polr.fit))

# Poisson regression log-like --------------------------------------------------

# Load datasets
library(datasets)

# Plot and check poissan variable
hist(warpbreaks$breaks)

# Fit Poisson regression model
poisson.model <- glm(
    formula = breaks ~ wool + tension, warpbreaks,
    family = poisson(link = "log")
)

# Extract log-likelihood value
logLik(poisson.model)