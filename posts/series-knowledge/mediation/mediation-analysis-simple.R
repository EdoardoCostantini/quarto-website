# Project:   quarto-website
# Objective: Simple mediation analysis
# Author:    Edoardo Costantini
# Created:   2024-02-03
# Modified:  2024-02-03
# Notes:     See mediation-analysis-simple-model.pdf for model graph

# Generate data ----------------------------------------------------------------

# Define sample size
n <- 1e4

# Define effects
a <- 10
b <- -20 # play around with this effect to see what happens to mediation
c <- -5
bc <- b * c
total <- a + b * c

# Set seed for replicability
set.seed(1234)

# Define variables based on model
sport <- rnorm(n) # predictor
weight <- b * sport + rnorm(n) # mediator
health <- 500 + a * sport + c * weight + rnorm(n) # outcome variable
Data <- data.frame(sport = sport, health = health, weight = weight)

# Estimation -------------------------------------------------------------------

# Define mediation model
model <- " # direct effect
             health ~ a * sport
           # mediator
             weight ~ b * sport
             health ~ c * weight
           # indirect effect (a * b)
             bc := b * c
           # total effect
             total := a + b * c
         "

# Alternative Define mediation model
model <- " # direct effect
             health ~ a * sport + c * weight
           # mediator
             weight ~ b * sport
           # indirect effect (a * b)
             bc := b * c
           # total effect
             total := a + b * c
         "


# Estimate model
fit <- sem(model, data = Data)

# get coefficients
fit_coefs <- coef(fit)

# Get fit measures
summary(fit)

# Check if the estiamted effects match the true ones
bc
total

# estimate a model without mediation
lm1 <- lm(health ~ sport + weight, data = Data)

# Effect of sport on health without estimating mediation
coef(lm1)["sport"]

# Effect of sport on health when estimating effect via weight
coef(fit)[c("a")] + prod(coef(fit)[c("b", "c")])