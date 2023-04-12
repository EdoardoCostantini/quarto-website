# How to: multinomial logistic regression

library(nnet) # for multinomial regression
library(MASS) # for cumulative logits

# Alligator example from Agresti 2002 -------------------------------------
# see: http://users.stat.ufl.edu/~aa/cda/Thompson_manual.pdf p.123 pdf

# 1. Data (Recreate data displayed in table 7.1)
food.labs <- factor(c("fish", "invert", "rep", "bird", "other"),
  levels = c("fish", "invert", "rep", "bird", "other")
)
size.labs <- factor(c("<2.3", ">2.3"),
  levels = c(">2.3", "<2.3")
)
gender.labs <- factor(c("m", "f"),
  levels = c("m", "f")
)
lake.labs <- factor(c("hancock", "oklawaha", "trafford", "george"),
  levels = c("george", "hancock", "oklawaha", "trafford")
)
table.7.1 <- expand.grid(
  food = food.labs,
  size = size.labs,
  gender = gender.labs,
  lake = lake.labs
)
temp <- c(
  7, 1, 0, 0, 5, 4, 0, 0, 1, 2, 16, 3, 2, 2, 3, 3, 0, 1, 2, 3, 2, 2, 0, 0, 1, 13, 7, 6, 0,
  0, 3, 9, 1, 0, 2, 0, 1, 0, 1, 0, 3, 7, 1, 0, 1, 8, 6, 6, 3, 5, 2, 4, 1, 1, 4, 0, 1, 0, 0,
  0, 13, 10, 0, 2, 2, 9, 0, 0, 1, 2, 3, 9, 1, 0, 1, 8, 1, 0, 0, 1
)
table.7.1 <- structure(
  .Data = table.7.1[rep(1:nrow(table.7.1), temp), ],
  row.names = 1:219
)

# 2. Fit models
options(contrasts = c("contr.treatment", "contr.poly"))
fitS <- multinom(food ~ lake * size * gender, data = table.7.1) # saturated model
fit0 <- multinom(food ~ 1, data = table.7.1) # null
fit1 <- multinom(food ~ gender, data = table.7.1) # G
fit2 <- multinom(food ~ size, data = table.7.1)
fit3 <- multinom(food ~ lake, data = table.7.1)
fit4 <- multinom(food ~ size + lake, data = table.7.1) # <- selected model
fit5 <- multinom(food ~ size + lake + gender, data = table.7.1)

# Prediction for invertebrates instead of fish
X <- model.matrix(~ size + lake, table.7.1)
oddsratio <- exp(X %*% coef(fit4)[1, ])
# ratio of probability of choosing invertebrates instead
# of fish for each alligator (odds)

# > Explicit prediction for a specific alligator -------------------------------

# What Am I trying to predict?
table(table.7.1$food)

# How many levels does the DV have?
J <- nlevels(table.7.1$food)
K <- J - 1

# Who am I trying to predict it for? (specific alligator)
alligator <- c(1, 0, 1, 0, 0)

# Define the same denominator for all possible food choices (DV)
denom <- 1 + sum(
  exp(alligator %*% coef(fit4)[1, ]),
  exp(alligator %*% coef(fit4)[2, ]),
  exp(alligator %*% coef(fit4)[3, ]),
  exp(alligator %*% coef(fit4)[4, ])
)


# Obtain predicted probabilities of eating a specific food (e.g., invertebrates)
# for a specific alligator that is large and lives in lake hancock
c(
  # probability of choosing fish
  prF = 1 / denom,
  # choosing invertebrates
  prI = exp(alligator %*% coef(fit4)[1, ]) / denom,
  # reptile
  prR = exp(alligator %*% coef(fit4)[2, ]) / denom,
  # bird
  prB = exp(alligator %*% coef(fit4)[3, ]) / denom,
  # other
  prO = exp(alligator %*% coef(fit4)[4, ]) / denom
)

# > Computational shortcuts ----------------------------------------------------

# First compute the logits
logits <- c(
  0, # a shortcut for the 1 in the denominator
  t(alligator) %*% t(coef(fit4))
)

# And compute the
exp(logits) / sum(exp(logits))

# Confirmed by the following:
predict(fit4,
  type = "probs",
  newdata = data.frame(
    size = ">2.3",
    lake = "hancock"
  )
)

# > Prediction for range of observations ---------------------------------------

# Define the range of alligators
new_alli <- 1:5

# Obtain prob prediction for these alligators:
predict(fit4, type = "probs", newdata = table.7.1[new_alli, , drop = FALSE])

# Manually compute the logits
logits <- (
  cbind(0, X[new_alli, , drop = FALSE] %*% t(coef(fit4)))
)

# Obtain prob prediction for these alligators (manually):
exp(logits) / (rowSums(exp(logits)))
