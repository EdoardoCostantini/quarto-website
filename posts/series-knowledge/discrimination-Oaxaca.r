# Project:   quarto-website
# Objective: Study discrimination between groups with Blinder–Oaxaca decomposition
# Author:    Edoardo Costantini
# Created:   2023-12-03
# Modified:  2023-12-03
# Notes: 

# Generate some data -----------------------------------------------------------

set.seed(20231203)

# sample size
n <- 50

# predictors
Education <- sample(c(0, 1, 2, 3), n, replace = TRUE)
SES <- sample(c(0, 1, 2, 3), n, replace = TRUE)
Sex <- sample(c(0, 1), n, replace = TRUE)

# Outcome
Salary <- 1000 + 500 * Education + 200 * SES + 750 * Sex + rnorm(n)

# Put together in data set
dat <- data.frame(
    Salary,
    Sex = Sex,
    Education = Education,
    SES
)

# Unadjusted gender gap --------------------------------------------------------

# Make test
unad_test <- t.test(Salary ~ Sex, data = dat)

# Group difference in pay
gd <- diff(unad_test$estimate)

# Mean for men
men_average <- unad_test$estimate[2]

# Unadjusted gender gap
un_gd <- gd / men_average * 100

# Regression
mod1 <- lm(Salary ~ Sex, data = dat)

# Unadjusted gender gap based on regression
(coef(mod1)["Sex"] / (coef(mod1)["Sex"] + coef(mod1)[2]) * 100)

# Adjusting with Blinder–Oaxaca decomposition ----------------------------------
# follow:
# - Annex 2 in: https://circabc.europa.eu/sd/a/c983d736-2399-40d8-90fa-78925615528d/DSS-2018-Mar-4.3%20Adjusted%20gender%20pay%20gap.pdf
# - For decomposition types: https://cran.r-project.org/web/packages/oaxaca/vignettes/oaxaca.pdf
# - For a good 3-way decomp: https://ete-online.biomedcentral.com/articles/10.1186/s12982-021-00100-9

# Divide data in the two groups
Females <- dat[dat$Sex == 0, ]
Males <- dat[dat$Sex == 1, ]

# Regression in female group
reg_female <- lm(Salary ~ Education + SES, data = Females)

# Regression in male group
reg_male <- lm(Salary ~ Education + SES, data = Males)

# Total difference
delta <- mean(Males$Salary) - mean(Females$Salary)

# Terms
b0_f <- coef(reg_female)[[1]]
b0_m <- coef(reg_male)[[1]]
b_f <- coef(reg_female)[-1]
b_m <- coef(reg_male)[-1]
x_f <- colMeans(Females[, c("Education", "SES")])
x_m <- colMeans(Males[, c("Education", "SES")])

# Two(+1)-fold decomposition
G0 <- b0_m - b0_f
G1 <- (x_f[1] * (b_m[1] - b_f[1])) + (x_f[2] * (b_m[2] - b_f[2]))
G2 <- (b_m[1] * (x_m[1] - x_f[1])) + (b_m[2] * (x_m[2] - x_f[2]))
c(
    delta = delta,
    two_way = as.numeric(G0 + G1 + G2)
)

# Three(+1)-fold decomposition
B <- b0_m - b0_f
E <- (b_f[1] * (x_m[1] - x_f[1])) + (b_f[2] * (x_m[2] - x_f[2]))
C <- (x_f[1] * (b_m[1] - b_f[1])) + (x_f[2] * (b_m[2] - b_f[2]))
I <- (x_m[1] - x_f[1]) * (b_m[1] - b_f[1]) + (x_m[2] - x_f[2]) * (b_m[2] - b_f[2])
c(
    delta = delta,
    two_way = as.numeric(G0 + G1 + G2),
    three_way = as.numeric(B + E + C + I)
)

# Percentages of difference explained by given factors

# Explained by the predictors in the model
E / delta * 100
G2 / delta * 100

# Explained by Education
(b_f[1] * (x_m[1] - x_f[1])) / delta * 100

# Explained by SES
(b_f[2] * (x_m[2] - x_f[2])) / delta * 100

# Unexplained
(G0 + G1) / delta * 100

# oaxaca package ---------------------------------------------------------------

# Install
install.packages("oaxaca")

# Load
library(oaxaca)

# Fit oaxaca
oax_educ <- oaxaca(Salary ~ Education | Sex, data = dat, R = 1)
oax_full <- oaxaca(Salary ~ Education + SES | Sex, data = dat, R = 1)

#
oax_educ$twofold$overall[1, c(2, 4)]
oax_full$twofold$overall[1, c(2, 4)]

#
oax_educ$twofold$variables[[1]][-1, 2] / oax_educ$y$y.diff * 100
oax_full$twofold$variables[[1]][-1, 2] / oax_full$y$y.diff * 100

# Extract two fold with group A as reference
data.frame(
    Package = oax$twofold$overall[1, c(2, 4)],
    Manual = c(G2, G0 + G1)
)

# Extract three fold with group A as reference
data.frame(
    Package = na.omit(oax$threefold$overall),
    Manual = c(E, B + C, I)
)

# Contribution of each variable to the explained difference in salaries
oax$twofold$variables[[1]][-1, 2] / oax$y$y.diff * 100

# Oaxaca package example with their data ---------------------------------------

# Check chicago data
head(chicago)

#