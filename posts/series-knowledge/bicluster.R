# Project:   quarto-website
# Objective: Exploring bilcusters through the biclust R package
# Author:    Edoardo Costantini
# Created:   2023-02-02
# Modified:  2023-02-02
# Notes: 

# Package ----------------------------------------------------------------------

install.packages("biclust")
library(biclust)

# Example run ------------------------------------------------------------------

# Some data
test <- matrix(rbinom(400, 50, 0.4), 20, 20)

# To be biclustered
res1 <- biclust(test, method = BCCC(), delta = 1.5, alpha = 1, number = 10)
