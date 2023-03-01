# Project:   quarto-website
# Objective: Explore the CHull method for model selection
# Author:    Edoardo Costantini
# Created:   2023-02-21
# Modified:  2023-02-21
# Notes: 

# Load package
install.packages("multichull")
library("multichull")

# Load 
complexity.fit <- cbind(
    c(305, 456, 460, 607, 612, 615, 758, 764, 768, 770, 909, 916, 921, 924),
    c(152, 89, 79, 71, 57, 57, 64, 49, 47, 47, 60, 41, 39, 39)
)
output <- CHull(complexity.fit)
output$Solution
plot(output)
print(output)
summary(output)