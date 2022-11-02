# Project:   blogdown
# Objective: Study setp-wise model selection
# Author:    Edoardo Costantini
# Created:   2022-09-15
# Modified:  2022-09-15
# Notes:  	 http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

# Prepare environment --------------------------------------------------------- 

library(tidyverse)
library(caret)
library(leaps)
library(MASS)

# Explore data
head(swiss)
dim(swiss)

# MASS package: stepAIC -------------------------------------------------------

# Fit the full model
full.model <- lm(Fertility ~ ., data = swiss)

# Stepwise regression model
step.model <- stepAIC(full.model,
    direction = "forward",
    trace = FALSE
)

# Obtain summary
summary(step.model)
 
# leaps package: regsubsets ---------------------------------------------------

models <- regsubsets(Fertility ~ .,
    data = swiss, 
    nvmax = ncol(swiss)-1,
    method = "seqrep"
)

summary(models)