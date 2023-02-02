## Influx outflux study

# Load mice package
library(mice)

# Consider a simple dataset
nhanes

# Obtain correct influx outflux pattern
io_pattern <- mice::flux(nhanes, local = names(nhanes))

# Get the Response indicator matrix
R <- !is.na(nhanes) # 1 = rec, 0 = mis

# Inbound Statistic (proportion of usable cases)
inboundStat <- function (R, j, k){
  sum((1 - R[, j]) * R[, k]) / sum(1 - R[, j])
}

inboundStat(R, j = 2, k = 3)

# average inbound
mean(c(inboundStat(R, j = 2, k = 1),
       inboundStat(R, j = 2, k = 3),
       inboundStat(R, j = 2, k = 4)))

# Outbound statistic
outboundStat <- function (R, j, k){
  sum(R[, j] * (1 - R[, k])) / sum(R[, j])
}

outboundStat(R, j = 2, k = 3)

# average inbound
mean(c(outboundStat(R, j = 2, k = 1),
       outboundStat(R, j = 2, k = 3),
       outboundStat(R, j = 2, k = 4)))

# Influx
influx <- function (R, j){
  sum((1 - R[, j]) * R[, -j]) / sum(R)
}

influx(R, 2)
sapply(1:ncol(R), influx, R = R)

# Outflux
outflux <- function (R, j){
  sum(R[, j] * (1 - R[, -j])) / sum(1 - R)
}

outflux(R, 2)
sapply(1:ncol(R), outflux, R = R)

# Compare with mice results
io_pattern
cbind(influx = sapply(1:ncol(R), influx, R = R),
      outflux = sapply(1:ncol(R), outflux, R = R))