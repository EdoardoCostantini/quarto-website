# Source: Enders 2010, Ch 10

# Import Table 10

table10 <- matrix(c(
    78, 13, 9, 9,
    84, 9, 13, 13,
    84, 10, 10, 10,
    85, 10, 8, NA,
    87, 7, 7, NA,
    91, 3, 7, NA,
    92, 12, 9, 9,
    94, 3, 9, 9,
    94, 13, 11, 11,
    96, 7, 7, NA,
    99, 6, 7, NA,
    105, 12, 10, 10,
    105, 14, 11, 11,
    106, 10, 15, 15,
    108, 7, 10, 10,
    112, 10, 10, 10,
    113, 14, 12, 12,
    115, 14, 14, 14,
    118, 12, 16, 16,
    134, 11, 12, 12
), ncol = 4, byrow = TRUE)

dt <- table10[, c(1, 2, 4, 3)]
colnames(dt) <- c("IQ", "WB", "JP", "JP_hp")
dt <- as.data.frame(dt)

dt$R <- is.na(dt$JP)

# Selection model -------------------------------------------------------------

# p(Y): Marginal distribution / substantive model

lm(JP ~ IQ, data = dt)

# p(R | Y): Conditional distribution of the missing data

glm(R ~ WB,
    data = dt,
    family = binomial(link = "probit")
)
