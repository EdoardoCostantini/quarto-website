# Project:   quarto-website
# Objective: Build and interpret PCA biplots
# Author:    Edoardo Costantini
# Created:   2022-11-22
# Modified:  2022-11-22
# Notes: 

# Set up ----------------------------------------------------------------------

# For better biplots
library(ggfortify)

# Keep the numeric variables
df <- iris[1:4]

# Compute PCs
pca_res <- prcomp(df, scale. = TRUE)

# Generate plot with default biplot function
biplot(pca_res)

# Generate a more readable biplot
autoplot(pca_res,
    data = df,
    loadings = TRUE, loadings.colour = "blue",
    loadings.label = TRUE, loadings.label.size = 3
)

# Build biplot yourself -------------------------------------------------------

# Extract component scores
T <- pca_res$x

# Define the index of PCs to plot
PC_a <- 1
PC_b <- 2

# Plot PC scores
plot(
    x = T[, PC_a], 
    y = T[, PC_b],
    xlab = paste0("PC", PC_a),
    ylab = paste0("PC", PC_b),
    main = "PCA biplot",
    col = "gray",
    pch = 19 # solid circle
)

# Extract loadings
V <- pca_res$rotation

# Count how many variables have been used to compute the PC scores
p <- ncol(df)

# Define a scaling factor for loading arrows
sf <- 2

# Add arrows from 0 to loading on the selected PCs (scaled up)
arrows(
    x0 = rep(0, p), 
    y0 = rep(0, p),
    x1 = V[, PC_a] * sf, 
    y1 = V[, PC_b] * sf,
    col = "darkgray"
)

# Add names of variables per arrow
text(x = V[, PC_a] * sf, y = V[, PC_b] * sf, labels = rownames(V))
