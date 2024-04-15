# Project:   quarto-website
# Objective: Code notes on how to handle errors and warnings
# Author:    Edoardo Costantini
# Created:   2023-08-03
# Modified:  2023-08-03
# Notes:     inspired by https://gist.github.com/mihaiconstantin/c4260fb8d7f3cd37e37bad589db4c270

# Task function with errors or warnings.
naughty_task <- function(error = TRUE, warning = FALSE) {
    # If warning.
    if (warning) {
        warning("Warning thrown.")
    }

    # If error.
    if (error) {
        stop("Error thrown.")
    }

    return(1)
}

# Test the function.
naughty_task(error = TRUE, warning = TRUE)

# Collect errors and warnings.
errors <- NULL
warnings <- NULL

# Try to run.
tryCatch(
    withCallingHandlers(
        # The expression to evaluate.
        expr = naughty_task(error = TRUE, warning = TRUE),

        # The warning handler.
        warning = function(w) {
            # Store warning.
            warnings <<- c(warnings, w$message)

            # Prevent the warning from being printed.
            invokeRestart("muffleWarning")
        }
    ),

    # The error handler.
    error = function(e) {
        # Store error.
        errors <<- c(errors, e$message)
    }
)

# Check the collectors.
errors
warnings