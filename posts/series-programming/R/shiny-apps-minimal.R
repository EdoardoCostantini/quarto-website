# Project:   quarto-website
# Objective: Basics of a shiny app
# Author:    Edoardo Costantini
# Created:   2024-02-20
# Modified:  2024-02-20
# Notes: 

# load the shiny app package
library(shiny)

# Shiny app basic --------------------------------------------------------------

# UI
ui <- fluidPage(
    "Hello, world!"
)

# Server
server <- function(input, output, session) {
}

# Run shiny app
shinyApp(ui, server)

# Add some UI elements ---------------------------------------------------------

# UI
ui <- fluidPage(
    selectInput(
        "variable", 
        label = "Chosen variable", 
        selected = colnames(mtcars)[1],
        choices = c(colnames(mtcars)[1:4], "hello")
    ),
    shiny::plotOutput("density")
)

# Server
server <- function(input, output, session) {
    output$density <- renderPlot(
        plot(density(mtcars[, input$variable]))
    )
}

# Run shiny app
shinyApp(ui, server)
