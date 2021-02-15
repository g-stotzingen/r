#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Dynamic Prediction"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("q1", "Describe your shopping list for a Christmas dinner / special dinner", "Write your answer"),
            textInput("q2", "What are the biggest challenges your native country is facing?", "Write your answer"),
            textInput("q3", "What are your 3 favourite sports?", "Write your answer"),
            textInput("q4", "How much do you buy online and what kind of products?", "Write your answer"),
            textInput("q5", "Describe a typical high school day", "Write your answer"),
            submitButton("Predict", icon("Submit"))
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            verbatimTextOutput("value")
        )
    )
))
