library(shiny)
library(wordcloud2) 





# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    titlePanel("Text Analytics Presentation - Team 7"),
    
    navlistPanel(
        "General",
        tabPanel("Intro",tags$img(src = "Opening.png")
                 
                          ),
        
        
        
        tabPanel("Models",
                 navbarPage("Navbar!",
                            tabPanel("Plot",
                                     sidebarLayout(
                                         sidebarPanel(
                                             radioButtons("plotType", "Plot type",
                                                          c("Economy"="eco", "Continent"="Co", "Country" = "Bi")
                                                          ),
                                             sliderInput("aa",
                                                         "Number of Counts for Countries:",
                                                         min = 1,
                                                         max = 10,
                                                         value = 2),
                                             width = 2
                                                      ),
                                         mainPanel(
                                             plotOutput("pieplot")
                                                   )
                                                   )
                                     ),
                            tabPanel("Naive Bayes",
                                     fluidRow(
                                         column(3),
                                         column(5,
                                     plotOutput("confusionplot")
                                                )
                                            )
                                     ),
                            tabPanel("TF_IDF",
                                     sliderInput("bb",
                                                 "Number of counts:",
                                                 min = 1,
                                                 max = 30,
                                                 value = 15),
                                     plotOutput("tfidfplot")
                                
                                    )
                            ),
                 ),
        
        "Insights",
        
        tabPanel("Frequency",
                 selectInput("Question", "Questions:", 
                             c("Q1. Describe your shopping list for a Christmas dinner / special dinner." = "q1",
                               "Q2. What are the biggest challenges your native country is facing." = "q2",
                               "Q3. What are your 3 favourite sports?" = "q3",
                               "Q4. How much do you buy online and what kind of products." = "q4",
                               "Q5. Describe a typical high school day." = "q5",
                               "Q6. What is your home country?" = "q6"),
                             width = 500),
                 fluidRow(
                     column(5, plotOutput("distPlot2")),
                     column(5, plotOutput("distPlot3"))),
                 sliderInput("gg",
                             "Number of Highest Frequency Words:",
                             min = 1,
                             max = 50,
                             value = 10)),
        tabPanel("Bigrams",
                 selectInput("Question2", "Questions:", 
                             c("Q1. Describe your shopping list for a Christmas dinner / special dinner." = "q1",
                               "Q2. What are the biggest challenges your native country is facing." = "q2",
                               "Q3. What are your 3 favourite sports?" = "q3",
                               "Q4. How much do you buy online and what kind of products." = "q4",
                               "Q5. Describe a typical high school day." = "q5",
                               "Q6. What is your home country?" = "q6"),
                             width = 500),
                 fluidRow(
                     column(2),
                     column(3, titlePanel(h3("Developed"))),
                     column(2),
                     column(3, titlePanel(h3("Developing"))),
                         ),
                 fluidRow(
                     column(5, plotOutput("distPlot4")),
                     column(5, plotOutput("distPlot5"))),
                 sliderInput("ss",
                             "Frequency Count of Words:",
                             min = 1,
                             max = 10,
                             value = 1)
                            ),
        "Alter Model",
        
        tabPanel("LDA Cluster", 
                 numericInput("Clusters", label = h3("Clusters Quantity :"), value = 2),
                 
                 hr(),
                 plotOutput("value")
                 ),
        
        tabPanel("Product", 
                 tags$img(src = "Product.png")
        ),
        
        tabPanel("The End",
                 fluidRow(column(8,
                 wordcloud2Output("wordcloud")
                 ))),
        

    widths = c(1,11))
))
