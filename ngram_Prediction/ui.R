#source("ngram_Prediction/katz_backoff.r")

library(shiny)
library(shinycssloaders)
library(bslib)

# Define UI for application that draws a histogram
fluidPage(
  theme = bs_theme(bootswatch = "sandstone"), 
  
  # Application title
  titlePanel("Ngram Prediction App"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textAreaInput("phrase",
                    "Input text:",
                    value = "", 
                    placeholder = "Enter text here"), 
      actionButton("go", "Predict"), 
      hr(),
      h3("Change Computation Time"),
      sliderInput("trimM", "Monogram", min = 1, max = 100, value = 100, ticks = FALSE, post = "%"), 
      sliderInput("trimB", "Bigram", min = 1, max = 100, value = 100, ticks = FALSE, post = "%"), 
      sliderInput("trimT", "Trigram", min = 1, max = 100, value = 100, ticks = FALSE, post = "%"), 
      sliderInput("trimQ", "Quadgram", min = 1, max = 100, value = 100, ticks = FALSE, post = "%"),
      p("Using a smaller percentage of data will reduce computation time by including
        only the most frequently seen combinations. However, leaving out less 
        frequently seen combinations will reduce flexibility and accuracy."),
      htmlOutput("subsetL")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      wellPanel(align = "center", 
                h3(tags$u("Top Prediction")),
      withSpinner(htmlOutput("pred"), 
                  type = 3, 
                  color="#86bb76", 
                  color.background = "#ffffff"),
      hr(),
      h3(tags$u("Top Ten Results")),
      tableOutput("table1")
      )
    )
  )
)
