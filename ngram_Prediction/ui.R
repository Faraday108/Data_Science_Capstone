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
      sliderInput("trimC", "Trim ngram frequencies", min = 5, max = 1000, value = 5), 
      p("Trimming ngram frequencies will speed up predictions at the cost of 
        reduced accuracy.")
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
