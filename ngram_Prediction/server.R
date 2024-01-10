#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  withProgress(message = 'Loading data', value = 0, {
    source("katz_backoff.r")
  })
  ## Need to work on being able to subset datasets via user input. 
  ### Planning to make the 4 datasets reactive and modify the functions
  ### from katz_ngram to accept the datasets as inputs. 
  pred <- eventReactive(input$go, {
                        katz_ngram(input$phrase)
                        })

  output$pred <- renderText({
    if(input$go > 0) {
      paste0("The top predicted next word is: ", "<b>", pred()$prediction[1], "</b>",
             "</br>", "With probability ", "<b>", round(pred()$prob[1],4), "</b>")
      
    } else {
    paste0("<em>","Awaiting Input","</em>")
    }
  })
  
  output$table1 <- renderTable({
    if(input$go > 0) {
      pred() %>%
        set_names("Prediction", "Probability", 'Ngram Source')
    } else {
      data.frame("Prediction" = rep("-", 10), 
                 "Probability" = rep("-", 10), 
                 "Ngram Source" = rep("-", 10), 
                 check.names = FALSE)
    }
  })
}
