source("helpers.R")

library(shiny)
# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output, session) {

  output$caption <- renderText({
    do.call(makeCaption, list(input))
  })
  output$predictions <-renderUI({
    choices <- selectInput("choices", "Choose a word:",
                           choices = do.call(makeTable, list(input)))
  })
  
  observe({
    x <- input$action
    if (x > updateCounter){
      
      updateTextInput(session, "caption", 
                      value=sprintf ("%s%s ", caption, input$choices))
      updateCounter <<- x
    }
    
  })
})