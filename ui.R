library(shiny)
# Define UI for dataset viewer application
shinyUI(fluidPage(
  # Application title
  titlePanel("Word Predict"),
  # Sidebar with controls to provide a caption, select a dataset,
  # and specify the number of observations to view. Note that
  # changes made to the caption in the textInput control are
  # updated in the output area immediately as you type
  sidebarLayout(
    sidebarPanel(
      helpText("Enter your text below, and a list of closest predictions 
               will appear in the dropdown menu below. Select your word and hit OK.\n
               HINT: If you end your phrase in a punctuation/space, the next whole 
               word will be predicted. Otherwise, prediction assumes the last 
               word is only a partial word."),
      textInput("caption", "Enter a phrase:", ""),
      uiOutput("predictions"),
      actionButton("action", "OK"),
      numericInput("obs", "Number of closest predictions:", 5)
    ),
    # Show the caption, a summary of the dataset and an HTML
    # table with the requested number of observations
    mainPanel(
      h3(textOutput("caption", container = span))
    )
  )
))