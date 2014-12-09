# WordPredict demo

**NOTE:** Requires Shiny 0.9.0 or later.

WordPredict allows users to write text via a combination of text entry and pulldown selection. Words and sentences are completed using a backoff trigram model, coupled with context aware prediction. 

This is a work in progress, and may be updated occasionally.

See the WordPredict demo live at https://ardecarlo.shinyapps.io/WordPredict/.

Or:
1. Fork the repository at  https://github.com/ardecarlo/WordPredict,
2. Open RStudio,
3. type 'library(shiny)'
4. setwd() to the directory above where the WordPredict folder is located, and 
5. type 'runApp("CensusVis")'