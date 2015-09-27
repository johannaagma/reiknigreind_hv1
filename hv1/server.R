library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$gvMotion <- renderGvis({
    
  gvisMotionChart(total, 
                  idvar="Age", 
                  timevar="Date",
                  )
  
                                })
  })
