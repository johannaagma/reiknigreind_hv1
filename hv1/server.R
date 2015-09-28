# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$gvMotion <- renderGvis({
    gvisMotionChart(allData,
                    idvar="Age", 
                    timevar="Date")
    })
  })
