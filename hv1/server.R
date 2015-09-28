library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$gvMotion <- renderGvis({
    
  gvisMotionChart(allData,
                  idvar="Age",
                  timevar="Date",
                  yvar="Liabilities of individuals [Million ISK]",
                  xvar="Assets of individuals [Million ISK]",
                  sizevar="Total annual income [Million ISK]")
  
                                })
  })
