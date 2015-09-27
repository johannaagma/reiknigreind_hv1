shinyUI(pageWithSidebar(
  
  
  # Application title
  headerPanel("Data by agegroup"),
  
  sidebarPanel(
    
            p(class = "text-muted",
              paste("Hér kemur smá lýsing")
              
                )
               ),
  
  mainPanel(
    
    tableOutput("gvMotion")
    
           )
))