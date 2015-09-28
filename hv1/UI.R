shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Data by agegroup"),
  sidebarPanel(
    p(class = "text-muted",
      paste("here comes description"))
  ),
  mainPanel(tableOutput("gvMotion"))
))