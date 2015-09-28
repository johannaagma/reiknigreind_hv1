shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Data by age"),
  sidebarPanel(),
  mainPanel(tableOutput("gvMotion"))
))