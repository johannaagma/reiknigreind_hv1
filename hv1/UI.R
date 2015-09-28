shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Financial status between agegroups"),
  sidebarPanel(
    p(class = "text-muted",
      paste("This multivariate graph shows relations between financial status and 
            several social attributes within agegroups over time.
            The graph is interactive and you can change the axis at will."))
  ),
  mainPanel(tableOutput("gvMotion"))
))