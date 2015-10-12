shinyUI(pageWithSidebar(
  
  headerPanel("Financial status between agegroups"),
  sidebarPanel(
    strong("Description:"),
    p(class = "text-muted", paste("This multivariate graph shows relations between financial 
                                  status and several social attributes within agegroups over 
                                  time. The graph is interactive and you can change the axis 
                                  at will.")),
    
    strong("Authors:"),
    p(class = "text-muted", paste("Davíð Steinar Ásgrímsson, Jóhanna Agnes Magnúsdóttir")),
    
    strong("Source:"),
    p(class = "text-muted", paste("hagstofan.is, datamarket.com")),
    
    strong("Github:"),
    p(class = "text-muted", paste("https://github.com/johannaagma/reiknigreind_hv1"))
  ),
  mainPanel(tableOutput("gvMotion"))
))

