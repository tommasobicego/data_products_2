library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel(
    h3("Linear Regression: Miles/(US) gallon vs ...")
  ),
  
  sidebarPanel(
    h4(verbatimTextOutput("regressionFormula")),
    h4("Select the variables:"),
    checkboxInput("cyl", "Number of cylinders", TRUE),
    checkboxInput("disp", "Displacement (cu.in.)", TRUE),
    checkboxInput("hp", "Gross horsepower", TRUE),
    checkboxInput("drat", "Rear axle ratio", TRUE),
    checkboxInput("wt", "Weight (lb/1000)", TRUE),
    checkboxInput("qsec", "1/4 mile time", TRUE),
    checkboxInput("vs", "V/S", TRUE),
    checkboxInput("am", "Transmission", TRUE),
    checkboxInput("gear", "Number of forward gears", TRUE),
    checkboxInput("carb", "Number of carburetors", TRUE)
  ),
  
  mainPanel(
    verbatimTextOutput("summary"),
    plotOutput("ggplot", height = "700px"),
    plotOutput("residPlot")
  )
))
