library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(enc2utf8("Virtual Router N/W Usage")),
  
  sidebarPanel(
    htmlOutput("zoneUI"),
    htmlOutput("podUI"),
    htmlOutput("hostUI"),
    htmlOutput("percentUI"),
    htmlOutput("vrnameUI"),
    htmlOutput("clusterUI"),
    htmlOutput("clustersetUI"),
    htmlOutput("showoptionUI")
  ),
  mainPanel(
    
    div(class="span10",plotOutput("mainPlot")),
    div(class="span10",tableOutput("data_table"))
  )
)
)