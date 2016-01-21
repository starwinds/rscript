library(shiny)
library(ggplot2)
require(rCharts)
#require(rNVD3)
options(RCHART_LIB = 'polycharts')
options(RCHART_LIB = 'morris')
options(RCHART_LIB = 'nvd3')



shinyUI(bootstrapPage(
  
  # Application title
  
    
    #h3("URL 콤포넌트"),
    #verbatimTextOutput("urlText"),
    
    #h3("Parsed query string"),
    #verbatimTextOutput("queryText"),
    
    #tableOutput("data_table"),
    plotOutput("corplot")
    
  
)
)