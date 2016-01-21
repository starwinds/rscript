library(shiny)
library(ggplot2)

shinyUI(bootstrapPage(
  
  #h3("URL 콤포넌트"),
  #verbatimTextOutput("urlText"),
  
  #h3("Parsed query string"),
  #verbatimTextOutput("queryText")
  
  #tableOutput("queryText")
  #plotOutput("corplot")
  div(style="text-align:center",h3(textOutput("mainTitle"))),
  div(style="text-align:center",h3("전체 시간에 대한 Trend")),
  plotOutput("trendAllChart"),
  div(style="text-align:center",h3("시간 단위 별 세분화 Trend")),
  if("trendFacetChart" != 'none'){
    #div(style="text-align:center",h3("시간 단위 별 세분화 Trend")),
    plotOutput("trendFacetChart")
  }
  
)
)