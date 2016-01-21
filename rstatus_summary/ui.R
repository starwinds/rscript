library(shiny)
library(ggplot2)

shinyUI(bootstrapPage(
  div(style="text-align:center",h3("리소스 타입 별 Usage Data간의 상관 관계 분석")),
  plotOutput("corplot"),
  div(style="text-align:center",h3("Usage Data 분포 요약")),
  div(class="row-fluid",div(class="span6",plotOutput("radarchart")),div(class="span6",plotOutput("boxplot")))
  #div(style="text-align:center",h3("리소스 타입 별 Usage Data간의 상관 관계 분석")),
  #plotOutput("corplot")
))