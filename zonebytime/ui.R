library(shiny)
library(ggplot2)
require(rCharts)
#require(rNVD3)
options(RCHART_LIB = 'polycharts')
options(RCHART_LIB = 'morris')
options(RCHART_LIB = 'nvd3')

zone_list <- unique(result_perf_day$vm_zone_name)
zone_list <- c('all',zone_list)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(enc2utf8("VM Performace Data(by Zone/by Day)")),
  
  sidebarPanel(
    #htmlOutput("yearUI"),
    #htmlOutput("monthUI"),
    #htmlOutput("dayUI")    
    selectInput("year", "Select Year", unique(result_time$the_year),selected='2013'),
    selectInput("month", "Select Month", unique(result_time$month_of_year),selected='6'),
    selectInput("day", "Select Day", unique(result_time$day_of_month),selected='1'),
    radioButtons(inputId = "report_type",label = "Select Report Type",choices=c('Summary Report','Analysis Report'),selected='Summary Report')  
  ),  
  mainPanel(
    
    h3("URL 콤포넌트"),
    verbatimTextOutput("urlText"),
    
    h3("Parsed query string"),
    verbatimTextOutput("queryText"),
    
    conditionalPanel(condition = "input.report_type=='Summary Report'",
    #if(input.report_type=='Summary Report'){
      div(style="text-align:center",h3(textOutput("plotTitle"))),
      
      div(class="row-fluid",div(class="span6",plotOutput("summary")),div(class="span6",plotOutput("summary2"))),
      div(class="row-fluid",div(class="span3",selectInput("sort_type", label="Choose Sort Order:",choices=c('ascending','descending'),selected='descending')),div(class="span3",selectInput("sort_pivot", label="Choose The Column:",choices=names(result_perf_day)[5:12],selected=names(result_perf_day)[5])),div(class="span3"),div(class="span3")),
      #selectInput("sort_type", label="Choose Sort Type:",choices=c('asc_sort','desc_sort'),selected='desc_sort'),
      #selectInput("sort_pivot", label="Choose The Column",choices=names(result_perf_day)[5:12],selected=names(result_perf_day)[5]),
      tableOutput("summary_table"),
      div(style="text-align:center",h3(textOutput("plotTitle2"))),
      #div(style="width:50%",showOutput("chart1", "polycharts")),
      #div(style="width:50%",showOutput("chart2","morris")),
      #div(class="row-fluid",div(class="span3",selectInput("resource_type", label="Choose The Resource:",choices=c('cpu','memory','vif','vbd'),selected='cpu')),div(class="span3",selectInput("zone", label="Choose The Zone:",choices=unique(result_perf_day$vm_zone_name),selected=unique(result_perf_day$vm_zone_name)[1])),div(class="span3"),div(class="span3")),
      div(class="row-fluid",div(class="span3",selectInput("resource_type", label="Choose The Resource:",choices=c('cpu','memory','vif','vbd'),selected='cpu')),div(class="span3",selectInput("zone", label="Choose The Zone:",choices=zone_list,selected=zone_list[1])),div(class="span3",radioButtons(inputId ="showTrend",label ="Show Trend Line",choices=c('yes','no'),selected = 'no')),div(class="span3",conditionalPanel(condition = "input.showTrend == 'yes'",radioButtons(inputId = "model",label = "Select Model",choices=c('lm','glm','loess'),selected='lm')))),
      div(style="text-align:center",h3(textOutput("plotTitle3"))),
      plotOutput("dailyPlot1"),
      div(style="text-align:center",h3(textOutput("plotTitle4"))),
      plotOutput("dailyPlot2")
      #showOutput("chart1","polycharts"),
      #showOutput("chart2","morris"),
      #showOutput("chart3","nvd3"),
      #plotOutput("testPlot")
    #}
    ),
    conditionalPanel(
      condition = "input.report_type=='Analysis Report'",
      div(style="text-align:center",h3(textOutput("plotTitle_a"))),
      div(style="width: 100% ; height: 500px",plotOutput("corplot"))
    )
  )
)
)