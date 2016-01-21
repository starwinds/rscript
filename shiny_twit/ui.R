library(shiny)

s_date <- substr(Sys.time(),1,10)
index_data <- c()
index_date <<- character(length=8)
for(i in 1:8)index_date[i]<<-as.character((as.Date(s_date)-8+i))
date_list <- as.list(index_date)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(enc2utf8("Tweet Analysis")),
  
  sidebarPanel(
    textInput("keyword","Search KeyWord",enc2utf8("cloud")),
    radioButtons("lang", "Language",
                 c("Korean" = "ko",
                   "English" = "en")),
    conditionalPanel(
      condition = "input.lang == 'en' ",
      radioButtons("sentiment", "Want Sentiment Analysis?",
                   list("yes","no"),selected="no"),
      conditionalPanel(
        condition = "input.sentiment == 'yes' ",
        radioButtons("sentiment_kind", "Extract Words for:",
                     list("positive","negative"))        
      )      
    ),
    radioButtons("chooseDate", "time criteria",
                 list("daily",
                      "term"),selected="daily"),
    conditionalPanel(
      condition = "input.chooseDate == 'daily' ",
      textOutput("datecaption"),
      sliderInput("input_date","",min=1,max=8,value=8,step=1,animate=TRUE)      
    ),
    
    conditionalPanel(
      condition = "input.chooseDate == 'term' ",
      selectInput("startdate","Choose start date",date_list),
      selectInput("enddate","Choose end date",date_list,selected=date_list[[length(date_list)]])
    ),
    
    numericInput("number", "Number of Tweets", 300),
    textInput("removeword","Remove Words"),
    conditionalPanel(
      condition = "(input.sentiment == 'no')||(input.lang == 'ko') ",  
      sliderInput("min_freq",
                  enc2utf8("Wordcloud - Number Of Min. Freq"),
                  min=1,
                  max=10,
                  value =10
      ),
      sliderInput("min_freq_2nd",
                  enc2utf8("Barplot - Number Of Min. Freq (boxplot stat)"),
                  min=0,
                  max=5,
                  value =0
      )
    ),
    selectInput("vtype","Choose Visual Type",list('wordcloud','hclust','heatmap')),
    conditionalPanel(
      condition = "input.vtype == 'wordcloud' ",
      radioButtons("wctype", "Visual Type of Wordcloud",
                   list("decreasing",
                        "random"),selected="decreasing")
    )
    
    #submitButton("Update Result")
  ),
  mainPanel(
    includeHTML("piwik.js"),
    conditionalPanel(
      condition = "input.sentiment == 'yes' ",
      h3(textOutput("caption"))       
    ),    
    #plotOutput("tweetPlot"),
    #plotOutput("tweetbarPlot")
    #textOutput("datecaption"),
    div(class="span8", plotOutput("tweetPlot")),
    div(class="span8", plotOutput("tweetbarPlot"))
    
  )
  
  ))

