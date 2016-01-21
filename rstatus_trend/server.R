library(shiny)
library(ggplot2)
library(reshape)
library(plyr)
library(fmsb)
library(RColorBrewer)



colors <- brewer.pal(7, "Set3")
pal <- colorRampPalette(colors)
pal_list <- pal(15)

query_text_ref <- c()

shinyServer(function(input, output, session) {
  
  
  
  
  # Parse the GET query string
  
  # data frame을 table output 출력하기위해 renderTable로 호출 
  output$queryText <- renderTable({
    query <- parseQueryString(session$clientData$url_search)
    
    # Return a string with key-value pairs
    #query_text_ref = paste(names(query), query, sep = "=", collapse=", ")
    input_length = as.numeric(query$field_length)+1
    query_text_ref <- get_df_from_grid_data(query$test,input_length)
    
    as.data.frame(query_text_ref)
    
  })
  
  output$trendAllChart <- renderPlot({
    query <- parseQueryString(session$clientData$url_search)
    
    input_length = as.numeric(query$field_length)
    
    input_df <-get_df_from_grid_data(query$test,(input_length+1))
    
    
    input_interval = as.character(query$interval)
    input_type = as.numeric(query$input_type)
    
    result_chart <- get_trend('all',input_df,input_length,input_type,input_interval)
    print(result_chart)
  })
  
  output$trendFacetChart <- renderPlot({
    query <- parseQueryString(session$clientData$url_search)
    
    input_length = as.numeric(query$field_length)
    
    input_df <- get_df_from_grid_data(query$test,(input_length+1))
        
    input_interval = as.character(query$interval)
    input_type = as.numeric(query$input_type)
    
    #if(input_interval!='1d')result_chart <- get_trend('facet',input_df,input_length,input_type,input_interval)
    #else result_chart <- 'none'
    result_chart <- get_trend('facet',input_df,input_length,input_type,input_interval)
    print(result_chart)
  })
  
  output$mainTitle <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    input_type = as.numeric(query$input_type)
    
    if(input_type==0)main_title <- c('CPU Usage Trend (Max)')
    if(input_type==1)main_title <- c('Memory Usage Trend')
    if(input_type==2)main_title <- c('vbd_read Usage Trend (Sum)')
    if(input_type==3)main_title <- c('vbd_write Usage Trend (Sum)')
    if(input_type==4)main_title <- c('vif_rx Usage Trend (Sum)')
    if(input_type==5)main_title <- c('vif_tx Usage Trend (Sum)')
    
    
    main_title
  })
  
})
