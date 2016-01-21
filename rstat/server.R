library(RJDBC)
library(shiny)
library(ggplot2)
library(reshape)
library(plyr)
library(fmsb)
library(RColorBrewer)

require(rCharts)
#require(rNVD3)
#options(RCHART_WIDTH = 800)

colors <- brewer.pal(7, "Set3")
pal <- colorRampPalette(colors)
pal_list <- pal(15)

query_text_ref <- c()

shinyServer(function(input, output, session) {
  
  
  output$urlText <- renderText({
    paste(sep = "",
          "protocol: ", session$clientData$url_protocol, "\n",
          "hostname: ", session$clientData$url_hostname, "\n",
          "pathname: ", session$clientData$url_pathname, "\n",
          "port: ",     session$clientData$url_port,     "\n",
          "search: ",   session$clientData$url_search,   "\n"
    )
  })
  
  # Parse the GET query string
  output$queryText <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    
    # Return a string with key-value pairs
    query_text_ref = paste(names(query), query, sep = "=", collapse=", ")
    query_text_ref
    
  })
  
  output$data_table <- renderTable({
    query <- parseQueryString(session$clientData$url_search)
    
    # Return a string with key-value pairs
    kv_query = paste(names(query), query, sep = "=", collapse=", ")
    
    temp = get_df_from_grid_data(kv_query)
    names(temp) = c('date','name','cpu_avg','mem_usage','vif_rx_sum','vif_tx_sum','vbd_read_sum','vbd_write_sum')
    temp
  })
  
  
  output$plotTitle <- renderText({
    if(input$report_type=='Summary Report'){
      plot_title <- paste(input$year,'-',input$month,'- ',input$day,', Resource Usage Summary(by Zone)',sep='')
    }
    if(input$report_type=='Analysis Report') {
      plot_title <- paste(input$year,'-',input$month,'- ',input$day,', Resource Analysis Report',sep='')
    }
    plot_title
  })
  
  
  
  output$dailyPlot1 <- renderPlot({
    
    
    #sub_df <- subset(result_df,(Zone=='kr-1'))
    
    sub_df <- data()
    
    sub_df$time_of_day <- as.numeric(sub_df$time_of_day)
    
    #sub_df_day <- subset(result_perf_day,(the_year==input$year)&(month_of_year==input$month)&(day_of_month==input$day))    
    
    #base <- ggplot(sub_df_day,aes(x=vbd_read_sum,y=vbd_write_sum,colour=vm_zone_name),environment=environment())
    
    if(input$resource_type=='cpu'){
      #y_axis <- 'cpu_avg'
      base <- ggplot(sub_df,aes(x=time_of_day,y=cpu_avg,colour=vm_zone_name,group=vm_zone_name))
    }
    if(input$resource_type=='memory'){
      #y_axis <- 'mem_usage_avg'
      base <- ggplot(sub_df,aes(x=time_of_day,y=mem_usage_avg,colour=vm_zone_name,group=vm_zone_name))
    }
    if(input$resource_type=='vif'){
      #y_axis <- 'vif_rx_sum'
      base <- ggplot(sub_df,aes(x=time_of_day,y=vif_rx_sum,colour=vm_zone_name,group=vm_zone_name))
    }
    if(input$resource_type=='vbd'){
      #y_axis <- 'vbd_read_sum'
      base <- ggplot(sub_df,aes(x=time_of_day,y=vbd_read_sum,colour=vm_zone_name,group=vm_zone_name))
    }
    
    if(input$zone!='all'){
      sub_df <- subset(sub_df,vm_zone_name==input$zone)
      base <- base %+% sub_df
    }
    
    #base <- ggplot(sub_df_day,aes(x=vif_rx_sum,y=vif_tx_sum,colour=vm_zone_name))
    #base <- ggplot(sub_df,aes(x=time_of_day,y=cpu_avg,colour=vm_zone_name,group=vm_zone_name))
    #p2 <- base + geom_point()+geom_line(aes(colour=vm_zone_name))
    
    #p2 <- base + geom_point(aes(size=(sub_df_day$vif_rx_sum+sub_df_day$vif_tx_sum)))
    #p2 <- base + geom_line() + geom_point(size=3,aes(shape=vm_zone_name))+scale_shape_manual(values=c(seq(0:14)))
    
    if(input$showTrend=='no'){
      p2 <- base + geom_line() + geom_point(size=3,aes(shape=vm_zone_name))+scale_shape_manual(values=c(seq(0:14)))
    }
    # Show Trend line == Yes
    else{
      # 모든 zone을 그리는 경우, line을 없애고, 모든 zone에 대한 trend line 표시, se = F 
      if(input$zone=='all'){
        p2 <- base + geom_point(size=3,aes(shape=vm_zone_name))+scale_shape_manual(values=c(seq(0:14)))+stat_smooth(method=input$model,aes(col=vm_zone_name),se=FALSE)
      }
      # 특정 zone 만 그리는 경우 
      else{
        p2 <- base + geom_line() + geom_point(size=3,aes(shape=vm_zone_name))+scale_shape_manual(values=c(seq(0:14)))+stat_smooth(method=input$model)
      }
    }
    
    print(p2)
    
  })
  
  output$dailyPlot2 <- renderPlot({
    
    
    #sub_df <- subset(result_df,(Zone=='kr-1'))
    
    sub_df <- data()
    
    sub_df$time_of_day <- as.numeric(sub_df$time_of_day)
    
    #sub_df_day <- subset(result_perf_day,(the_year==input$year)&(month_of_year==input$month)&(day_of_month==input$day))    
    
    #base <- ggplot(sub_df_day,aes(x=vbd_read_sum,y=vbd_write_sum,colour=vm_zone_name),environment=environment())
    #y_axis=""
    if(input$resource_type=='cpu'){
      #y_axis <- 'cpu_max'
      base <- ggplot(sub_df,aes(x=time_of_day,y=cpu_max,colour=vm_zone_name,group=vm_zone_name))
    }
    if(input$resource_type=='memory'){
      #y_axis <- 'mem_usage_max'
      base <- ggplot(sub_df,aes(x=time_of_day,y=mem_usage_max,colour=vm_zone_name,group=vm_zone_name))
    }
    if(input$resource_type=='vif'){
      #y_axis <- 'vif_tx_sum'
      base <- ggplot(sub_df,aes(x=time_of_day,y=vif_tx_sum,colour=vm_zone_name,group=vm_zone_name))
    }
    if(input$resource_type=='vbd'){
      #y_axis <- 'vbd_write_sum'
      base <- ggplot(sub_df,aes(x=time_of_day,y=vbd_write_sum,colour=vm_zone_name,group=vm_zone_name))
    }
    
    if(input$zone!='all'){
      sub_df <- subset(sub_df,vm_zone_name==input$zone)
      base <- base %+% sub_df
    }
    
    #base <- ggplot(sub_df_day,aes(x=vif_rx_sum,y=vif_tx_sum,colour=vm_zone_name))
    #base <- ggplot(sub_df,aes(x=time_of_day,y=y_axis,colour=vm_zone_name,group=vm_zone_name))
    #p2 <- base + geom_point()+geom_line(aes(colour=vm_zone_name))
    
    #p2 <- base + geom_point(aes(size=(sub_df_day$vif_rx_sum+sub_df_day$vif_tx_sum)))
    
    if(input$showTrend=='no'){
      p2 <- base + geom_line() + geom_point(size=3,aes(shape=vm_zone_name))+scale_shape_manual(values=c(seq(0:14)))
    }
    # Show Trend line == Yes
    else{
      # 모든 zone을 그리는 경우, line을 없애고, 모든 zone에 대한 trend line 표시, se = F 
      if(input$zone=='all'){
        p2 <- base + geom_point(size=3,aes(shape=vm_zone_name))+scale_shape_manual(values=c(seq(0:14)))+stat_smooth(method=input$model,aes(col=vm_zone_name),se=FALSE)
      }
      # 특정 zone 만 그리는 경우 
      else{
        p2 <- base + geom_line() + geom_point(size=3,aes(shape=vm_zone_name))+scale_shape_manual(values=c(seq(0:14)))+stat_smooth(method=input$model)
      }
    }
    
    
    
    
    print(p2)
    
  })
  
  output$corplot <- renderPlot({
    #sub_df <- data()
    
    query <- parseQueryString(session$clientData$url_search)
    
    kv_query = paste(names(query), query, sep = "=", collapse=", ")
    
    temp = get_df_from_grid_data(kv_query)
    names(temp) = c('date','name','cpu_avg','mem_usage','vif_rx_sum','vif_tx_sum','vbd_read_sum','vbd_write_sum')
    # ggcorplot 함수 적용을 위해 numeric으로 변경 
    for(i in 3:8)temp[,i]  <- as.numeric(temp[,i])
    #g_plot <- ggcorplot(data = sub_df[c(6,8,10,11,12,13)],var_text_size = 5,cor_text_limits = c(5,10))
    g_plot <- ggcorplot(data = temp[c(3:8)],var_text_size = 5,cor_text_limits = c(5,10))
    print(g_plot)
  })
})
