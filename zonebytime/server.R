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

shinyServer(function(input, output, session) {
  
    
  # data processing 
  data <- reactive({
            
    sub_df <- subset(result_perf,(the_year==input$year)&(month_of_year==input$month)&(day_of_month==input$day))
    sub_df$time_of_day <- as.character(sub_df$time_of_day)
    sub_df$vm_zone_name <- as.factor(sub_df$vm_zone_name)
    levels(sub_df) <- sort(unique(sub_df$vm_zone_name))
    sub_df 
  })  
  
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
    paste(names(query), query, sep = "=", collapse=", ")
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
  
  output$plotTitle2 <- renderText({
    plot_title <- paste(input$year,'-',input$month,'- ',input$day,', Daily Resource Usage (by Zone/by Time)',sep='')
    plot_title
  })
  
  output$plotTitle3 <- renderText({
    #plot_title <- paste(input$year,'-',input$month,'- ',input$day,', Daily Resource Usage (by Zone/by Time)',sep='')
    if(input$resource_type=='cpu'){
      plot_title <- 'CPU Usage AVG'
    }
    if(input$resource_type=='memory'){
      plot_title <- 'Memory Usage AVG'
    }
    if(input$resource_type=='vif'){
      plot_title <- 'vif_rx_sum'
    }
    if(input$resource_type=='vbd'){
      plot_title <- 'vbd_read_sum'
    }
    plot_title
  })
  
  output$plotTitle4 <- renderText({
    #plot_title <- paste(input$year,'-',input$month,'- ',input$day,', Daily Resource Usage (by Zone/by Time)',sep='')
    if(input$resource_type=='cpu'){
      plot_title <- 'CPU Usage Max'
    }
    if(input$resource_type=='memory'){
      plot_title <- 'Memory Usage Max'
    }
    if(input$resource_type=='vif'){
      plot_title <- 'vif_tx_sum'
    }
    if(input$resource_type=='vbd'){
      plot_title <- 'vbd_write_sum'
    }
    plot_title
  })
  
  output$plotTitle_a <- renderText({
    
    plot_title <- paste(input$year,'-',input$month,'- ',input$day,', Resource Analysis Report',sep='')
    
    plot_title
  })
  
  output$summary <- renderPlot({
    
    
    #sub_df <- subset(result_df,(Zone=='kr-1'))
    
    sub_df_day <- subset(result_perf_day,(the_year==input$year)&(month_of_year==input$month)&(day_of_month==input$day))    
    
    plot_title <- paste(input$year,'-',input$month,'- ',input$day,', Resource Usage Summary(per Zone)',sep='')
    
    sub_df_radar <- sub_df_day[,c(5,7,9,10,11,12)]
    rownames(sub_df_radar) <- sub_df_day$vm_zone_name
    
    t1 <- colwise(function(x)max(x))(sub_df_radar)
    t2 <- colwise(function(x)min(x))(sub_df_radar)
    t <- rbind(t1,t2)
    
    normalised_dat <- as.data.frame(mapply(
      function(x, mm)
      {
        (x - mm[2]) / (mm[1] - mm[2])
      },
      sub_df_radar,
      t
    ))
    
    
    
    # p <- radarchart(normalised_dat, axistype=1, seg=5, plty=1, maxmin=FALSE,title=plot_title)
    
    p <- radarchart(normalised_dat, axistype=1, seg=5, plty=1, maxmin=FALSE)
    print(p)
    
  })
  
  output$summary2 <- renderPlot({
    
    
    #sub_df <- subset(result_df,(Zone=='kr-1'))
    
    sub_df_day <- subset(result_perf_day,(the_year==input$year)&(month_of_year==input$month)&(day_of_month==input$day))    
    
    sub_df_radar <- sub_df_day[,c(5,7,9,10,11,12)]
    rownames(sub_df_radar) <- sub_df_day$vm_zone_name
    
    t1 <- colwise(function(x)max(x))(sub_df_radar)
    t2 <- colwise(function(x)min(x))(sub_df_radar)
    t <- rbind(t1,t2)
    
    normalised_dat <- as.data.frame(mapply(
      function(x, mm)
      {
        (x - mm[2]) / (mm[1] - mm[2])
      },
      sub_df_radar,
      t
    ))
    
    normalised_dat$id <- factor(rownames(sub_df_radar))
    
    long_dat <- melt(normalised_dat, id.vars = "id")
    
    colnames(long_dat)[1] <- c('zone_name')
    
    levels(long_dat$variable) <- c(levels(long_dat$variable), "")
    
    #p <- ggplot(long_dat, aes(x = variable, y = value, colour = zone_name,group = zone_name))+geom_line()+geom_point(size=5,aes(shape=zone_name))+scale_shape_manual(values=c(seq(0:14)))+coord_polar(theta = "x", direction = -1)
    p <- ggplot(long_dat, aes(x = variable, y = value, colour = zone_name,group = zone_name))+geom_line()+geom_point(size=3,aes(shape=zone_name))+scale_shape_manual(values=c(seq(0:14)))+coord_polar(theta = "x", direction = -1)
    
    print(p)
    
  })
  
  output$summary_table <- renderTable({
    sub_df_day <- subset(result_perf_day,(the_year==input$year)&(month_of_year==input$month)&(day_of_month==input$day))
    index <- which(names(sub_df_day)==as.character(input$sort_pivot))
    if(input$sort_type == 'descending'){
      sub_df_day <- sub_df_day[order(sub_df_day[,index],decreasing=TRUE),]
    }
    else{
      sub_df_day <- sub_df_day[order(sub_df_day[,index],decreasing=FALSE),]
    }
    
    rownames(sub_df_day) <- seq(1:nrow(sub_df_day))
    sub_df_day$the_year <- as.character(sub_df_day$the_year)
    sub_df_day$month_of_year  <- as.character(sub_df_day$month_of_year)
    sub_df_day$day_of_month <- as.character(sub_df_day$day_of_month)
    sub_df_day
  })  
  
  output$chart1 <- renderChart({
    
    sub_df <- data()
    
    #sub_df_day <- subset(result_perf_day,(the_year==input$year)&(month_of_year==input$month)&(day_of_month==input$day))    
    #sub_df_day$vif_rx_sum <- as.character(sub_df_day$vif_rx_sum)
    #sub_df_day$vif_tx_sum <- as.character(sub_df_day$vif_tx_sum)
    
    p1 <- rPlot(cpu_avg ~ time_of_day, group = 'vm_zone_name',color='vm_zone_name', type = 'point', data = sub_df)
    #p1$layer(x = "time_of_day", y = "cpu_avg", group = 'vm_zone_name',color=pal_list,data = sub_df, type = 'line')
    for(i in 1:length(levels(sub_df))){
      p1$layer(x = "time_of_day", y = "cpu_avg",data = subset(sub_df,vm_zone_name==levels(sub_df)[i]), type = 'line')
    }
    
    
    #p1 <- rPlot(vif_tx_sum ~ vif_rx_sum, color = 'vm_zone_name', type = 'point', data = sub_df_day)
    
    # r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = "point", color = "gear")
    
    # p1 <- rPlot(vif_tx_sum ~ vif_rx_sum | vm_zone_name, color = 'vm_zone_name', type = 'point', data = sub_df_day)
    
    #p1 <- rPlot(cpu_avg ~ time_of_day | vm_zone_name, type = 'point', data = sub_df)
    #p1$guides(x = list(title = "", ticks = unique(sub_df$time_of_)))
    
    #p1 <- rPlot(cpu_avg ~ time_of_day,data=sub_df,type='bar')
    #p1$layer(x = "time_of_day", y = "cpu_max", data = sub_df, type = 'point')
    
    p1$addParams(height = 300, dom = 'chart1')
    
    return(p1)
    
    
  })
  
  output$chart2 <- renderChart({
    
    sub_df <- data()
    
    sub_df_k1 <- subset(sub_df,vm_zone_name=='kr-1')
    
    
    
    m1 <- mPlot(x = "time_of_day", y = c("vbd_read_sum", "vbd_write_sum"), type = "Line", data = sub_df_k1)
    m1$set(pointSize = 1, lineWidth = 1)
    
    
    m1$addParams(height = 300, dom = 'chart2')
        
    
    return(m1)
    
    
  })
  
  output$chart3 <- renderChart({
    sub_df <- data()
    sub_df$vm_zone_name <- as.factor(sub_df$vm_zone_name)
    levels(sub_df) <- sort(unique(sub_df$vm_zone_name))
    
    #p1 <- rPlot(cpu_avg ~ time_of_day, group = 'vm_zone_name',color='vm_zone_name', type = 'point', data = sub_df)
    
    p3 <- nPlot(cpu_avg ~ time_of_day, group = 'vm_zone_name', data = sub_df, type = 'scatterChart')
    
    #p3 <- nvd3Plot(mpg ~ wt, group = 'cyl', data = mtcars, type = 'scatterChart')
    
    
    #p3$set(width = 550)
    return(p3)
    
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
    sub_df <- data()
    g_plot <- ggcorplot(data = sub_df[c(6,8,10,11,12,13)],var_text_size = 5,cor_text_limits = c(5,10))
    print(g_plot)
  })
})
