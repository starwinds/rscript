library(shiny)
library(ggplot2)
library(reshape)
library(plyr)
library(fmsb)
temp_df <- c()
shinyServer(function(input, output, session) {
  
  # data processing 
  data <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    temp_df <- get_df_from_grid_data(query$test,8)
    sub_df <- temp_df[,c(3:8)]
    sub_df[,1:6] <- sapply(sub_df[,1:6],as.character)
    sub_df[,1:6] <- sapply(sub_df[,1:6],as.numeric)
    names(sub_df) <- c('cpu_avg','mem_usage','vif_rx_sum','vif_tx_sum','vbd_read_sum','vbd_write_sum')
    sub_df$cpu_avg <- round(sub_df$cpu_avg,3)
    sub_df$mem_usage <- round(sub_df$mem_usage,3)
    sub_df
  }) 
  
  # radar plot
  output$radarchart <- renderPlot({
    sub_df <- data()
    t1 <- colwise(function(x)max(x))(sub_df[,3:6])
    t2 <- colwise(function(x)min(x))(sub_df[,3:6])
    t <- rbind(t1,t2)
    normalised_dat <- sub_df[c(1:2)]
    normalised_dat <- cbind(normalised_dat,as.data.frame(mapply(
      function(x, mm)
      {
        diff <- (mm[1] - mm[2]);
        if(diff>0){(x - mm[2]) / (mm[1] - mm[2])}
        else{
          if(x==0) x
          else 1
        }
      },
      sub_df[c(3:6)],
      t
    )))
    
    rownames(sub_df) <- temp_df$V1
    normalised_dat$id <- factor(rownames(sub_df))
    long_dat <- melt(normalised_dat, id.vars = "id")
    colnames(long_dat)[1] <- c('date')
    levels(long_dat$variable) <- c(levels(long_dat$variable), "")
    
    p_radar <- ggplot(long_dat, aes(x = variable, y = value, colour = variable,group = date))+geom_point(size=3,aes(shape=variable))+geom_line()+scale_shape_manual(values=c(seq(0:5)))+coord_polar(theta = "x", direction = -1)
    p_radar <- p_radar + scale_y_continuous('usage_data') + scale_x_discrete('resource_type')
    print(p_radar)
  })
  
  # box plot
  output$boxplot <- renderPlot({
    sub_df <- data()
    t1 <- colwise(function(x)max(x))(sub_df[,3:6])
    t2 <- colwise(function(x)min(x))(sub_df[,3:6])
    t <- rbind(t1,t2)
    normalised_dat <- sub_df[c(1:2)]
    normalised_dat <- cbind(normalised_dat,as.data.frame(mapply(
      function(x, mm)
      {
        diff <- (mm[1] - mm[2]);
        if(diff>0){(x - mm[2]) / (mm[1] - mm[2])}
        else{
          if(x==0) x
          else 1
        }
      },
      sub_df[c(3:6)],
      t
    )))
    
    rownames(sub_df) <- temp_df$V1
    normalised_dat$id <- factor(rownames(sub_df))
    long_dat <- melt(normalised_dat, id.vars = "id")
    colnames(long_dat)[1] <- c('date')
   
    levels(long_dat$variable) <- c(levels(long_dat$variable), "")
    
    p_box <- ggplot(long_dat,aes(x=variable,y=value,fill=variable,outlier.colour='red')) + geom_boxplot() + geom_point() + scale_y_continuous('usage_data') + scale_x_discrete('resource_type')
    p_box <- p_box + theme(axis.text.x = element_text(angle=20,face="bold"))
    print(p_box)
    
  })
  
  # cor plot
  output$corplot <- renderPlot({
    sub_df <- data()
    #cor plot
    
    g_plot <- ggcorplot(data = sub_df,var_text_size = 5,cor_text_limits = c(5,9))
    print(g_plot)
  })
  
  
  
})

