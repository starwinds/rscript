library(shiny)
library(ggplot2)
library(reshape)

na_rm_from_csv <- function(filepath,ncol){
  temp <- read.delim(filepath,header=TRUE,sep=',',na.strings='')
  index_list <- list()
  for (i in 1:ncol){
    index_list[[i]] <- which(is.na(temp[,i])==FALSE)
    index_length <- length(index_list[[i]])
    print(index_length)
    for (j in 1:index_length){
      start <- (index_list[[i]][j]+1)
      if(start>nrow(temp))start<-(start-1)
      if(j < index_length){
        if(start < index_list[[i]][j+1]){
          end <- (index_list[[i]][j+1]-1)  
        }
        else{
          end <- start
        }
      }
      else{
        end <- nrow(temp)
      }
      # 'NA'가 아닌 값을 갖는 ROW가 연속해서 있을 경우 (n,n+1), n+1이 n의 값으로 
      # 대체되어 버리는 문제 해결을 위해 검사 
      
      
      #if((start<end)||(temp[,i][start]=='NA'))temp[,i][start:end] <- temp[,i][start-1]
      #tryCatch({if((start<end)||(is.na(temp[,i][start])))temp[,i][start:end] <- temp[,i][start-1]},error=function(err){print(err);print(i);print(start);print(end)})      
      if((start<end)||(is.na(temp[,i][start])))temp[,i][start:end] <- temp[,i][start-1]
    }
  }
  temp  
}

find_hull <- function(df){
  df[chull(df$Avg_Of_ReceivedPackagets,df$Avg_Of_TransmitedPackets),]
}

result_df <- suppressWarnings(na_rm_from_csv('./splunkvr0604avg2.csv',3))
result_df$Pod <- as.factor(result_df$Pod)

server_index <- unique(result_df[,1:3])
server_index <- server_index[order(server_index$Zone,server_index$Pod,server_index$Host),]

cluster_index <<- ""

shinyServer(function(input, output) {
  
  output$zoneUI <- renderUI({     
    selectInput("zone", "Select Zone", c('Not Selected',as.character(unique(server_index[,1]))))
    #if(!is.null(input$zone)){
    #  if(input$zone == 'Not Selected'){
    #    cluster_index <<- c('zone',as.character(unique(server_index[,1])))
    #  }
    #}
  })
  output$podUI <- renderUI({ 
    if(is.null(input$zone))return()
        
    data_zone <- input$zone
    conditionalPanel(condition = "input.zone != 'Not Selected'",
      selectInput("pod", "Select Pod", c('Not Selected',as.character(unique(subset(server_index,Zone==data_zone)$Pod)),selected=NULL))
    )
    #if(!is.null(input$pod)){
    #  if(input$pod == 'Not Selected')cluster_index <<- c('pod',as.character(unique(subset(server_index,Zone==data_zone)$Pod)))
    #}
    #if(!is.null(input$zone)){
    #  if(input$zone == 'Not Selected'){
    #    cluster_index <<- c('zone',as.character(unique(server_index[,1])))
    #  }
    #}
  })
  output$hostUI <- renderUI({ 
    if(is.null(input$zone))return()
    data_zone <- input$zone
    if(is.null(input$pod))return()
    data_pod <- input$pod
    conditionalPanel(condition = "input.pod != 'Not Selected'",
      selectInput("host", "Select Host", c('Not Selected',as.character(unique(subset(server_index,(Zone==data_zone)&(Pod==data_pod))$Host)),selected=NULL))
    )
    #if(!is.null(input$host)){
    #  if(input$host == 'Not Selected')cluster_index <<- c('host',as.character(unique(subset(server_index,(Zone==data_zone)&(Pod==data_pod))$Host)))
    #}
  })
  output$percentUI <- renderUI({ 
    sliderInput(inputId="percent",label = "Choose the Percentile (Sum Of Rx/Tx):",min=10,max=100,value=100,step=10,animate=TRUE)
  })
  output$vrnameUI <- renderUI({ 
    checkboxInput(inputId = "vrname",
                  label = strong("Show VR Name"),
                  value = FALSE)
  })
  output$clusterUI <- renderUI({ 
    checkboxInput(inputId = "cluster",
                  label = strong("Show VR Cluster"),
                  value = FALSE)
  })
  output$clustersetUI <- renderUI({ 
    
    
    if(input$cluster==TRUE){
      if(!is.null(input$zone)){
        if(input$zone=='Not Selected'){
          zone_list <- as.character(unique(server_index[,1]))
          checkboxGroupInput(inputId = "cluster_zone_list",
                             label = "Select Zone",
                             choices=zone_list,
                             selected=zone_list[1]
                             )                  
        }
        else if(input$pod=='Not Selected') {
          data_zone <- input$zone
          pod_list <- as.character(unique(subset(server_index,Zone==data_zone)$Pod))
          checkboxGroupInput(inputId = "cluster_pod_list",
                             label = "Select Pod",
                             choices=pod_list,
                             selected=pod_list[1]
                             )   
        }
        else if(input$host=='Not Selected'){
          data_zone <- input$zone
          data_pod <- input$pod
          host_list <- as.character(unique(subset(server_index,(Zone==data_zone)&(Pod==data_pod))$Host))
          checkboxGroupInput(inputId = "cluster_host_list",
                             label = "Select Host",
                             choices=host_list,
                             selected=host_list[1]
          ) 
        }
        else{
          host <- as.character(input$host)
          checkboxGroupInput(inputId = "cluster_host_list",
                             label = "Select Host",
                             choices=host,
                             selected=host
          ) 
        }
        #conditionalPanel(condition = "input.zone=='Not Selected'",
        #                 checkboxGroupInput(inputId = "clusterlist",
        #                                    labe = "Select Cluster",
        #                                    choices=as.character(unique(server_index[,1]))                                        
        #                 )                     
        #)
        #conditionalPanel(condition = "input.pod=='Not Selected'",
        #                 checkboxGroupInput(inputId = "clusterlist",
        #                                    labe = "Select Cluster",
        #                                    choices=as.character(unique(server_index[,1]))                                        
        #                 )                     
        #)
      }
    }
  })
  
  output$showoptionUI <- renderUI({ 
    if(input$cluster==TRUE){
      checkboxInput(inputId = "showoption",
                    label = strong("Show All VR"),
                    value = TRUE)
    }
  })
  
  # data processing 
  data <- reactive({
    
    if(input$zone=='Not Selected')data_zone <- NA
    else data_zone <- as.character(input$zone)
    
    if((is.na(input$zone)==TRUE)|(input$pod=='Not Selected'))data_pod <- NA
    else data_pod <- as.character(input$pod)
    
    if((is.na(input$pod)==TRUE)|(input$host=='Not Selected'))data_host <- NA
    else data_host <- as.character(input$host)
    
        
    percentile <- input$percent/100
    
    if(!is.na(data_host))sub_df <- subset(result_df,(Zone==data_zone)&(Pod==data_pod)&(Host==data_host))
    else if(!is.na(data_pod))sub_df <- subset(result_df,(Zone==data_zone)&(Pod==data_pod))
    else if(!is.na(data_zone))sub_df <- subset(result_df,Zone==data_zone)
    else sub_df <- result_df
    #sub_df <- subset(result_df,(Zone==data_zone)&(Pod==data_pod))
    q <- quantile(sub_df[,'Avg_Of_SumPackets'],percentile)    
    sub_df <- subset(sub_df,Avg_Of_SumPackets<=q)
    sub_df
  })  
  output$mainPlot <- renderPlot({
    
    sub_df <- data()
    #sub_df <- subset(result_df,(Zone=='kr-1'))
    
    
    base <- ggplot(sub_df,aes(x=Avg_Of_ReceivedPackagets,y=Avg_Of_TransmitedPackets),environment=environment())
      
    plot <- base + geom_point(aes(size=Avg_Of_SumPackets), colour='orangered',alpha=I(0.8)) + scale_size(range=c(3,7))
    
    if(input$vrname==TRUE)plot <- plot + geom_text(aes(label=VR,size=Avg_Of_SumPackets),hjust=1,vjust=-1,angle=25) 
    
    # Show VR Cluster
    if(input$cluster==TRUE){
      #if(!is.na(input$cluster_zone_list)){
      if(input$zone=='Not Selected'){
        sub_df_polygon <- subset(sub_df,Zone %in% as.character(input$cluster_zone_list))
        hulls <- ddply(sub_df_polygon, "Zone", find_hull)
        plot <- plot + geom_polygon(data = hulls, aes(colour=Zone,fill=Zone),alpha=0.5)
      }
      #if(!is.na(input$cluster_pod_list)){
      else if(input$pod=='Not Selected'){
        sub_df_polygon <- subset(sub_df,Pod %in% as.character(input$cluster_pod_list))
        hulls <- ddply(sub_df_polygon, "Pod", find_hull)
        plot <- plot + geom_polygon(data = hulls, aes(colour=Pod,fill=Pod),alpha=0.5)
      }
      else{
        sub_df_polygon <- subset(sub_df,Host %in% as.character(input$cluster_host_list))
        hulls <- ddply(sub_df_polygon, "Host", find_hull)
        plot <- plot + geom_point(data=sub_df_polygon,aes(colour=Host)) + geom_polygon(data = hulls, aes(colour=Host,fill=Host),alpha=0.5)
      }
    }
    
    #plot <- base + geom_point(colour='orangered',alpha=I(0.8)) + geom_text(aes(label=VR),hjust=1,vjust=-1,angle=25) + scale_size(range=c(3,7))
    
    # plot <- plot + geom_smooth(se=F)
    
    # plot <- qplot(x=Sum_Of_ReceivedPackets,y=Sum_Of_TransmitedPackets,data=sub_df,geom="point")
    print(plot)
    
  })
  output$data_table <- renderTable({
    df <- data()
    df <- df[order(df$Avg_Of_SumPackets,decreasing=TRUE),]
    df
  })  
})
