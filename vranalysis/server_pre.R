library(shiny)
library(ggplot2)

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

result_df <- suppressWarnings(na_rm_from_csv('./splunkvr0604avg2.csv',3))
server_index <- unique(result_df[,1:3])
server_index <- server_index[order(server_index$Zone,server_index$Pod,server_index$Host),]
shinyServer(function(input, output) {
  output$zoneUI <- renderUI({     
    selectInput("zone", "Select Zone", c('Not Selected',as.character(unique(server_index[,1]))))
  })
  output$podUI <- renderUI({ 
    if(is.null(input$zone))return()
    data_zone <- input$zone
    conditionalPanel(condition = "input.zone != 'Not Selected'",
      selectInput("pod", "Select Pod", c('Not Selected',as.character(unique(subset(server_index,Zone==data_zone)$Pod)),selected=NULL))
    )
  })
  output$hostUI <- renderUI({ 
    if(is.null(input$zone))return()
    data_zone <- input$zone
    if(is.null(input$pod))return()
    data_pod <- input$pod
    conditionalPanel(condition = "input.pod != 'Not Selected'",
      selectInput("host", "Select Host", c('Not Selected',as.character(unique(subset(server_index,(Zone==data_zone)&(Pod==data_pod))$Host)),selected=NULL))
    )
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
