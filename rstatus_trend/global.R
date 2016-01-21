library(ggplot2)
library(ProgGUIinR)
# co-relation 분석을 위함 
ezLev=function(x,new_order){
  for(i in rev(new_order)){
    x=relevel(x,ref=i)
  }
  return(x)
}

ggcorplot = function(data,var_text_size,cor_text_limits,smooth_method='lm'){
  # normalize data
  for(i in 1:length(data)){
    data[,i]=(data[,i]-mean(data[,i]))/sd(data[,i])
  }
  # obtain new data frame
  z=data.frame()
  i = 1
  j = i
  while(i<=length(data)){
    if(j>length(data)){
      i=i+1
      j=i
    }else{
      x = data[,i]
      y = data[,j]
      temp=as.data.frame(cbind(x,y))
      temp=cbind(temp,names(data)[i],names(data)[j])
      z=rbind(z,temp)
      j=j+1
    }
  }
  names(z)=c('x','y','x_lab','y_lab')
  z$x_lab = ezLev(factor(z$x_lab),names(data))
  z$y_lab = ezLev(factor(z$y_lab),names(data))
  z=z[z$x_lab!=z$y_lab,]
  #obtain correlation values
  z_cor = data.frame()
  i = 1
  j = i
  while(i<=length(data)){
    if(j>length(data)){
      i=i+1
      j=i
    }else{
      x = data[,i]
      y = data[,j]
      x_mid = min(x)+diff(range(x))/2
      y_mid = min(y)+diff(range(y))/2
      this_cor = cor(x,y)
      this_cor.test = cor.test(x,y)
      this_col = ifelse(this_cor.test$p.value<.05,'<.05','>.05')
      this_size = (this_cor)^2
      # substr index 수정 : left side 출력되도록 1일경우를 고려 
      cor_text = ifelse(
        this_cor>0
        ,substr(format(c(this_cor,.123456789),digits=2)[1],1,4)
        ,paste('-',substr(format(c(this_cor,.123456789),digits=2)[1],2,5),sep='')
      )
      b=as.data.frame(cor_text)
      b=cbind(b,x_mid,y_mid,this_col,this_size,names(data)[j],names(data)[i])
      z_cor=rbind(z_cor,b)
      j=j+1
    }
  }
  names(z_cor)=c('cor','x_mid','y_mid','p','rsq','x_lab','y_lab')
  z_cor$x_lab = ezLev(factor(z_cor$x_lab),names(data))
  z_cor$y_lab = ezLev(factor(z_cor$y_lab),names(data))
  diag = z_cor[z_cor$x_lab==z_cor$y_lab,]
  z_cor=z_cor[z_cor$x_lab!=z_cor$y_lab,]
  #start creating layers
  points_layer = layer(
    geom = 'point'
    , data = z
    , mapping = aes(
      x = x
      , y = y
    )
  )
  lm_line_layer = layer(
    geom = 'line'
    , geom_params = list(colour = 'red')
    , stat = 'smooth'
    , stat_params = list(method = smooth_method)
    , data = z
    , mapping = aes(
      x = x
      , y = y
    )
  )
  lm_ribbon_layer = layer(
    geom = 'ribbon'
    , geom_params = list(fill = 'green', alpha = .5)
    , stat = 'smooth'
    , stat_params = list(method = smooth_method)
    , data = z
    , mapping = aes(
      x = x
      , y = y
    )
  )
  cor_text = layer(
    geom = 'text'
    , data = z_cor
    , mapping = aes(
      x=y_mid
      , y=x_mid
      , label=cor
      , size = rsq
      , colour = p
    )
  )
  var_text = layer(
    geom = 'text'
    , geom_params = list(size=var_text_size)
    , data = diag
    , mapping = aes(
      x=y_mid
      , y=x_mid
      , label=x_lab
    )
  )
  f = facet_grid(y_lab~x_lab,scales='free')
  o = theme(
    panel.grid.minor = element_blank()
    ,panel.grid.major = element_blank()
    ,axis.ticks = element_blank()
    ,axis.text.y = element_blank()
    ,axis.text.x = element_blank()
    ,axis.title.y = element_blank()
    ,axis.title.x = element_blank()
    ,legend.position='none'
  )
  #size_scale = scale_size(limits = c(0,1),to=cor_text_limits)
  size_scale = scale_size(limits = c(0,1),range=cor_text_limits)
  return(
    ggplot()+
      points_layer+
      lm_ribbon_layer+
      lm_line_layer+
      var_text+
      cor_text+
      f+
      o+
      size_scale
  )
}

get_df_from_grid_data = function(input_str,field_length){
  
  
  temp_array = strsplit(input_str,',')
  n = length(temp_array[[1]])/field_length
  result_df = data.frame()
  for(i in 0:(n-1)){
    temp_row = c()
    for(j in 1:field_length){
      temp_row = cbind(temp_row,temp_array[[1]][i*field_length+j])
    }
    result_df = rbind(result_df,temp_row)
  }
  result_df
}

get_trend <- function(type,input_df,input_col_length,input_type,interval){
  # date 값의 길이를 맞추기 위한 작업 (ex. 1/01 11:15 -> 01/01 11:15)
  input_df$V1 <- as.character(input_df$V1)
  if(nchar(input_df$V1[1])==10)input_df$V1 <- paste('0',input_df$V1,sep='')
  
  input_df[,2:(input_col_length+1)] <- sapply(input_df[,2:(input_col_length+1)],as.character)
  input_df[,2:(input_col_length+1)] <- sapply(input_df[,2:(input_col_length+1)],as.numeric)
  # cpu
  if(input_type==0){
    cpu_max <- c()
    for(i in 1:nrow(input_df)){
      cpu_max <- rbind(cpu_max,max(input_df[i,c(2:(input_col_length+1))]))
      #cpu_max <- round(cpu_max,3)*100
    }
    input_df <- cbind(input_df,cpu_max)
    input_df$cpu_max <- round(input_df$cpu_max,3)*100
  }
  # memory
  else if(input_type==1){
    memory_usage <- c()
    for(i in 1:nrow(input_df)){
      memory_usage <- rbind(memory_usage,(input_df[i,2]-input_df[i,3])/input_df[i,2])
      #memory_usage <- round(memory_usage,3)*100
    }
    input_df <- cbind(input_df,memory_usage)
    input_df$memory_usage <- round(input_df$memory_usage,3)*100
  }
  # vif,vbd
  else{
    sum <- c()
    for(i in 1:nrow(input_df)){
      sum <- rbind(sum,sum(input_df[i,c(2:(input_col_length+1))]))
    }
    input_df <- cbind(input_df,sum)
  }
  if(type=='facet'){
    # interval에 따라 time facet을 다르게 설정하기 위한 작업 
    if(interval=='5m'){
      # facet : 시간
      # 차트 시간 단위 : 분 
      input_df <- cbind(input_df,substr(input_df$V1,1,8))
      names(input_df)[ncol(input_df)] <- 'time'
      
      input_df <- cbind(input_df,substr(input_df$V1,10,11))
      names(input_df)[ncol(input_df)] <- 'minute'
      
      
      
      if(input_type==0)base <- ggplot(input_df,aes(x=minute,y=cpu_max,group=1)) + scale_y_continuous('cpu max(%)')
      else if(input_type==1)base <- ggplot(input_df,aes(x=minute,y=memory_usage,group=1)) + scale_y_continuous('memory_usage(%)')
      else base <- ggplot(input_df,aes(x=minute,y=sum,group=1))
      result_chart <- base + geom_point()+geom_line() + stat_smooth(method='loess') + facet_grid(time~.)
      
    }
    if(interval=='1h'){
      # facet : 일
      # 차트 시간 단위 : 시간 
      input_df <- cbind(input_df,substr(input_df$V1,1,5))
      names(input_df)[ncol(input_df)] <- 'date'
      
      input_df <- cbind(input_df,substr(input_df$V1,7,8))
      names(input_df)[ncol(input_df)] <- 'hour'
      
      if(input_type==0)base <- ggplot(input_df,aes(x=hour,y=cpu_max,group=1)) + scale_y_continuous('cpu max(%)')
      else if(input_type==1)base <- ggplot(input_df,aes(x=hour,y=memory_usage,group=1)) + scale_y_continuous('memory_usage(%)')
      else base <- ggplot(input_df,aes(x=hour,y=sum,group=1))
      result_chart <- base + geom_point()+geom_line() + stat_smooth(method='loess') + facet_grid(date~.)
      
    }
    if(interval=='1d'){
      # facet : 주 
      # 차트 시간 단위 : 요일  
      #input_df <- cbind(input_df,substr(input_df$V1,1,5))
      input_df$V1 <- as.character(input_df$V1)
#       w_split <- strsplit(input_df$V1,';')
#       t_col <- c()
#       for(i in 1:length(w_split))t_col <- rbind(t_col,w_split[[i]][2])
#       input_df <- cbind(input_df,t_col)
#       names(input_df)[ncol(input_df)] <- 'weekday'
#       input_df$weekday <- as.character(input_df$weekday)
      # facet_grid y label을 위한 array     
      m_array <- c('Jan(1)','Feb(2)','Mar(3)','Apr(4)','May(5)','Jun(6)','Jul(7)','Aug(8)','Sep(9)','Oct(10)','Nov(11)','Dec(12)');
      # facet_grid x label을 위한 tick,label
      t <- seq(1:7)
      l <- c('sun','mon','tue','wed','thur','fri','sat')
      ticks <- data.frame(t=t,l=l)
      ticks$l <- as.character(ticks$l)
      # 주, 요일 column 추가 
      t_col <- c()
      t_col2 <- c()
      #wd <- c('sun','mon','tue','wed','thur','fri','sat')
      for(i in 1:nrow(input_df)){
        temp_year <- as.numeric(strsplit(input_df$V1[i],' ')[[1]][1])
        temp_month <- as.numeric(strsplit(strsplit(input_df$V1[i],' ')[[1]][2],'/')[[1]][1])
        temp_day <- as.numeric(strsplit(strsplit(input_df$V1[i],' ')[[1]][2],'/')[[1]][2])
        week_number = (week.of.month(temp_year,temp_month,temp_day)+1)
        #t_col <- rbind(t_col,(week.of.month(temp_year,temp_month,temp_day)+1))
        t_col <- rbind(t_col,paste(m_array[temp_month],' week',week_number,sep=''))
        t_col2 <- rbind(t_col2,(day.of.week(temp_year,temp_month,temp_day)+1))
      }
      input_df <- cbind(input_df,t_col)      
      names(input_df)[ncol(input_df)] <- 'week'      
      input_df$week <- as.character(input_df$week)
      input_df <- cbind(input_df,t_col2)      
      names(input_df)[ncol(input_df)] <- 'weekday'      
      #input_df$weekday <- as.character(input_df$weekday)
      
#       for(i in 1:length(w_split))t_col <- rbind(t_col,paste(m_array[as.numeric(substr(w_split[[i]][1],1,2))],' week',w_split[[i]][3],sep=''))
#       input_df <- cbind(input_df,t_col)      
#       names(input_df)[ncol(input_df)] <- 'week'      
#       input_df$week <- as.character(input_df$week)
      
      # x axis tick & label 
      t <- seq(0:6)
      l <- c('sun','mon','tue','wed','thur','fri','sat')
      ticks <- data.frame(t=t,l=l)
      ticks$l <- as.character(ticks$l)
      
      if(input_type==0)base <- ggplot(input_df,aes(x=weekday,y=cpu_max,group=1)) + scale_y_continuous('cpu max(%)')
      else if(input_type==1)base <- ggplot(input_df,aes(x=weekday,y=memory_usage,group=1)) + scale_y_continuous('memory_usage(%)')
      else base <- ggplot(input_df,aes(x=weekday,y=sum,group=1))
      result_chart <- base + geom_point()+geom_line() + stat_smooth(method='loess') + facet_grid(week~.) +scale_x_discrete(breaks=c(ticks$t),labels=c(ticks$l)) 
      #result_chart <- base + geom_point()+geom_line() + stat_smooth(method='loess') + facet_grid(week~.)
      
    }
  }
  if(type=='all'){
    names(input_df)[1] <- 'date'
    input_df$date <- substr(input_df$date,1,11)
    
    t <- 2*(seq(1:round(nrow(input_df)/2,0))-1)+1
    l <- input_df$date[t]
    ticks <- data.frame(t=t,l=l)
    ticks$l <- as.character(ticks$l)
    
    # tick으로 x axis label을 조정하기 위해 rownames 사용 
    rw <- c()
    rw <- rownames(input_df)
    rw <- as.numeric(rw)
    
    if(input_type==0){base <- ggplot(input_df,aes(x=as.numeric(rownames(input_df)),y=cpu_max,group=1),environment=environment()) + scale_y_continuous('cpu max')}
    else if(input_type==1){base <- ggplot(input_df,aes(x=as.numeric(rownames(input_df)),y=memory_usage,group=1),environment=environment()) + scale_y_continuous('memory_usage')}
    else {base <- ggplot(input_df,aes(x=as.numeric(rownames(input_df)),y=sum,group=1),environment=environment())}
    
#     if(input_type==0)base <- ggplot(input_df,aes(x=date,y=cpu_max,group=1)) + scale_y_continuous('cpu max')
#     else if(input_type==1)base <- ggplot(input_df,aes(x=date,y=memory_usage,group=1)) + scale_y_continuous('memory_usage')
#     else base <- ggplot(input_df,aes(x=date,y=sum,group=1))
    
    #result_chart <- base + geom_point()+geom_line() + stat_smooth(method='loess') +theme(axis.text.x = element_text(angle=45,size=10,face="bold",color="orangered"))
    result_chart <- base + geom_point()+geom_line() + stat_smooth(method='loess') +theme(axis.text.x = element_text(angle=45,size=10,face="bold",color="orangered"))+scale_x_discrete('date',breaks=c(ticks$t),labels=c(ticks$l)) 
    
  }
  result_chart
}
