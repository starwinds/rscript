
library(shiny)
library(twitteR)
library(RJSONIO)
library(ROAuth)
library(tm)
library(wordcloud)
library(ggplot2)
library(KoNLP)
library(grid)

# 전역 변수 선언 : 같은 검색 조건일 경우, Twitter 검색/단어 파싱하는 과정을 생략하기 위함 
pre_input_word <<- ""
pre_input_number <<- 0
searchNoun <<- c()
pre_removeword <<- c()
pre_input_date <<- ""
pre_lang <<- ""
# wordcount <<- c()

score <<- 0
pos.m.score <<-0
neg.m.score <<-0

# 단어 판별 기준 데이터 (English기준)
pos <- scan('./pos_neg_english/positive-words.txt',what='character',comment.char=';')
neg <- scan('./pos_neg_english/negative-words.txt',what='character',comment.char=';')


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  formulaText1 <- reactive(function() {
    # paste("Sentiment Score(Pos-Neg) : ",as.character(score),", ",input$sentiment_kind," word cloud",sep="")
    paste("Sentiment Analysis : ", input$sentiment_kind," word cloud",sep="")
    #if(input$sentiment_kind=='positive')paste("Sentiment Score (",input$sentiment_kind,"): ",as.character(pos.m.score),", ",input$sentiment_kind," word cloud",sep="")
    #else paste("Sentiment Score (",input$sentiment_kind,"): ",as.character(neg.m.score),", ",input$sentiment_kind," word cloud",sep="")
  })
  
  # Return the formula text for printing as a caption
  output$caption <- reactiveText(function() {
    formulaText1()
  })
  
  formulaText2 <- reactive(function() {
    paste("Search Date : ", index_date[input$input_date],sep="")
  })
  
    
  # Return the formula text for printing as a caption
  output$datecaption <- reactiveText(function() {
    formulaText2()
  })
  
  # data processing 
  data <- reactive(function(){
    # 새로운 단어/새로운 트윗 수를 검색하는 것인지 확인 
    is_new <- 'FALSE'
    if((input$keyword!=pre_input_word)|(input$number!=pre_input_number)|(index_date[input$input_date]!=pre_input_date)|(input$lang!=pre_lang))is_new <- 'TRUE'
    
    #if(input$lang=='ko')input$sentiment<-'no'
    
    if(is_new == 'TRUE'){
      pre_input_word <<- input$keyword
      pre_input_numer <<- input$number
      pre_input_date <<- index_date[input$input_date]
      pre_lang <<- input$lang
      pre_removeword <<- c()
      
      searchNoun <<- c()
      if(input$chooseDate=='daily')search_result <- searchTwitter(input$keyword,n=input$number,lang=input$lang,since=index_date[input$input_date],until=as.character(as.Date(index_date[input$input_date])+1))
      else search_result <- searchTwitter(input$keyword,n=input$number,lang=input$lang,since=input$startdate,until=as.character(as.Date(input$enddate)+1))
      
      searchtw <- c()
      for(i in 1:length(search_result)){
        searchtw <- append(searchtw, search_result[[i]]$text)
      }
      
      
      searchtw <- gsub("[[:space:]]"," ", searchtw)
      
      # @트윗 태그 제거
      searchtw <- gsub("@[[:graph:]]*","", searchtw)
      
      # URL 제거
      searchtw <- gsub("http://[[:graph:]]*","", searchtw)
      searchtw <- gsub("https://[[:graph:]]*","", searchtw)
      
      useSejongDic()
      
      searchNoun <<- sapply(searchtw, extractNoun,USE.NAMES=F)
      
      searchNoun <<- unlist(searchNoun, use.name=F)
      
      searchNoun <<- searchNoun[-which(tolower(searchNoun) %in% c(stopwords("english"),"rt"))]
      if((input$sentiment=='yes')&(input$lang=='en')){
        pos.match <- c()
        neg.match <- c()
        pos.match <- match(tolower(searchNoun),pos)
        pos_index <- which(!is.na(pos.match))
        neg.match <- match(tolower(searchNoun),neg)
        neg_index <- which(!is.na(neg.match))
        pos.m.score <<- !is.na(pos.match)
        pos.m.score <<- sum(pos.m.score)
        neg.m.score <<- !is.na(neg.match)
        neg.m.score <<- sum(neg.m.score)
        score <<- 0
        score <<- (pos.m.score-neg.m.score)
        
        #formulaText1()
        
        if(input$sentiment_kind=='positive'){          
          searchNoun <<- searchNoun[pos_index]
        }
        if(input$sentiment_kind=='negative'){          
          searchNoun <<- searchNoun[neg_index]
        }
        
      }
      
      
      # 단어 수 5이하, 2이상 : 단어가 너무 길면 bar plot 표시에 문제..
      searchNoun <<- Filter(function(x){(nchar(x)<=5)&(nchar(x)>=2)}, searchNoun)
      searchNoun <<- gsub("[[:punct:]]", "", searchNoun)
      
      # 검색 키워드에 해당하는 단어는 제외 
      temp <- tolower(input$keyword)
      temp <- unlist(strsplit(temp,' '))
      index <- which(tolower(searchNoun) %in% temp)
      if(length(index)!=0)searchNoun <<- searchNoun[-index]
      #for(i in 1:length(temp)){
      #  index <- which(tolower(searchNoun)==temp[i])
      #  if(length(index)!=0)searchNoun <<- searchNoun[-index]
      #}
      #searchNoun <<- searchNoun[-which(tolower(searchNoun)==input$keyword)]
    }
    if(input$removeword!=""){
      # length(pre_removeword)==0 이면 새로운 검색이 시작되어, remove word를 입력한 것이므로 단어 삭제 작업 진행 
      # length(pre_removeword)==0 이 아니면, 기존 삭제 작업한 단어 리스트와 다를 경우에 만 삭제 진행 
      # || 일 경우에만 좌측이 true면 바로 if문 안 실행함 -> | 는 error 남 
      if((length(pre_removeword)==0)||(input$removeword != pre_removeword)){
        
        pre_removeword <<- input$removeword
        
        temp <- tolower(input$removeword)
        temp <- unlist(strsplit(temp,' '))
        index <- which(tolower(searchNoun) %in% temp)
        if(length(index)!=0)searchNoun <<- searchNoun[-index]
        #for(i in 1:length(temp)){
        #  index <- which(tolower(searchNoun)==temp[i])
        #  if(length(index)!=0)searchNoun <<- searchNoun[-index]
        #}
      }
    }
    wordcount <- table(searchNoun)
    wordcount
    
  })
  
  output$tweetPlot <- reactivePlot(function(){
    
    #wordcount <- data()
    
    
    if(input$vtype=='wordcloud'){
      wordcount <- data()
      pal <- brewer.pal(8,"Dark2")
      if(input$wctype=="decreasing"){
        if(input$sentiment == 'no')wordcloud(names(wordcount),freq=wordcount,scale=c(4,0.3),min.freq=input$min_freq,random.order=F,rot.per=.1,colors=pal)
        if(input$sentiment=='yes')wordcloud(names(wordcount),freq=wordcount,scale=c(4,0.3),min.freq=1,random.order=F,rot.per=.1,colors=pal)
      }
      else{
        if(input$sentiment == 'no')wordcloud(names(wordcount),freq=wordcount,scale=c(4,0.3),min.freq=input$min_freq,random.order=T,rot.per=.1,colors=pal)
        if(input$sentiment=='yes')wordcloud(names(wordcount),freq=wordcount,scale=c(4,0.3),min.freq=1,random.order=T,rot.per=.1,colors=pal)
      }
    }
    else if(input$vtype=='hclust'){
      myCorpus <- Corpus(VectorSource(searchNoun))  
      myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(2, Inf)))
      myTdm2 <- removeSparseTerms(myTdm, sparse = 0.99)
      m2 <- as.matrix(myTdm2)
      
      distMatrix <- dist(scale(m2))
      
      fit <- hclust(distMatrix, method = "ward")
      
      print(plot(fit))
      rect.hclust(fit, k = length(fit$height)/2)
      #rect.hclust(fit, k = 10)
    }
    
    
    
  })
  
  output$tweetbarPlot <- reactivePlot(function(){
    
    
    
    wordcount <- data()
    
    wordcount_df <- as.data.frame(wordcount)
    wordcount_df <- wordcount_df[order(wordcount_df$Freq,decreasing=T),]
    if(input$sentiment == 'no'){
      if(input$min_freq_2nd==0)wordcount_df <- subset(wordcount_df,Freq>=input$min_freq)
      else{
        min <- boxplot.stats(wordcount_df$Freq[which(wordcount_df$Freq>=input$min_freq)])$stats[input$min_freq_2nd]
        wordcount_df <- subset(wordcount_df,Freq>=min)
      }
    }
    # if(input$sentiment == 'yes') 일경우 모두 (min.freq>=1) 보여줌 
    base <- ggplot(wordcount_df,aes(x=searchNoun,y=Freq))
    print(base + geom_bar() + scale_x_discrete(limits=wordcount_df$searchNoun)+theme(axis.text.x = element_text(angle=45,size=10,face="bold",color="orangered")))
  })
  
})
