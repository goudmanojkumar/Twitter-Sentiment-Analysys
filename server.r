library(shiny)
shinyServer(function(input, output) {
  library('twitteR')
  library('ROAuth')
  consumerKey="9U7w63UkBA5p2PlEyUeG7Vjoa"
  consumerSecret="zzMOAle6SArQDyl6CPSzdtLIadyhgYXbyeGwWyMU2juowjy0iJ"
  reqURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "https://api.twitter.com/oauth/access_token"
  authURL <- "https://api.twitter.com/oauth/authorize"
  Cred <- OAuthFactory$new(consumerKey=consumerKey,
                           consumerSecret=consumerSecret,
                           requestURL=reqURL,
                           accessURL=accessURL,
                           authURL=authURL)
  setup_twitter_oauth(consumerKey,consumerSecret,access_token="971799081952530433-8GDzhxarT5k7kgSX0WNyw1K0OgIFVP9",access_secret="vUmHhNG3H4QYfOmIwyY4kMZDVzlKPxFcdSWEBdx606zwD")
  output$plot<-renderPlot({ 
    if(input$pType=='a')
    {
      searchterm<-input$term
      num<-input$i
      
      #access tweets and create cumulative file
      list <- searchTwitter(searchterm,n= num, lang="en", since=NULL, until=NULL, retryOnRateLimit=10) 
      
      df <- twListToDF(list)
      df <- df[, order(names(df))]
      df$created <- strftime(df$created, '%Y-%m-%d')
      if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
      #merge last access with cumulative file and remove duplicates
      stack <- read.csv(file=paste(searchterm, '_stack.csv'))
      stack <- rbind(stack, df)
      stack <- subset(stack, !duplicated(stack$text))
      write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
      
      
      #evaluation tweets function
      score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
      {
        library("plyr")
        library("stringr")
        scores <- laply(sentences, function(sentence, pos.words, neg.words){
          
          
          
          
          sentence <- iconv(sentence, "latin1", "ASCII//TRANSLIT")
          
          
          sentence <- iconv(sentence, to='ASCII//TRANSLIT')
          
          
          
          
          sentence <- gsub('[[:punct:]]', "", sentence)
          sentence <- gsub('[[:cntrl:]]', "", sentence)
          sentence <- gsub('\\d+', "", sentence)
          sentence <- tolower(sentence)
          
          
          
          word.list <- str_split(sentence, '\\s+')
          words <- unlist(word.list)
          pos.matches <- match(words, pos.words)
          neg.matches <- match(words, neg.words)
          pos.matches <- !is.na(pos.matches)
          neg.matches <- !is.na(neg.matches)
          score <- sum(pos.matches) - sum(neg.matches)
          return(score)
        }, pos.words, neg.words, .progress=.progress)
        scores.df <- data.frame(score=scores, text=sentences)
        return(scores.df)
        detach("package:plyr", unload = TRUE)
        
      }
      library("ggplot2")
      pos <- scan('http://ptrckprry.com/course/ssd/data/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
      neg <- scan('http://ptrckprry.com/course/ssd/data/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
      
      pos.words <- c(pos, 'upgrade')
      neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
      Dataset <- stack
      Dataset$text <- as.factor(Dataset$text)
      scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
      write.csv(scores, file=paste(searchterm, '_scores.csv'), row.names=TRUE) #save evaluation results into the file
      #total evaluation: positive / negative / neutral
      stat <- scores
      stat$created <- stack$created
      stat$created <- as.Date(stat$created)
      stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
      require("dplyr")
      by.tweet <- group_by(stat, tweet, created)
      by.tweet <- summarise(by.tweet, number=n())
      detach("package:dplyr", unload = TRUE)
      write.csv(by.tweet, file=paste(searchterm, '_opin.csv'), row.names=TRUE)
      #create chart
      
      qplot(scores$score, geom="histogram",xlim = c(-5,5),xlab = "Tweet Sentiment Score (-5 to +5)",ylab="Count",main = searchterm) 
      
      
    }
  }
  )
  
}) 
#})

