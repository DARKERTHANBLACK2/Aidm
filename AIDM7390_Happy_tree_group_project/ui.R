#############library#############
library("shiny")
library("rtweet")
#################################

############token info###########
token <- create_token(
  consumer_key='nAmZd1Rk9JXdpuBfhwAl0S7aP',
  consumer_secret='ryHhWC8DMY0HUqfgsrw61yQrjQQ2dMsosHJI5I4yQMRMGqQZ7d',
  access_token='1330517411330592768-W3LZYgzz1agP700rgcDc0eiiZrVGAq',
  access_secret='bqpV6J3vpAhAFWHMicQUKnZyOo2QLbyepW3ZKeFEASKBt',
  set_renv=TRUE)
#############FUNCTION############
get_dates<-function(keyword){
  keyword_info <- search_tweets(paste("#",keyword,sep=""), n = 1000, include_rts = FALSE, lang = "en")
  save(keyword_info,file=gsub(" ","",paste("./data/",keyword,".Rdata",sep="")))
}
words<-c("JustinBieber","TaylorSwift","BTS","LadyGaga","DuaLipa","HarryStyles")
############Loop grab data########
#We use the get_dates function to capture the data and store it locally by looping words#
for(n in words){
  get_dates(n)
}
#################################
#################################
ui <- fluidPage(
  titlePanel("AIDM 7390¡ª¡ªHappy Tree"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "Rows",
                  label = "Rows of datasets:",
                  min = 1,
                  max = 5000,
                  value = 1000),
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices =c("Best_Pop_Duo","Best_Pop_Solo","Best_Pop_Vocal","Grammy","BTS","DuaLipa","rock","HarryStyles","JustinBieber","LadyGaga","TaylorSwift"))
                  ),
      mainPanel(
       tabsetPanel(type = "tabs",
                    tabPanel("line", plotOutput("line")),
                    tabPanel("bar", plotOutput("bar")),
                    tabPanel("word", plotOutput("word"))
      )
    )
  )
)
#################################
#################################
server <- function(input, output) {
 
  library("ggplot2")
  library("tm")
  library("wordcloud")
  library("wordcloud2")
  #############FUNCTION############
  create_chart<-function(data,Title,type,rows){
    l<-load(gsub(" ","",paste("./data/",data,".Rdata",sep="")))
    datas <- eval(parse(text = l))
    datas<-head(datas,rows)
    if(type==1){
      ts_plot(datas, "3 hours") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          x = NULL, y = NULL,
          title = Title
        )
    }else{
      mt.v <- VectorSource(datas$text)
      mt.c <- SimpleCorpus(mt.v)
      mt.c.p <- tm_map(mt.c, content_transformer(tolower)) 
      mt.c.p <- tm_map(mt.c.p, removeNumbers) 
      mt.c.p <- tm_map(mt.c.p, removeWords, stopwords("english"))
      mt.c.p <- tm_map(mt.c.p, removeWords, c("is","it","a","are","am","applause","can","cant","will","that","weve","dont","wont","youll","youre"))
      mt.c.p <- tm_map(mt.c.p, removePunctuation) 
      mt.c.p <- tm_map(mt.c.p, stripWhitespace) 
      dtm <- TermDocumentMatrix(mt.c.p)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing = TRUE)
      d <- data.frame(word = names(v),freq=v)
      #sele_word<-c("justinbieber","gaga","dualipa","taylorswift","harry")
      #sele_res<-subset(d,word %in% sele_word ,select=c(word,freq))
      #ggplot(data=sele_res,mapping=aes(x=word,y=freq,fill=word,group=factor(1)))+
      #geom_bar(stat="identity")+ggtitle("select singer  in GRAMMYs(20000 datas)")
      if(type==2){
        fre<-head(d,10)
        ggplot(data=fre,mapping=aes(x=word,y=freq,fill=word,group=factor(1)))+
          geom_bar(stat="identity")+ggtitle(Title)
      }else if(type==3){
        wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                  max.words=200, random.order=FALSE, rot.per=0.0,
                  colors=brewer.pal(8, "Dark2"))
      }
    }
  }

  #################################
  datasetInput <- reactive({
    switch(input$dataset,
           "Best_Pop_Duo"="Best_Pop_Duo",
           "Best_Pop_Solo"="Best_Pop_Solo",
           "Best_Pop_Vocal"="Best_Pop_Vocal",
           "BTS"="BTS",
           "DuaLipa"="DuaLipa",
           "rock" = "rock",
           "Grammy"="Grammy",
           "HarryStyles"="HarryStyles",
           "JustinBieber"="JustinBieber",
           "LadyGaga"="LadyGaga",
           "TaylorSwift"="TaylorSwift"
    )
  })
  output$line <- renderPlot({
    dataset <- datasetInput()
    create_chart(dataset,dataset,1,input$Rows)
  })
  output$bar <- renderPlot({
    dataset <- datasetInput()
    create_chart(dataset,dataset,2,input$Rows)
  })
  output$word <- renderPlot({
    dataset <- datasetInput()
    create_chart(dataset,dataset,3,input$Rows)
  })
}

#################

shinyApp(ui = ui, server = server)

