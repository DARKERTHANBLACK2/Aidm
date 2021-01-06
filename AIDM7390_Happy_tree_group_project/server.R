server <- function(input, output) {
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
