
# subset of the data from ARTICLES in the date interval
data_overview <- reactive({
  subset_articles( ARTICLES, input$dateRange )
})

# summary table
output$statArticles <- renderDataTable({
    overview_stats( data_overview() )
})

# map plot
output$cybMap = renderPlot({
  data <- data_overview()
  plot_overview_map( world, data, input$dateRange, input$whatMapped )
})
