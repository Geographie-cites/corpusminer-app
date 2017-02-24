clusterCountries <- reactive({
  geo_semantic_data[[ c(input$semanticMethod, input$aggregationMethod ) ]]
})

cahCountries <- reactive({
  clusters <- clusterCountries()
  data_frame(
    ID = clusters$data[,1],
    group = cutree( clusters$hc, k = input$nClassifGroups )
  )
})

output$termsXCountriesMap = renderPlot({
  groupsOfCountries <- input$nClassifGroups

  country_id <- world@data$CNTR_ID

  cahRes <- cahCountries()
  groups <- cahRes$group[ match(country_id, cahRes$ID ) ]
  col <- paletteCybergeo[ groups ]

  par(mfrow=c(1,1), mar = c(0,0,1,0), bg="#2b3e50")
  plot(world, col=col, border="white", lwd=0.7)
  title("Groups of countries based on semantic networks", col.main = "white")
})

output$termsXCountriesLegend = renderPlot({
  clusters <- clusterCountries()

  groups <- cahCountries()$group

  data <- mutate( clusters$data, group = groups ) %>%
    group_by( group )

  nArticlesByGroup <- data %>%
    summarise( n = sum(n, na.rm = TRUE) )

  leg <- data %>%
    summarise_at(vars(one_of(clusters$themes)), mean) %>%
    select(-group) %>%
    as.matrix

  groupsOfCountries <- input$nClassifGroups
  mfrow <- c( ceiling(groupsOfCountries/2), 2 )

  par(mfrow=mfrow, las=2, mar = c(4,10,2,1), bg="#2b3e50")
  for(i in 1:groupsOfCountries){
    barplot(leg[i,], col=paletteCybergeo[i], horiz=TRUE, cex.names=0.8, xlab= "Frequency of themes", col.lab="white", col.axis="white")
    axis(1, col = "white", col.axis = "white")
    if(nArticlesByGroup[i, "n"] == 1)  title(paste0(nArticlesByGroup[i, "n"], " article"), col.main = "white")
    if(nArticlesByGroup[i, "n"] > 1)  title(paste0(nArticlesByGroup[i, "n"], " articles"), col.main = "white")
  }

})
