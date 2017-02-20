##############################
# Shiny App: Cybergeo20
# Server
##############################



# set server ----

shinyServer(function(input, output, session) {

  ### Overview
  #
  # data used:
  # - cyberData$ARTICLES
  # - world

  # subset of the data from cyberData$ARTICLES in the date interval
  data_overview <- reactive({
    subset_articles( cyberData$ARTICLES, input$dateRange )
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


  ### CLEM ----

  themeNames <- reactiveValues(listThemes = NULL)

  observe({
    if (input$semanticMethod == "Citations")  themeNames$listThemes = colnames(justeTerms)[2:13]
    if (input$semanticMethod == "Keywords")  themeNames$listThemes = colnames(hadriTerms)[2:11]
    if (input$semanticMethod == "Semantic")  themeNames$listThemes = nameThemes
  })

  clusterCountries <- reactive({
    termsMethod = input$semanticMethod
    themeNames = themeNames$listThemes
    groupsOfCountries = input$nClassifGroups
    termCountryRelation = input$aggregationMethod
    articles = cyberData$ARTICLES

    if(termCountryRelation == "Authoring") tcr = authors
    if(termCountryRelation ==  "Studied") tcr = studies
    if (termsMethod == "Citations"){
      cybterms = justeTerms[justeTerms$CYBERGEOID != 0,]
      cybterms$idterm = rownames(cybterms)
      cybterms2 = data.frame(cybterms, articles[match(cybterms$CYBERGEOID,articles$id), ])
    }
    if (termsMethod == "Keywords"){
      cybterms = hadriTerms
      cybterms2 = data.frame(cybterms, articles[match(cybterms$ID,articles$id), ])
    }
    if (termsMethod == "Semantic"){
      articlesWithThemes = data.frame(articles, files[match(articles$id,files$id), ])
      cybterms = articlesWithThemes[,c("id",themeNames)]
      cybtermsbis = cybterms[complete.cases(cybterms[,themeNames]),]
      cybterms2 = data.frame(cybtermsbis, articles[match(cybtermsbis$id,articles$id), ])
    }

    cybterms3 = data.frame(cybterms2, lookup[match(cybterms2$firstauthor,lookup$countries), ])
    cybterms4 = cybterms3[complete.cases(cybterms3$id.1),]

    themes_By_country_bf = aggregateCountriesBasedOnTerms(themesFile = cybterms4, themes = themeNames, countries_to_aggregate = tcr)

    return(themes_By_country_bf)
  })

  cahCountries <- reactive({
    groupsOfCountries = input$nClassifGroups
    themeNames = themeNames$listThemes
    themes_By_country_bf = clusterCountries()
    cahRes = cahCountriesBasedOnTerms(themes_By_country_bf = themes_By_country_bf, numberOfGroups = groupsOfCountries, themes = themeNames)
    cahResDF = data.frame("ID" = themes_By_country_bf[,1], "group" = cahRes)
    return(cahResDF)
  })

  legCahCountries <- reactive({
    groupsOfCountries = input$nClassifGroups
    themeNames = themeNames$listThemes
    themes_By_country_bf = clusterCountries()
    countriesDF = themes_By_country_bf[,themeNames]
    rownames(countriesDF) = themes_By_country_bf[,1]
    legcahRes = cahCountriesBasedOnTerms(themes_By_country_bf = themes_By_country_bf, numberOfGroups = groupsOfCountries, themes = themeNames)
    leg = sapply(countriesDF, stat.comp,y=legcahRes)
    return(leg)
  })

  output$termsXCountriesMap = renderPlot({
    groupsOfCountries = input$nClassifGroups
    themeNames = themeNames$listThemes

    cahRes = cahCountries()
    cahRes$groupColour = as.character(cut(cahRes$group, breaks = c(1:groupsOfCountries, groupsOfCountries+1),
                      labels = paletteCybergeo[1:groupsOfCountries],include.lowest = TRUE,right = FALSE))
    REG=world
    REG@data = data.frame(REG@data, cahRes[match(REG@data$CNTR_ID,cahRes$ID), ])
    par(mfrow=c(1,1), mar = c(0,0,1,0), bg="#2b3e50")
    plot(REG, col=REG@data$groupColour, border="white", lwd=0.7)
    title("Groups of countries based on semantic networks", col.main = "white")
  })

  output$termsXCountriesLegend = renderPlot({
    groupsOfCountries = input$nClassifGroups
    themeNames = themeNames$listThemes
    leg = legCahCountries()
    if(groupsOfCountries %% 2 == 0) window = c(groupsOfCountries/2,2)
    if(groupsOfCountries %% 2 == 1) window = c(groupsOfCountries/2 + 0.5,2)
    termsMethod = input$semanticMethod
    themes_By_country_bf = clusterCountries()
    themes_By_country_bf$group =  cahCountriesBasedOnTerms(themes_By_country_bf = themes_By_country_bf, numberOfGroups = groupsOfCountries, themes = themeNames)
    nArticlesByGroup = aggregate(themes_By_country_bf[,"n"], by = list(themes_By_country_bf$group), sum, na.rm = TRUE)
    colnames(nArticlesByGroup) = c("ID", "n")
    nArticlesByGroup = nArticlesByGroup[order(nArticlesByGroup$ID),]

    par(mfrow=window, las=2, mar = c(4,10,2,1), bg="#2b3e50")
    for(i in 1:groupsOfCountries){
      barplot(leg[i,], col=paletteCybergeo[i], horiz=TRUE, cex.names=0.8, xlab= "Frequency of themes", col.lab="white", col.axis="white")
      axis(1, col = "white", col.axis = "white")
      if(nArticlesByGroup[i, "n"] == 1)  title(paste0(nArticlesByGroup[i, "n"], " article"), col.main = "white")
      if(nArticlesByGroup[i, "n"] > 1)  title(paste0(nArticlesByGroup[i, "n"], " articles"), col.main = "white")
    }
  })

  ### Juste ----


   #########

  # output$semanticNetwork <- renderForceNetwork({
  #   forceNetwork(Links = edf, Nodes = vdf,
  #                Source = "source", Target = "target",
  #                Value = "value", NodeID = "name",
  #                Group = "community", opacity = 0.8,zoom=TRUE)
  #
  #  })
  #
  #  DO NOT use networkD3js, unless initial layout is possible

   #########

   # data loading
   # output$citationdataloading<-renderText({
   #   if(is.null(citationGlobalVars$loaded)){"Data Loading..."}else{""}
   # })

   ## selection datatable
   output$citationcybergeo <- renderDataTable({
     withProgress(expr={},message='Data Loading...')
     citation_cybergeodata[citation_cybergeodata$linknum>0|citation_cybergeodata$kwcount>0,c(1,2,3,7)]
     #DT::datatable(citation_cybergeodata[,c(1,2,3,7)],selection='single')
   })

   # strange behavior of rows_selected : due to use of datatable ?
   #  -> must do a setdiff on global var - ultra dirty
   #  seems to be a wrong implementation of rows_selected with single selection -- beurk. [horrible function intrication]
   # OK fuck this shit, allow multiple selection and draw plot only if a single selected
   # as some situations are not detectable.
   citationSelectedCybergeoArticle <- reactive({
     return(input$citationcybergeo_rows_selected)
     # show(sel)
     # if(is.null(citationGlobalVars$prevsel)){
     #   if(length(sel)>0){citationGlobalVars$prevsel=sel;return(sel[1])}else{return(0)}
     # }else{
     #   if(length(sel)==length(citationGlobalVars$prevsel)){return(citationGlobalVars$citationSelected)}
     #   if(length(sel)<length(citationGlobalVars$prevsel)){
     #     # deselect a row : nothing selected
     #      citationGlobalVars$prevsel = sel
     #      return(0)
     #   }else{
     #      selindex = setdiff(sel,citationGlobalVars$prevsel)[1]
     #      citationGlobalVars$prevsel = sel#setdiff(sel,c(citationGlobalVars$citationSelected))
     #      return(selindex)
     #   }
     # }
   })


   # observer make data update requests
   observe({
     selected = citationSelectedCybergeoArticle()
     selected_hand = which(citation_cybergeodata$id==input$citationselected)
     if(length(selected_hand)>0){selected=selected_hand}
     #show(paste0("selected : ",selected))
     if(length(selected)==1){
       if(selected[1]!=citationGlobalVars$citationSelected){
         #show(paste0("selected different : ",citation_cybergeodata$title[as.numeric(selected)]))
         citationGlobalVars$citationSelected=selected[1]
         selectedschid = citation_cybergeodata$SCHID[as.numeric(selected[1])]
         # make request for edges in sqlitedb
         citationGlobalVars$edges = citationLoadEdges(selectedschid)
       }
     }
   })

   # similar observer for semantic plot
   observe({
     selected = citationSelectedCybergeoArticle()
     selected_hand = which(citation_cybergeodata$id==input$citationsemanticselected)
     if(length(selected_hand)>0){selected=selected_hand}
     if(length(selected)==1){
       if(selected[1]!=citationGlobalVars$citationSemanticSelected){
         citationGlobalVars$citationSemanticSelected=selected[1]
         selectedschid = citation_cybergeodata$SCHID[as.numeric(selected[1])]
         citationGlobalVars$keywords = citationLoadKeywords(selectedschid)
       }
     }
   })


   # render citation graph around selected article
   output$citationegoplot = renderPlot({
      citationVisuEgo(citationGlobalVars$edges)
   })

   # render wordclouds
   output$citationesemanticplot = renderPlot({
     citationWordclouds(citation_cybergeodata$SCHID[citationGlobalVars$citationSemanticSelected],citationGlobalVars$keywords)
   })


   # semantic nw viz
   output$citationsemanticnw<-renderSvgPanZoom({
     svgPanZoom('data/semantic.svg',
                zoomScaleSensitivity=1,
                minZoom=2,
                maxZoom=20,
                contain=TRUE
                )
   })


   ######## PO ---

   # Ask a pattern to match in the corpus
   patterns <- reactive({
     if(input$mode == 'one') { input$pattern_input } else { input$patterns_selection }
   })

   # Ask for a new pattern to add in the list
   observeEvent(
     input$add_pattern,
     {
       pattern_list <<- c(input$pattern_input, pattern_list)
       updateTextInput(session, "pattern_input", value = "")
       updateCheckboxGroupInput(session, "patterns_selection", choices = pattern_list, selected = c(input$pattern_input, input$patterns_selection))
     }
   )

   # Compute the Outputs
   output$chronogram <- renderPlot(chronogram(patterns()))
   output$cloud <- renderPlot(cloud(patterns()))
   output$citations <- renderPrint(titles_matched(patterns()))
   output$phrases <- renderPrint(phrases(patterns()))







  ### HADRI ----

  # select community
  SelectComm <- reactive( extract_community_graph(cyberData$NETKW, input$commid) )

  # create semantic field
  SelectSemField <- reactive(SemanticField(cyberData$NETKW, kw = input$kwid2) )

  # outputs ----

  # panel "Data summary"

  output$textfull <- renderText({
    describe_network( cyberData$NETKW )
  })

  output$contentsnodes <- renderDataTable(
    info_table_nodes( cyberData$NETKW ),
    options = list( pageLength = 10 )
  )

  output$contentsedges <- renderDataTable(
    info_table_edges( cyberData$NETKW ),
    options = list( pageLength = 10 )
  )


  # panel "Communities"
  output$plotcomm <- renderPlot({
    VisuComm( SelectComm(),
              vsize.prop = input$vsizecom, vsize.fac = input$vfacsizecom, vsize.default = 1,
              esize.prop = input$esizecom, esize.fac = input$efacsizecom,
              vertex.label.cex = input$tsizecom / 10
              )
  })

  output$downcomm <- downloadHandler(
    filename = "Community.svg",
    content = function(file) {
      svg(file, width = 20 / 2.54, height = 20 / 2.54, pointsize = 8)

      VisuComm( SelectComm(),
                vsize.prop = input$vsizecom, vsize.fac = input$vfacsizecom, vsize.default = 30,
                esize.prop = input$esizecom, esize.fac = input$efacsizecom,
                vertex.label.cex = input$tsizecom / 10
      )
      dev.off()
    })


  # panel "Semantic area"

  output$plotsem <- renderPlot({
    VisuSem(SelectSemField(), kw = input$kwid2, textsizemin = input$tsizesemmin, textsizemax = input$tsizesemmax)
  })

  output$downsem <- downloadHandler(
    filename = "Semantic.svg",
    content = function(file) {
      svg(file, width = 20 / 2.54, height = 20 / 2.54, pointsize = 8)
      VisuSem(SelectSemField(), kw = input$kwid2, textsizemin = input$tsizesemmin, textsizemax = input$tsizesemmax)
      dev.off()
    })


})
