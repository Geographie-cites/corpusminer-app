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
       citationGlobalVars$edges = citationLoadEdges(citationdbcit, selectedschid)
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
       citationGlobalVars$keywords = citationLoadKeywords(citationdbcit, citationdbkws, selectedschid)
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
