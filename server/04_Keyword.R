# select community
SelectComm <- reactive( extract_community_graph(NETKW, input$commid) )

# create semantic field
SelectSemField <- reactive( SemanticField(NETKW, kw = input$kwid2) )

# outputs ----

# panel "Data summary"

output$textfull <- renderText({
  describe_network( NETKW )
})

output$contentsnodes <- renderDataTable(
  info_table_nodes( NETKW ),
  options = list( pageLength = 10 )
)

output$contentsedges <- renderDataTable(
  info_table_edges( NETKW ),
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
  }
)
