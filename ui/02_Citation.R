navbarMenu("Citation network",
  tabPanel("Citation Network",

    # select article to visualize
    fluidRow(
      h4("Data Selection"),
      tags$p(class="text-justify","Search and select a cybergeo paper in the table."),
      htmlOutput("citationdataloading"),
      dataTableOutput("citationcybergeo")
    ),

    # citation ego network
    fluidRow(
      h4("Citation network neighborhood"),
      tags$p(class="text-justify","This graph shows the citation neighborhood of the selected paper"),
      selectInput(inputId = "citationselected",
                  label = "Select a publication by id",
                  choices = c("",sort(citation_cybergeodata$id[citation_cybergeodata$linknum>0],decreasing = TRUE)),
                  selected = "",
                  multiple = FALSE),
      plotOutput("citationegoplot", width = "100%", height = "800px")
    ),

    # word clouds of semantic content
    fluidRow(
      h4("Semantic content"),
      tags$p(class="text-justify","This graph shows the semantic content (color legend in user guide) of the paper (left) and its neighborhood (right)."),
      selectInput(inputId = "citationsemanticselected",
                  label = "Select a publication by id",
                  choices = c("",sort(citation_cybergeodata$id[citation_cybergeodata$kwcount>0],decreasing = TRUE)),
                  selected = "",
                  multiple = FALSE),
      plotOutput("citationesemanticplot", width = "100%", height = "800px")
    )
  ),

  # svg viusalization of the full semantic network
  tabPanel("Semantic Network",
    h4("Full Semantic Network"),
    column(12,svgPanZoomOutput(outputId = "citationsemanticnw",width = "100%", height = "100%"))
  ),
  # user guide
  tabPanel("User guide",
    # describe data provenance and signification of measures
    includeMarkdown("doc/CitationNetwork.md")
  )
)
