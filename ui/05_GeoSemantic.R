navbarMenu("Geo-semantic Networks",
  tabPanel("Geo-semantic Networks",
    fluidRow(
      column(4, selectInput("semanticMethod", label = "Semantic Method", choices = c("Citations", "Keywords", "Semantic"), multiple = F)),
      column(4, selectInput("aggregationMethod", label = "Set of Countries",choices = c("Authoring", "Studied"), selected = "Studied", multiple = F)),
      column(4, sliderInput("nClassifGroups", label = "Number of Clusters", min = 1, max = 8, value = 4, step = 1), animate=T)
    ),
    plotOutput("termsXCountriesMap"),
    plotOutput("termsXCountriesLegend")
  ),
  tabPanel("User guide", includeMarkdown("doc/GeoSemanticNetworks.md") )
)
