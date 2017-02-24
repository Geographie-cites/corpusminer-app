tabPanel("Overview",
  fluidRow(
    column(6,
      sliderInput("dateRange", label = "Time Range",
        min = 1996, max = 2015, value = c(1996,2015),
        step = 1, animate=T)
    ),
    column(6,
      selectInput("whatMapped", label = "Indicator to map",
        choices=c("Authoring countries" = "A", "Countries Studied"= "S", "Countries Studies by Locals"= "L"),
        multiple=F)
    )
  ),
  plotOutput("cybMap"),
  dataTableOutput('statArticles')
)
