navbarMenu("Keyword network",

  tabPanel("Data summary",
    fluidRow(
      htmlOutput("textfull"),
      tags$hr(),
      dataTableOutput("contentsnodes"),
      dataTableOutput("contentsedges")
    )
  ),

  tabPanel("Communities",
    selectInput(inputId = "commid", label = "Choose a community", choices = choices_communities, selected = "", multiple = FALSE),
    fluidRow(

      column(3, wellPanel(
        tags$strong("Graphic options"),
        radioButtons("vsizecom", "Nodes proportional to:", list("Uniforme" = "uni", "Number of articles" = "poi","Nodes degree" = "deg")),
        radioButtons("esizecom", "Edges proportional to:", list("Observed weight" = "nbl","Residuals" = "rel")),
        sliderInput(inputId = "vfacsizecom",
                    label = "Nodes size",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.05),
        sliderInput(inputId = "efacsizecom",
                    label = "Edges size",
                    min = 0,
                    max = 2,
                    value = 1,
                    step = 0.1),
        sliderInput(inputId = "tsizecom",
                    label = "Font size",
                    min = 1,
                    max = 15,
                    value = 10,
                    step = 1),
        downloadButton("downcomm", "Download plot")
      )),
      column(9,
        plotOutput("plotcomm", width = "100%", height = "800px")
      )
    )
  ),
  tabPanel("Semantic area",
    selectInput(inputId = "kwid2", label = "Choose a keyword", choices = choices_keywords, selected = "", multiple = FALSE ),
    fluidRow(
      column(3, wellPanel(
        tags$strong("Graphic options"),
        sliderInput(inputId = "tsizesemmin",
                    label = "Font size (min)",
                    min = 1,
                    max = 10,
                    value = 4,
                    step = 0.5),
        sliderInput(inputId = "tsizesemmax",
                    label = "Font size (max)",
                    min = 1,
                    max = 10,
                    value = 6,
                    step = 0.5),
        downloadButton("downsem", "Download plot")
      )),
      column(9, plotOutput("plotsem", width = "100%", height = "800px"))
    )
  ),
  tabPanel("User guide", withMathJax(),includeMarkdown("doc/README_HC.md"))
)
