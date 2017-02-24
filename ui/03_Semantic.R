tabPanel("Full-text Semantic network",
  fluidPage(
   column( 3,
     # Select an option in order to compute the interface (one or more patterns) and the outputs
     selectInput("mode", "Mode", c(
       "Single Pattern Analysis" = "one",
       "Multiple Pattern Analysis" = "multi",
       "Parameterisation" = "param"
     )),
     conditionalPanel(
       "input.mode == 'multi' | input.mode == 'one'",
       textInput("pattern_input", "Pattern")
     ),
     conditionalPanel(
       "input.mode == 'multi'",
       actionButton("add_pattern", "Add to Selection"),
       p(),
       checkboxGroupInput("patterns_selection", "Pattern Selection", pattern_list)
     )
   ),
   column( 9,
     # All the outputs panel
     tabsetPanel(
       # show a chronogram of the number of match per year
       tabPanel("Chronogram", plotOutput("chronogram", height = "700px") ),

       # show a wordcloud of the matched items
       tabPanel( "Word Cloud",plotOutput("cloud", height = "700px") ),

       # show the sentences including a term that matches a pattern
       tabPanel("Sentences", verbatimTextOutput("phrases")),

       # show the articles including a term that matches a pattern
       tabPanel("Citations", verbatimTextOutput("citations"))
     ))
  )
) 
