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
