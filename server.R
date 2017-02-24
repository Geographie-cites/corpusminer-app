# set server ----

shinyServer(function(input, output, session) {

  source( "server/01_Overview.R", local = TRUE)$value
  source( "server/02_Citation.R", local = TRUE)$value
  source( "server/03_Semantic.R", local = TRUE)$value
  source( "server/04_Keyword.R", local = TRUE)$value
  source( "server/05_GeoSemantic.R", local = TRUE)$value

})
