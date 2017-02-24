library(shiny)

shinyUI(navbarPage(
  "CybergeoNetworks",
  theme = "darkBlue.css",

  source( "ui/00_Project.R")$value,
  source( "ui/01_Overview.R")$value,
  source( "ui/02_Citation.R")$value,
  source( "ui/03_Semantic.R")$value,
  source( "ui/04_Keyword.R")$value,
  source( "ui/05_GeoSemantic.R")$value

))
