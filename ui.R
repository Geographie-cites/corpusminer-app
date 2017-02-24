library(shiny)

shinyUI(navbarPage(
  "CybergeoNetworks",
  theme = "darkBlue.css",

  sub_app( "00_Project" ),
  sub_app( "01_Overview" ),
  sub_app( "02_Citation" ),
  sub_app( "03_Semantic" ),
  sub_app( "04_Keyword" ),
  sub_app( "05_GeoSemantic" )

))
