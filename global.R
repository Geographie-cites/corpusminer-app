##############################
# Shiny App: Cybergeo20
# Packages and functions
##############################



# load packages ----

library(shiny)
library(rgdal)
library(plyr)
library(mapproj)
library(maptools)
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(reshape2)
library(grid)
library(igraph)
library(dplyr)
library(RSQLite)
library(svgPanZoom)
library(wordcloud)
library(scales)
library(lubridate)
library(stringr)


# load data ----



#### Keywords network

#'
#' @description contains cyberData = list(NETKW = igraph network of thesaurus keywords, ARTICLES keywords raw data)
load("data/CyberData.RData")
cyberData$ARTICLES$year <- as.numeric( substr( as.character(cyberData$ARTICLES$date.1), 1, 4 ) )

choices_communities <- sort( unique( V(cyberData$NETKW)$clus ))
choices_keywords <- sort( unique( V(cyberData$NETKW)$name ))

#'
#' @description thesaurus themes probas
hadriTerms = read.csv("data/kwprop.csv", sep=",", dec=".")


#### Semantic network

#'
#' @description themes probas with the semantic network classification
justeTerms = read.csv("data/docprobasJuste2.csv", sep=",", dec=".")


#### LDA analysis

#'
#' @description LDA analysis output : files = ids and raw files ;
#'   themes.termes = keywords of thematics ; document.themes = probability matrix
load("data/themesPO.Rdata")
files$name = NULL
files$path = NULL

#'
#' @description keywords and themes from LDA
poTerms = read.csv("data/20themes20words.csv", sep=",", dec=".")

nameThemes = c(as.character(poTerms$NAME), "Other")
colnames(document.themes) = nameThemes
files[,3:22] = document.themes
colnames(files)[3:22] = nameThemes


#### Geographical data

world = readOGR(dsn="data/world_withZoom.shp",
                layer = "world_withZoom", encoding="utf8", verbose = F)
countries = as.character(world@data$CNTR_ID)
locals = paste0("L_", countries)
authors = paste0("A_", countries)
studies = paste0("S_", countries)
lookup = data.frame(countries)
lookup$polyID = as.numeric(rownames(lookup)) - 1

######## PO :
##   Regexp terms in full textes

#-- Loading data --------------------------------------------------------------

# Read the terms dataframe
terms <- read.table(
  "data/terms.csv",
  sep = ";",
  quote = "",
  comment.char = "",
  header = TRUE,
  stringsAsFactors = FALSE
) %>%
  tbl_df() %>%
  dplyr::mutate(
    article_id = id,
    id = row_number()
  ) %>%
  dplyr::select(id, article_id, term, count)

# Read the sentences dataframe
sentences <- read.table(
  "data/sentences.csv",
  sep = "|",
  quote = "",
  comment.char = "",
  header = TRUE,
  stringsAsFactors=FALSE
) %>%
  tbl_df() %>%
  dplyr::mutate(
    article_id = id,
    id = row_number()
  )

# Read the metadata of articles
articles <- read.table(
  "data/cybergeo.csv",
  sep = ",",
  quote = "\"",
  comment.char = "",
  header = TRUE
) %>%
  tbl_df() %>%
  dplyr::rename(titre = title_en, auteurs = authors) %>%
  dplyr::mutate(citation = paste(sep = ". ", auteurs, substr(date,1,4), titre)) %>%
  dplyr::select(id, date, citation, langue)



####################
### Juste ---
#
#  --  Archi for cit. nw exploration  --
#
#   - data/semanticnw.RData is not loaded as huge ; replaced by sqlite
#    -> for performance, can be fully loaded is speed is prefered over memory
#   - load datatable for cybergeo articles ; request in local sqlite db for connections
#   - get the ego nw, and display info for neighbors
#   - display semantic info : keywords, corresponding communities.
#   - one tab with sem nw visu : svg viz
#
#


##
#  Notations / id conventions : vars and ids prefixed with "citation"

#' ---- DATA ----

#'
#'  citation nw cybergeo table
load('data/citation_cybergeodata.RData')

#'
#'  kws domains dico
load('data/citation_kwthemdico.RData')

#'
#'  sqlite connection : citation nw
citationdbcit = dbConnect(SQLite(),"data/CitationNetwork.sqlite3")

#'
#'  sqlite connection : keywords
citationdbkws = dbConnect(SQLite(),"data/CitationKeywords.sqlite3")


#' ---- GLOBALS ----

#'
#' list specific colors associated with thematics
#'  -- SHOULD NOT BE HARDCODED --


# global vars (needed e.g. to avoid numerous db request with reactive functions)
citationGlobalVars <- reactiveValues()
citationGlobalVars$citationSelected = "0"
citationGlobalVars$citationSemanticSelected = "0"
