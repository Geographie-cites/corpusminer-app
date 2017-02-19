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

#-- Functions -----------------------------------------------------------------

#' @title Matched terms list
#' @name terms_matched
#' @description Compute a dataframe of matched terms
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a data frame of matched terms
terms_matched <- function(patterns) {
  data <- data_frame()
  for (pattern in patterns) {
    indices <- grep(pattern, terms$term, ignore.case = TRUE, perl = TRUE)
    data <- data_frame(id = indices) %>%
      dplyr::mutate(pattern = pattern) %>%
      dplyr::bind_rows(data)
  }
  data <- data %>%
    dplyr::left_join(terms, by = c("id")) %>%
    dplyr::arrange(id, pattern)
  return(data)
}

#' @title Matched articles list
#' @name titles_matched
#' @description Compute a list of articles containing a matched term
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a string vector of articles containing a matched term
titles_matched <- function(patterns) {
  citations <- terms_matched(patterns) %>%
    dplyr::select(article_id) %>%
    dplyr::unique() %>%
    dplyr::left_join(articles, by = c("article_id" = "id")) %>%
    dplyr::arrange(date) %>%
    dplyr::select(citation)
  return(citations$citation)
}

#' @title Matched sentences list
#' @name phrases
#' @description Compute a list of sentences containing a matched term
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a vector of sentences containing a matched term
phrases <- function(patterns) {
  data <- data_frame()
  for (pattern in patterns) {
    indices <- grep(pattern, sentences$sentence, ignore.case = TRUE, perl = TRUE)
    data <- data_frame(id = indices) %>%
      dplyr::bind_rows(data)
  }
  data <- data %>%
    dplyr::left_join(sentences, by = c("id")) %>%
    dplyr::select(sentence)
  return(data$sentence)
}

#' @title Metadata for each matched terms
#' @name terms_matched_cloud
#' @description Compute the metadata of each terms in order to build a wordcloud
#' @param patterns: a string vector of regexp patterns to match
#' Returns:
terms_matched_cloud <- function(patterns) {
  terms_matched(patterns) %>%
    dplyr::group_by(term) %>%
    #    summarise(articles = n_distinct(article_id), terms = sum(count))
    dplyr::summarise(articles = sum(count))
}

#' @title Metadata of each articles containing a matched term
#' @name articles_matched
#' @description Compute the metadata of each articles containing a matched term
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a dataframe of articles metadata
articles_matched <- function(patterns) {
  terms_matched(patterns) %>%
    dplyr::group_by(article_id, pattern) %>%
    dplyr::summarise(count = sum(count)) %>%
    dplyr::left_join(articles, by = c("article_id" = "id")) %>%
    dplyr::mutate(ym = str_sub(date, 1, 4)) %>%
    dplyr::group_by(ym, pattern) %>%
    dplyr::summarise(articles=n_distinct(article_id), terms=sum(count)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = parse_date_time(ym, "%y")) %>%
    dplyr::select(date, pattern, articles, terms)
}

#' @title Chronogramme
#' @name chronogram
#' @description Compute a chronogram graphic of articles
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a graphic
chronogram <- function(patterns) {
  ggplot(articles_matched(patterns), aes(date, articles)) +
    geom_bar(stat = "identity") +
    facet_grid(pattern ~ ., scales = "free_y", space = "free_y") +
    labs(title="Chronogramme des articles publiés dans Cybergéo", x = "Année de publication", y = "Nombre d'articles publiés")
}

#' @title Cloud of terms
#' @name cloud
#' @description Compute a word cloud of matched terms
#' @param patterns: a string vector of regexp patterns to match
#' Returns: a graphic
cloud <- function(patterns) {
  words <- terms_matched_cloud(patterns)
  wordcloud(
    words$term,
    words$articles,
    scale = c(10,1),
    rot.per = 0
  )
}
















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
