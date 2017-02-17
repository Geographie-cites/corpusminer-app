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

articles = data.frame()
paletteCybergeo = c("#1C6F91", "#df691a", "#77c5ba", "orange", "#2db92d", "#e1ff2f", "#ff2313", "#bbab61")
pattern_list <- c("espace", "territoire", "environnement", "société", "réseau", "interaction", "aménagement", "urbanisme", "carte", "modèle", "système", "SIG", "fractale", "durabilité", "représentation", "migration", "quantitatif", "qualitatif", "post-moderne")










######## PO :
##   Regexp terms in full textes



pattern_list <- c("espace", "territoire", "environnement", "société", "réseau", "interaction", "aménagement", "urbanisme", "carte", "modèle", "système", "SIG", "fractale", "durabilité", "représentation", "migration", "quantitatif", "qualitatif", "post-moderne")

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
semanticcolors = list(rgb(204,0,255,maxColorValue=255),rgb(255,102,0,maxColorValue=255), rgb(255,102,0,maxColorValue=255),
                      rgb(255,153,0,maxColorValue=255),rgb(0,204,102,maxColorValue=255),rgb(255,0,0,maxColorValue=255),
                      rgb(153,153,0,maxColorValue=255),rgb(102,204,0,maxColorValue=255),rgb(0,255,255,maxColorValue=255),
                      rgb(255,255,0,maxColorValue=255),rgb(51,102,255,maxColorValue=255),rgb(51,255,51,maxColorValue=255),
                      rgb(0,102,0,maxColorValue=255),rgb(0,0,255,maxColorValue=255),rgb(102,51,0,maxColorValue=255)
)
names(semanticcolors)<-c("complex systems","health","crime",
                         "statistical methods","remote sensing","political sciences/critical geography",
                         "traffic modeling","microbiology","cognitive sciences",
                         "spatial analysis","GIS","biogeography",
                         "environnment/climate","economic geography","physical geography")


# global vars (needed e.g. to avoid numerous db request with reactive functions)
citationGlobalVars <- reactiveValues()
citationGlobalVars$citationSelected = "0"
citationGlobalVars$citationSemanticSelected = "0"


#' ---- FUNCTIONS ----


#'
#' @name citationLoadEdges
#' @description load citation edges given an reference id
citationLoadEdges<-function(id){
  res=data.frame()
  res=rbind(res,dbGetQuery(citationdbcit,paste0("SELECT * FROM edges WHERE `from`='",id,"';")))
  res=rbind(res,dbGetQuery(citationdbcit,paste0("SELECT * FROM edges WHERE `to`='",id,"';")))
  return(res)
}

#' @name  citationLoadKeywords
#' @description load neighbors keywords given an id
citationLoadKeywords<-function(id){
  # load edges
  toids=dbGetQuery(citationdbcit,paste0("SELECT `to` FROM edges WHERE `from`='",id,"';"))[,1]
  fromids=dbGetQuery(citationdbcit,paste0("SELECT `from` FROM edges WHERE `to`='",id,"';"))[,1]
  ids=c(id,toids,fromids)
  req = "SELECT * FROM keywords WHERE "
  for(i in ids[1:(length(ids)-1)]){req=paste0(req,"`id`='",i,"' OR ")}
  req=paste0(req,"`id`='",ids[length(ids)],"';")
  res=dbGetQuery(citationdbkws,req)
  l = sapply(res$keywords,function(s){strsplit(s,";")})
  names(l)<-res$id
  return(l)
}



#'
#' @name citationVisuEgo
#' @description visualize an ego network given edges
citationVisuEgo<-function(edges){
  if(!is.null(edges)){
     if(nrow(edges)>0){
      citsubgraph = graph_from_data_frame(edges,directed=TRUE)
      #show(citsubgraph)
      V(citsubgraph)[head_of(citsubgraph,E(citsubgraph))$name]$cyb = E(citsubgraph)$fromcyb
      V(citsubgraph)[tail_of(citsubgraph,E(citsubgraph))$name]$cyb = E(citsubgraph)$tocyb
      V(citsubgraph)[head_of(citsubgraph,E(citsubgraph))$name]$title = E(citsubgraph)$fromtitle
      V(citsubgraph)[tail_of(citsubgraph,E(citsubgraph))$name]$title = E(citsubgraph)$totitle
      lay=layout_as_tree(citsubgraph,circular=FALSE)
      lay[lay[,2]==0,2]=-sample.int(length(which(lay[,2]==0)),replace=FALSE)-2
      #lay[lay[,2]==2,1]= sample.int(10,size=length(which(lay[,2]==2)))-5#((-length(which(lay[,2]==2))/2):(length(which(lay[,2]==2))/2))*5/length(which(lay[,2]==2))
      lay[lay[,2]==2,1]= sample.int(length(which(lay[,2]==2)))-5
      lay[lay[,2]==2,2]=4+sample.int(length(which(lay[,2]==2)),replace=FALSE)
      palette=c("#df691a","#1C6F91")
      par(bg = "#4e5d6c")
      plot(citsubgraph,edge.color="#df691a",edge.arrow.size = 1,
           vertex.label=V(citsubgraph)$title,vertex.color=palette[V(citsubgraph)$cyb+1],
           vertex.frame.color="#1C6F91",vertex.label.color = "#ebebeb",
           layout=lay
      )
      #16283 22232 23337 23502 26325 24841 26026 22270 24798 25354 26969
    }
  }
}





#'
#' @name citationWordclouds
#' @description plots word clouds, one for the keywords of the ref itself, the other for the provided keywords (neighborhood)
citationWordclouds<-function(id,keywords){
  if(id!="0"&!is.null(keywords)){
    # at least kws for the paper, so no need to check emptyness
    par(mfrow=c(1,2))
    par(bg = "#4e5d6c")
    wordcloud(words=keywords[[id]],
              freq=citationkwfreqs[keywords[[id]]],
              colors=unlist(semanticcolors[citationkwthemdico[keywords[[id]]]]),
              ordered.colors = TRUE
              )
    allkws=unlist(keywords)
    wordcloud(words=allkws,
              freq=citationkwfreqs[allkws],
              colors=unlist(semanticcolors[citationkwthemdico[allkws]]),
              ordered.colors = TRUE
    )
  }
}
