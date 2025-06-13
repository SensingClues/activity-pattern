require(dplyr)
require(data.tree)
require(lubridate)
require(ggplot2)
require(RColorBrewer)
# geospatial
require(sf)
require(stringr)

# multi language
library(shiny.i18n)

tryCatch({
  # try to get online version
  # i18n <- Translator$new(translation_json_path = "https://focus.sensingclues.org/api/labels/list") # Production Environment
  i18n <- Translator$new(translation_json_path = "https://focus.test.sensingclues.org/api/labels/list") # Test Environment
},
error=function(e){
  message("No labels available online, we will use the old ones from disk.")
})

if(!exists("i18n")) {
  # use the stored version
  i18n <- Translator$new(translation_json_path = "translations.json")
}


# -----------------------------------------------------------------------------
# LANGUAGE
# -----------------------------------------------------------------------------

# get system language from locale (Windows)
get_sys_language <- function(OS) {
  default_locale <- Sys.getlocale("LC_CTYPE")
  # Extract the language part from the locale
  language <- sub("_.*", "", default_locale) # on a MAC(OS=Darwin) this is the "short" language, e.g. "en""or "fr"
  # in Windows we still need to convert:
  if(OS=="Windows") {
    lang <- case_when(language == "Dutch" ~ "nl",
                      language == "English" ~ "en",
                      language == "French" ~ "fr",
                      language == "Spanish" ~ "es",
                      .default = "en")
  } else { #if(OS=="Darwin") {
    lang <- language
  } 
  list(lang_long = language, lang_short = lang)
}


# -----------------------------------------------------------------------------
# TREE
# -----------------------------------------------------------------------------

# build tree structure from concepts
# function to build tree structure from concepts and counts
# filter only concepts that have at least one observation
build_tree_from_concepts <- function(concepts, counts) {
  # make data.frame.network from concepts
  dfn <- data.frame()
  for (i in 1:length(concepts)) {
    dfn <- rbind(dfn, c(concepts[[i]]$id, concepts[[i]]$parent, concepts[[i]]$label))
  } 
  
  names(dfn) <- c("from", "to", "label")
  # filter concepts to only the base ontology
  dfn <- dfn %>% filter(grepl("SCCSSOntology", from))

  tree <- FromDataFrameNetwork(dfn, check = "check")
  trav <- Traverse(tree)
  trav_name <- Get(trav, "name")
  trav_label <- Get(trav, "label")
  # set new attribute
  tree$Set(text = trav_label)
  tree$Set(iri = trav_name)
  tree$Set(id = trav_label)
  
  
  # transform counts to dataframe
  dfs <- lapply(counts, data.frame, stringsAsFactors = FALSE)
  df_counts <- bind_rows(dfs)
  # order df according to traverse sequence
  f <- df_counts[match(trav_name, df_counts$X_value), "frequency"]

  # set new attribute
  tree$Set(freq = f)
  
  # prune tree freq > 0
  Prune(tree, function(x) !is.na(x$freq) )
  
  # combine name and counts
  c <- paste0(tree$Get("text"), " (", tree$Get("freq"), ")")
  tree$Set(text = c)
  
  # add state selected = TRUE
  stateLeafs <- list(opened = TRUE, selected = TRUE)
  nLeafs <- tree$leafCount
  tree$Set(state = rep(list(stateLeafs), nLeafs), filterFun = isLeaf)
  return(tree)
}


# -----------------------------------------------------------------------------
# VISUALIZATIONS
# -----------------------------------------------------------------------------

# table might be handy later?
show_DT_table <- function(df, lang_long, copy_text, csv_text) {
  DT::datatable(df,
                extensions = "Buttons",
                options = list(
                  language = list(url = paste0('//cdn.datatables.net/plug-ins/1.10.11/i18n/',lang_long,'.json')),
                  paging = TRUE,
                  searching = TRUE,
                  fixedColumns = TRUE,
                  autoWidth = TRUE,
                  ordering = TRUE,
                  dom = 'frtip<"sep">B', 
                  pageLength = 999,
                  buttons = list(
                    list(
                      extend = "copy",
                      text = copy_text
                    ), 
                    list(
                      extend = "csv",
                      text = csv_text
                    ))
                )
  )
}
