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
  # seems impossible to have tree opened and selected from tree.data object
  #stateNotLeafs <- list(opened = TRUE)
  #tree$Set(state = rep(list(stateNotLeafs),length(tree)-nLeafs), filterFun = isNotLeaf)
  # "state": {
  #   "opened": true,
  #   "disabled": false,
  #   "selected": false,
  #   "loaded": false
  # }
  #tree$Do(function(node) node$state = list(opened = TRUE, disabled = FALSE, selected = TRUE, loaded = FALSE))
  
  #tree = lapply(tree, function(x) structure(x, stopened = T))
  
  return(tree)
}


# -----------------------------------------------------------------------------
# OBSERVATIONS
# -----------------------------------------------------------------------------

obs_total_count <- function(df_obs) {
  nrow(df_obs)
}

obs_count_agent <- function(df_obs, n = 1) {
  df_obs %>% group_by(agentName) %>% summarise(count = n()) %>% arrange(desc(count)) %>% top_n(n)
}

obs_count_type <- function(df_obs, n = 1) {
  df_obs %>% group_by(observationType) %>% summarise(count = n()) %>% arrange(desc(count)) %>% top_n(n)
}

obs_count_agent_type <- function(df_obs) {
  df_obs %>% group_by(agentName, observationType) %>% summarise(count = n())
}

obs_count_agent_label <- function(df_obs) {
  df_obs %>% group_by(agentName, conceptLabel) %>% summarise(count = n())
}

obs_count_concept_type <- function(df_obs) {
  df_obs %>% group_by(conceptLabel) %>% summarise(count = n()) #  %>% arrange(desc(count)) 
}

obs_count_concept_label_per_obs_type <- function(df_obs) {
  df_obs %>% group_by(observationType, conceptLabel) %>% summarise(count = n()) %>% arrange(desc(conceptLabel))
}

obs_count_month <- function(df_obs) {
  # per conceptLabel per month
  df_table <- df_obs
  df_table$month <- floor_date(as_date(df_table$when), "month")
  df_table$agent <- as.factor(df_table$agentName)
  
  df <- df_table %>%
    select("conceptLabel", "month", "agent") %>%
    dplyr::group_by(conceptLabel, month) %>%
    dplyr::summarize(observed = n()
    )
  # dplyr::mutate(
  #   avgLength = round(length/numberOfpatrols,0),
  #   avgSpeed = round(length/patrolDuration,0)
  #   )
  
  df
}

obs_count_concept_label_per_time_unit <- function(df_obs, unit) {
  # per time period for all selected conceptLabels
  df_table <- df_obs
  
  df_table$timeUnit <- floor_date(as_date(df_table$when), unit)
  
  df <- df_table %>%
    dplyr::select("conceptLabel", "timeUnit") %>%
    dplyr::group_by(conceptLabel, timeUnit) %>%
    dplyr::summarize(observed = n()
    )
  df
}


# -----------------------------------------------------------------------------
# INTERSECTION
# -----------------------------------------------------------------------------

# check which observations are in which layers
intersect_observations_to_layers <- function(layer, obs) {
  # per feature
  for (i in 1:nrow(layer)){
    # json
    # name is in properties
    #j <- unlist(f[[i]])
    f01 <- layer[i,]
    
    label <- f01$NAME
    
    # make sure coordinate systems are the same
    # we actually overwrite the features crs so this could be wrong...
    f01 <- st_set_crs(f01, st_crs(obs))
    # drop z dimension
    f01 <- st_zm(f01)
    # seems most MULTIPOLYGONS are POLYGONS at the lower level
    f01 <- st_make_valid(f01)
    
    # f01$geometry <- f01$geometry %>%
    #   s2::s2_rebuild() %>%
    #   sf::st_as_sfc()
    
    # check which points intersect with polygon
    tryCatch({
      # do not use s2
      sf::sf_use_s2(FALSE)
      lst <- st_intersects(obs, f01, sparse = FALSE)
      message(paste("LAYER ", label, " is OK"))
      sf::sf_use_s2(TRUE)
      # fill in the name for these points 
      if (sum(lst) > 0) {
        obs[lst,]$area <- label
      }
    }, 
    #error
    error = function(cond) message(paste("LAYER ", label, " has issues"))
    ) #tryCatch
  }
  return(obs)
}


# -----------------------------------------------------------------------------
# VISUALIZATIONS
# -----------------------------------------------------------------------------

# plot agent counts bar chart
plot_top_agents <- function(df_obs) {
  #df_rng_type <- obs_count_agent_label(df_obs)
  df_rng_type <- obs_count_agent_type(df_obs)
  
  # colors per concept label
  colourCount <- length(unique(df_rng_type$observationType)) # number of levels
  
  p <- ggplot(df_rng_type, aes(x = agentName, # x=reorder(agentName,count)
                               y = count,
                               label = count,
                               fill = observationType)) + #observationType conceptLabel 
    geom_bar(aes(reorder(agentName, desc(agentName)), count), stat = "identity") +
    geom_text(aes(label = count ), position = position_stack(vjust = 0.5), show.legend = FALSE) +
    #coord_flip() +
    #ggtitle("Top agents (productivity)") +
    #xlab("Agent") +
    #ylab("Number of observations")
    theme_minimal() +
    scale_fill_manual(values = colorRampPalette(brewer.pal(4, "Paired"))(colourCount)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16)) +
    labs(title = i18n$t("labels.perAgent"),
         # subtitle = i18n$t("labels.countConcept"),
         caption = i18n$t("labels.SCWildCatReport"),
         x = i18n$t("labels.agent"),
         y = i18n$t("labels.count"),
         fill = i18n$t("labels.observationType")
    )
  
  p
}

# plot concept counts bar chart
plot_top_concepts <- function(df_obs) {
  df_cncpt_type <- obs_count_concept_label_per_obs_type(df_obs)
  
  # colors per concept label
  colourCount <- length(unique(df_cncpt_type$observationType)) # number of levels
  
  df_cncpt_type %>% arrange(conceptLabel) %>% ggplot(aes(x = conceptLabel, # no reorder reorder(conceptLabel,count)
                                                         y = count,
                                                         label = count,
                                                         fill = observationType)
  ) + 
    geom_bar(aes(reorder(conceptLabel,desc(conceptLabel)), count), stat = 'identity') +
    geom_text(aes(label = count ), position = position_stack(vjust = 0.5), show.legend = FALSE) +
    #  geom_text(size=10,hjust=1.5) +
    #coord_flip() + 
    theme_minimal() +
    scale_fill_manual(values = colorRampPalette(brewer.pal(4, "Paired"))(colourCount)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = str_to_title(i18n$t("labels.perConcept")),
         # subtitle = i18n$t("labels.countConcept"),
         caption = i18n$t("labels.SCWildCatReport"),
         x = i18n$t("labels.concept"),
         y = i18n$t("labels.count"),
         fill = i18n$t("labels.observationType")
    ) 
}

# plot concepts per month
plot_concepts_month <- function(df_obs, selectedPeriod = "Period", month_from, month_to) {
  df_cnt_month <- obs_count_month(df_obs)
  
  # colors per concept label
  colourCount <- length(unique(df_cnt_month$conceptLabel)) # number of levels
  
  p1 <- ggplot(data = df_cnt_month, aes(x = month, 
                                        y = observed,
                                        fill = conceptLabel)) +
    geom_col() +
    geom_text(aes(label = observed), position = position_stack(vjust = 0.5), show.legend = FALSE) +  # colour='#FFFFFF'
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", limits = c(month_from, month_to)) +
    theme_minimal() +
    scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(colourCount)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = i18n$t("labels.perMonth"),
         # subtitle = i18n$t("labels.countTotal"),
         caption = i18n$t("labels.SCWildCatReport"),
         x = selectedPeriod,
         y = i18n$t("labels.count"),
         fill = i18n$t("labels.concept"),
         color = "Legenda<br>"
    )
  
  p1
}

# plot concepts per area
plot_per_area <- function(df_obs, selectedLayer = "no layer") {
  
  # colors per area
  colourCount <- length(unique(df_obs$conceptLabel)) # number of levels
  
  p1 <- ggplot(data = df_obs, aes(x = area, 
                                  y = numberOfConcepts,
                                  fill = conceptLabel)) +
    geom_col() +
    geom_text(aes(label = numberOfConcepts), position = position_stack(vjust = 0.5), show.legend = FALSE) +  # colour='#FFFFFF'
    #scale_x_date(date_breaks = "1 month",date_labels =  "%b %Y") +
    theme_minimal() +
    scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(colourCount)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = i18n$t("labels.obsConcPerArea"),
         # subtitle = i18n$t("labels.countTotal"),
         caption = i18n$t("labels.SCWildCatReport"),
         x = str_to_title(i18n$t("labels.area")),
         y = i18n$t("labels.count"),
         fill = i18n$t("labels.concept"),
         color = "Legenda<br>"
    )
  p1
}

# plot concepts counts per time unit, including trend
plot_concepts_trend <- function(df_obs, per, per_from, per_to) {
  
  label_x <- i18n$t(paste0("labels.", per))
  
  p1 <- ggplot(data = df_obs, aes(x = timeUnit, 
                                  y = observed,
                                  color = conceptLabel))  +
    geom_point(aes(color = conceptLabel)) +
    geom_line(linetype = "dash") +
    stat_smooth(formula = y ~ x, method = "lm", level = .9, fullrange = T) +
    scale_x_date(date_labels = "%d %b %Y", limits = c(per_from, per_to)) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(0, 2, 2, 2, "cm")) +
    labs(title = paste(i18n$t("labels.observedTrends"),"<br>"),
         # subtitle = paste(i18n$t("labels.countTotal"),"<br>"),
         caption = i18n$t("labels.SCWildCatReport"),
         x = paste("<br>", label_x),
         y = i18n$t("labels.count"),
         color = i18n$t("labels.concept")
         # color = "Legenda<br>"
    )
  
  p1
}


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
