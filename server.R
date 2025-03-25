## Translating heatmap plot into RShiny App SC - server script
## Author: H. Fricke
## Date: 22-03-2025
## Description:
## To-Do:
## [] selectInput to observeEvent

options(shiny.reactlog = TRUE)



## SET UP libraries and sourced files
#  Define server logic (other libraries in ui)
library(DT)
library(sf)
library(dplyr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(ipc)
library(future)
library(promises)

# load the sensincluesr package
library(devtools)
devtools::install_github("sensingclues/sensingcluesr@v1.0.3", upgrade = "never")
# dynamic color maps for more then 12 colors
library(colorRamps)


# as part of future package we need to define where the future is executed,
# multisession means we are launching background R processes on the same machine
# other options are multicore (not on Windows) and multiprocess
plan(multisession)

# source function lib
source("functions.R")

## Some language package stuff

### CB: added these lines for treeToJSON

# library(devtools)
# install_github("shinyTree/shinyTree")

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


#----------------------- SERVER ------------------------------------------------

server <- function(input, output, session) {
  # error logging
  message("=========== Starting Observation report App =============")
  
  # -- SET UP LANGUAGE STUFF --
  
  # js code to get the browser language
  js_lang <- "var language =  window.navigator.userLanguage || window.navigator.language;
              Shiny.onInputChange('browser_language', language);
              console.log(language);"
  runjs(js_lang)
  
  language <- get_sys_language(Sys.info()['sysname']) # gives a long and a short version, e.g. "English" and "en"
  lang_short <- language[["lang_short"]] # language according to system (short="en" instead of "English")
  lang_long <- language[["lang_long"]] # language according to system (long="English" instead of "en")
  #lang_user <- lang_short # for use in most api functions (long version is needed for DT package)
  
  # set initial language to english
  i18n$set_translation_language("en")
  
  obs0 <- observe({    
    req(input$ok)
    isolate({
      # set language to browser language (or "en") and get the appropriate json translation file
      i18n$set_translation_language(input$lang)
    })
  })
  # -- end of language stuff --
  
  # -- SET UP SESSION OBJECT TO STORE DATA --
  
  session$userData <- reactiveValues(authenticated = FALSE)
  #session$userData$authenticated <- FALSE
  session$userData$clueyUser <- NULL
  session$userData$groups <- NULL
  session$userData$selectedGroup <- "none"
  session$userData$filterConcepts <- NULL
  session$userData$currentConcept <- NULL
  session$userData$hierarchy <- NULL # sensingcluesr::get_hierarchy()
  session$userData$concepts <- NULL # hierarchy$concepts
  session$userData$cookie_mt <- NULL
  session$userData$date_to <- Sys.Date()
  session$userData$date_from <- Sys.Date() - 30
  session$userData$url <- "https://focus.sensingclues.org/"
  session$userData$layers <- NULL
  reactive( session$userData$selectedLayer <- i18n$t("labels.outsideArea") )
  session$userData$aoi <- ""
  session$userData$Go <- NULL
  languages <- c("en","English",
                 "fr","French",
                 "nl","Dutch")
  # "es","Spanish",
  # "pl","Polski")
  lang_arr <- array(languages,dim=c(2,length(languages)/2))
  language_table <- data.frame(aperm(lang_arr))
  names(language_table) <- c("lang_short","lang_long")
  session$userData$language_table <- language_table
  session$userData$language_choices <- language_table$lang_short
  session$userData$lang_short <- lang_short
  session$userData$lang_long <- lang_long
  
  # allows to close modal on clicking enter
  js <- '
  $(document).keyup(function(event) {
    if ($("#password").is(":focus") && (event.keyCode == 13)) {
        $("#ok").click();
    }
  });
  '
  # -- End session object  --
  
  # Get language from browser language
  observeEvent(input$browser_language, {
    # let user choose language, pre-filled = browser language, unless this language is
    # not (yet) supported, then default is "en" (English)
    session$userData$inp_lang <- substr(input$browser_language,1,2)
    
    session$userData$sel_lang <- ifelse(session$userData$inp_lang %in% session$userData$language_choices,
                                        session$userData$inp_lang, "en")
    
    # set language to browser language (or "en") and get the appropriate json translation file
    # path <- paste0(session$userData$url_translation, sel_lang)
    # i18n <- Translator$new(translation_json_path = path)
    i18n$set_translation_language(session$userData$sel_lang)
    
    updateSelectInput(session, "lang", choices=session$userData$language_choices, selected = session$userData$sel_lang)
    
    message('browserlanguage is: ',session$userData$inp_lang)
    message('chosen language is: ',session$userData$sel_lang)
  })
  
  
  # -- MAKE POP UP MODAL FOR ENTERING USER CREDENTIALS AND DATA
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      tags$script(HTML(js)),
      title = i18n$t("labels.clueyCredentials"), #"Cluey credentials",
      # the selectbox for a server will only show in apps for testing
      if (grepl("test", session$clientData$url_pathname)) {
        message(paste("Adding selectbox for server because we are running on", session$clientData$url_pathname))
        selectInput("server", label = "Server", 
                    choices = c("focus.sensingclues", "focus.test.sensingclues"), 
                    selected = "focus.sensingclues")
      },
      selectInput("lang", label = i18n$t("labels.chooseLanguage"), 
                  choices = language_table$lang_short, selected = session$userData$sel_lang), #lang_short),
      textInput("username", i18n$t("labels.Cluey-username")),
      passwordInput("password", i18n$t("labels.Cluey-password")),
      size = "s",
      if (failed) 
        div(tags$b(i18n$t("labels.invalid-credential"), style = "color: red;")),
      footer = tagList(
        actionButton("ok", "OK")
      )
    )
  }
  
  resetModal <- function() {
    modalDialog(
      title = i18n$t("commands.logout"),
      size = "s",
      footer = tagList(
        modalButton(i18n$t("commands.cancel")),
        actionButton("reset", "OK")
      )
    )
  }
  resetModal <- function() {
    modalDialog(
      title = i18n$t("commands.logout"),
      size = "s",
      footer = tagList(
        modalButton(i18n$t("commands.cancel")),
        actionButton("reset", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.
  # This `observe` is suspended only with right user credential
  obs1 <- observe({
    showModal(dataModal())
  })
  
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal.
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <-  input$username
      Password <-  input$password
      session$userData$clueyUser <- Username
    })
    
    message(input$server)
    # set server
    if(isolate(hasName(input, "server"))) {
      if (input$server == "focus.sensingclues") {
        session$userData$url <- "https://focus.sensingclues.org/"
      } else {
        session$userData$url <- "https://focus.test.sensingclues.org/"
      }
    }
    message(paste("LOGGING INTO", session$userData$url))
    
    session$userData$cookie_mt <- sensingcluesr::login_cluey(username = Username, password = Password, url = session$userData$url)
    if (!is.null(session$userData$cookie_mt)) {
      session$userData$authenticated <- TRUE
      obs1$suspend()
      removeModal()
      # after successful login
      session$userData$hierarchy <- sensingcluesr::get_hierarchy(url = session$userData$url, lang = session$userData$lang_short) 
      session$userData$concepts <- session$userData$hierarchy$concepts
      
      # put start en end date in dateRangeInput
      updateDateRangeInput(session, "DateRange",
                           start = isolate(session$userData$date_from),
                           end = isolate(session$userData$date_to),
                           max = Sys.Date())
      
      # which layers are available to the user
      layers <- sensingcluesr::get_layer_details(cookie = session$userData$cookie_mt, url = session$userData$url)
      # filter layers of the (Multi)Polygon type for the per area tab
      session$userData$layers <- layers %>% filter(geometryType %in% c("Polygon", "MultiPolygon"))
      # message(paste0("LAYERS ", paste(session$userData$layers, sep = "|")))
      
      # enable input fields/buttons
      enable("DateRange")
      enable("GroupListDiv")
      enable("GetData")
      
    } else {
      session$userData$authenticated <- FALSE
      # inform user
      showModal(dataModal(failed = TRUE))
    }     
  }
  )
  
  # -- End modal stuff --
  
  # Evt. taal wijzigen 
  
  obs3 <- observe({
    req(input$ok)
    isolate({
      session$userData$lang_short <- input$lang
      # get corresponding long version of language (for DT)
      row <- which(session$userData$language_table$lang_short == input$lang)
      session$userData$lang_long <- session$userData$language_table$lang_long[row]
      message(paste("Language change!", input$lang))
      shiny.i18n::update_lang(input$lang)
    })
  })
  
  # ------- OUTPUT SIDE - SIDE PANEL SHOWING GROUPS, DATA DIVIDED IN TREE ---------
  
  output$userstatus <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    req(input$ok)
    isolate({
      Username <- input$username
    })
    if (session$userData$authenticated) {
      strong(paste(i18n$t("labels.connectedAs"), Username))
    }
  })
  
  # observe group select box
  observeEvent(input$GroupList, { # input is defined in ui.R
    req(session$userData$groups) # dataset groups
    message("Observe event on Group Select box")
    
    session$userData$selectedGroupValue <- input$GroupList
    # translate group value(name) to group name(id)
    selectedGroup <- session$userData$groups %>% 
      filter(value %in% session$userData$selectedGroupValue) %>% 
      select(name) # user selection of group to visualise/?
    # for debugging
    message(paste("User", session$userData$clueyUser, "selected group", selectedGroup))
    session$userData$selectedGroup <- selectedGroup
    # invalidate the current data
    session$userData$obsDataAvailable <- FALSE
    message("Invalidating data as new group selected")
    
    # NEEDED?? retrieve the concept counts 
    counts <- sensingcluesr::get_concept_counts(from = session$userData$date_from,
                                                to = session$userData$date_to,
                                                group = selectedGroup,
                                                cookie = session$userData$cookie_mt,
                                                url = session$userData$url,
                                                lang = session$userData$lang_short) # do I need that?
    message(paste(as.character(now()), "Concept counts", length(counts)))
    # show message if counts are empty
    if (is.null(counts)) {
      session$userData$counts <- 0
      showModal(modalDialog(
        title = i18n$t("labels.zeroCount"),
        i18n$t("labels.zeroCountExplanation"), 
        footer = modalButton(i18n$t("commands.dismiss"))
      ))
    } else {
      session$userData$counts <- counts
    }
  })
  
  ## DONT NEED? -- no maps
  # # build layer of interest selection box 
  # output$SelectLayer <- renderUI({
  #   message("Render layer select box")
  #   if (session$userData$authenticated) {
  #     choices <- c(i18n$t("labels.outsideArea") , sort(unlist(session$userData$layers$layerName)))
  #     # render box if we have groups else do nothing
  #     if (length(choices) != 0) {
  #       message(paste0("Render select layer for ", session$userData$clueyUser, " with layers ", paste(choices, collapse = " ")))
  #       selectInput("LayerList",
  #                   i18n$t("labels.selectArea"),
  #                   choices = choices,
  #                   multiple = FALSE,
  #                   selected = session$userData$selectedLayer
  #       )
  #     } # no choices
  #     
  #   } # not authenticated
  # })
  # 
  # 
  # ### observe layer select box
  # observeEvent(input$LayerList, {
  #   req(input$ok)
  #   message("Observe event on Layer selectbox")
  #   session$userData$selectedLayer <- input$LayerList
  #   
  #   if (session$userData$selectedLayer != i18n$t("labels.outsideArea")) {
  #     message(paste(" Layer ", session$userData$selectedLayer, " selected"))
  #     lrs <- session$userData$layers %>% filter(layerName == session$userData$selectedLayer)
  #     # get list of geojson multipolygons
  #     f <- sensingcluesr::get_layer_features(projectId = lrs$pid,
  #                                            layerId = lrs$lid,
  #                                            cookie = session$userData$cookie_mt,
  #                                            url = session$userData$url)
  #     # now we need to label the observations by layer
  #     # we expect the observations to be available
  #     # this needs to be an sf object
  #     session$userData$my_sf <- reactive({f})
  #     
  #     
  #   } else {
  #     # no layer so nothing to do
  #     message("LAYER EMPTY")
  #   }
  #   
  #   # invalidate the current data
  #   #session$userData$obsDataAvailable <- FALSE
  #   #message("Invalidating data as new group selected")
  #   
  # },
  # ignoreInit = TRUE)
  
  
  # Get the date range based on user input and get the groups for which data is available in the date range
  observeEvent(input$DateRange, {
    req(session$userData$authenticated)  ## ensures things are authenticated
    req(input$DateRange[1])
    req(input$DateRange[2])
    message("Observe event on Date Range")
    
    # keep previous date range
    from_p <- session$userData$date_from
    to_p <- session$userData$date_to
    # store new date range
    session$userData$date_from <- input$DateRange[1]
    session$userData$date_to <- input$DateRange[2]
    
    if (input$DateRange[1]>input$DateRange[2]) {
      # incorrect date range
      showModal(modalDialog(
        title = i18n$t("labels.incorrectDateRange"),
        i18n$t("labels.startBeforeEnd"), 
        footer = modalButton(i18n$t("commands.dismiss"))
      ))
      # restore date range in ui to previous values
      message(paste("Restore date range in UI from", from_p, " to", to_p))
      
      # set date range back to previous values
      updateDateRangeInput(session, "DateRange",
                           start = from_p,
                           end = to_p)
    } else {
      message(paste("Adjusted date range from", session$userData$date_from, "to", session$userData$date_to))
      # invalidate the current data
      session$userData$obsDataAvailable <- FALSE
      message("Invalidating data as we selected new date range")
      
      # check which groups are available in this date range
      message(paste(as.character(now()), "GETGROUPS for", session$userData$clueyUser, "from", 
                    session$userData$date_from, "to", session$userData$date_to)) #makes sure it only displays groups if data is available in that time period
      
      # make call to focus backend to get groups
      session$userData$groups <- sensingcluesr::get_groups(from = session$userData$date_from,
                                                           to = session$userData$date_to,
                                                           cookie = session$userData$cookie_mt,
                                                           url = session$userData$url)
      
      if (length(session$userData$groups) == 0) {
        # no groups in this date range
        showModal(modalDialog(
          title = i18n$t("labels.noGroups"),
          i18n$t("labels.noGroupsExplanation"),
          footer = modalButton(i18n$t("commands.dismiss"))
        ))
      }
      
      # update choices in Select Group
      updatePickerInput(session, "GroupList", i18n$t("labels.selectGroup"), 
                        choices = as.list(sort(session$userData$groups$value)))
    }
  })
  
  # -- New inputs - Hanna
  
  # Generate input fields per season dynamically based on number of seasons user desires
  output$season_inputs <- renderUI({
    req(input$num_seasons) # required input before going further
    lapply(1:input$num_seasons, function(i) {
      textInput(inputId = paste0("season_", i), label = paste("Season", i, "(e.g., '12,1,2' for Dec-Jan-Feb):"), value = "") 
    }) # loop through number of seasons and add suffix (e.g. 2 seasons = season_1, season_2)
  })
  message(paste0())
  # Reactive expression to parse user-defined seasons
  user_defined_seasons <- reactive({ 
    req(input$num_seasons)
    seasons <- list() 
    for (i in 1:input$num_seasons) {
      season_input <- input[[paste0("season_", i)]]
      if (!is.null(season_input) && season_input != "") {
        months <- unlist(strsplit(season_input, ","))
        seasons[[paste0("Season_", i)]] <- sprintf("%02d", as.numeric(trimws(months)))
      }
    }
    seasons
  })  
  
  # -- New inputs end 
  
  # -- TREE FOR SELECTION OF DATA SUBSETS ---
  
  # needs to be reactive to change in date or different group selected
  tree <- eventReactive({
    input$DateRange
    input$GroupList
    1
  }, {
    # disable input fields/buttons
    disable_all(c("DateRange", "GroupListDiv", "GetData"))
    
    # update concept counts again?
    counts <- sensingcluesr::get_concept_counts(from = session$userData$date_from,
                                                to = session$userData$date_to,
                                                group = session$userData$selectedGroup,
                                                cookie = session$userData$cookie_mt, 
                                                url = session$userData$url)
    if (!(is.null(counts))) {
      session$userData$counts <- counts
      message(paste0("Concept counts in this date range ", length(session$userData$counts)))
    }
    
    # build the tree from the hierarchy concepts and the concept counts --> will give you the hierachy and the counts within the tree
    tryCatch({
      ptm_tree <- proc.time()["elapsed"]
      session$userData$tree <- suppressMessages(build_tree_from_concepts(session$userData$concepts, session$userData$counts)) # dit is een data.tree Node R6
      message(paste("Tree built in", round(proc.time()["elapsed"] - ptm_tree, 2), "seconds"))
      
      # remove the subtree below the Community Work node (6477) and
      # the subtree below the Tracks node (42) which do not belong in this report
      # if there are no observations left, an empty tree is returned
      top_nodes <- sapply(session$userData$tree$children, function(x) x$name)
      cw_node <- "https://sensingclues.poolparty.biz/SCCSSOntology/6477"
      tr_node <- "https://sensingclues.poolparty.biz/SCCSSOntology/42"
      if (cw_node %in% top_nodes) {invisible(session$userData$tree$RemoveChild(cw_node))}
      if (tr_node %in% top_nodes) {invisible(session$userData$tree$RemoveChild(tr_node))}
      if (length(session$userData$tree$children) == 0) {session$userData$tree <- list()}
      jsonTree <- shinyTree::treeToJSON(session$userData$tree, pretty = TRUE, createNewId = FALSE)
      enable_all(c("DateRange", "GroupListDiv", "GetData"))
      return(jsonTree)
    }, error = function(e) {
      message("The tree could not be built. Returning an empty list.")
      session$userData$tree <- list()
      enable_all(c("DateRange", "GroupListDiv", "GetData"))
      return()
    })
  }) # end tree
  
  output$conceptTree <- renderTree({
    # only render after we have selected a group and retrieved the counts from that group
    req(session$userData$counts, session$userData$concepts)
    message("Render concept tree")
    tree()
  }) 
  
  observeEvent(input$conceptTree, {
    # we need to check if a user selects new concepts
    cTree <- input$conceptTree # this is a list
    #req(cTree)
    message("Observe event on concept tree")
    #message(paste0("Concept tree : ", cTree))
    # save current selected concepts
    availableConcepts <- session$userData$availableConcepts
    # get new selected concepts
    newConcepts <- get_selected(cTree, format = "names")
    
    # check if new selected concepts are a subset of the old ones, if so we do not have to get new data
    # if (newConcepts %in% previousConcepts) {
    #   # new set is subset we do not need to do anything
    # } else {
    #   # we need to reload the data from the backend
    #   session$userData$obsDataAvailable <- FALSE
    # }
    session$userData$obsDataAvailable <- FALSE
    message("Invalidating data as we selected new concepts in tree")
    
    #message(paste0("Available concepts from data ", paste0(availableConcepts, collapse = "|")))
    #message(paste0("Selected new concepts from tree ", paste0(newConcepts, collapse = "|")))
    #message(length(concepts))
    # check if not empty
    if (!length(newConcepts) == 0) {
      # unlist structure
      #concepts <- names(concepts) # unlist
      #message(paste0("Selected concepts : ", concepts))
      conceptIris <- list()
      for (i in 1:length(newConcepts)) {
        # message(paste0("Concept : ", concepts[i]))
        nm <- trimws(
          unlist(
            strsplit(
              gsub("^.*[.]", "",
                   newConcepts[i]
              )
              , "[(]")
          )[1]
        ) # character class in regex
        # message(paste0("Name : ", nm))
        # get iri from hierarchy
        iri <- sensingcluesr::get_id(nm, session$userData$hierarchy)
        # message(paste0("IRI : ", iri))
        conceptIris <- c(conceptIris, iri)
      }
      # replace filtered concepts
      session$userData$filterConcepts <- conceptIris
      
      #message(paste0("Selected concept IRI IDs ", paste0(gsub("^.*/", "", session$userData$filterConcepts), collapse = " ")))
    } else {
      message("No concepts selected in the tree")
      
      session$userData$filterConcepts <- NULL
    }
  })
  
  
  # -- BUILD DATASETS FOR VISUALISATION AND EXPORT --
  
  disable_all <- function(elements){
    # disable input fields/buttons
    for (element in elements) {
      disable(element)
    }
  }
  
  enable_all <- function(elements){
    # enable input fields/buttons
    for (element in elements) {
      enable(element)
    }
  }
  
  # show modal to user when no concepts are selected
  observeEvent(input$GetData, {
    if (is.null(session$userData$filterConcepts)) {
      showModal(modalDialog(
        title = i18n$t("labels.noConcSel"),
        i18n$t("labels.noConcSelExplanation"), footer = modalButton(i18n$t("commands.dismiss"))
        # title = "No concepts selected",
        # "Please select at least one concept from the tree"
      ))
      session$userData$Go <- FALSE
    } else session$userData$Go <- TRUE
  })
  
  # build datasets
  session$userData$obsdata <- eventReactive(input$GetData, {
    req(session$userData$Go)
    # start when get data is clicked
    #req(input$GetData)
    message(as.character(now()), " Get data button activated")
    
    disable_all(c("GetData", "DateRange", "GroupListDiv"))
    # "conceptTree" cannot be disabled, it remains adaptable
    
    # get an estimation of #observations from #concepts, for progress bar
    # this is an overestimation since observations may contain multiple concepts
    counts <- session$userData$counts
    total <- sum(sapply(counts, function(x) x$frequency))
    leafs <- session$userData$tree$Get("name", filterFun = isLeaf)
    leafs_counts <- counts[sapply(counts, function(x) (x$'_value' %in% leafs) & (x$'_value' %in% session$userData$filterConcepts))]
    total_leafs <- sum(sapply(leafs_counts, function(x) x$frequency))
    message(paste("#concepts", total_leafs))
    
    # Create an asynchronous Progress object
    progress <- AsyncProgress$new(session, message = i18n$t("labels.gettingObs"), 
                                  value = 0, max = total_leafs)
    # Make sure it closes when we exit this reactive, even if there's an error
    # on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
      #progress$inc(value = value, detail = detail)
    }
    
    date_from <- session$userData$date_from
    date_to <- session$userData$date_to
    selectedGroup <- session$userData$selectedGroup
    filterConcepts <- session$userData$filterConcepts
    cookie_mt <- session$userData$cookie_mt
    url <- session$userData$url
    lang_short <- session$userData$lang_short
    
    
    # get observations, future promise prevents freezing 
    future_promise({
      sensingcluesr::get_observations(from = date_from,
                                      to = date_to,
                                      # aoi = session$userData$aoi, # TODO issue with backend we cannot use drawings now
                                      group = selectedGroup,
                                      filteredConcepts = filterConcepts,
                                      cookie = cookie_mt,
                                      url = url,
                                      allAttributes = TRUE,
                                      updateProgress = updateProgress,
                                      lang = lang_short) # get concept names in desired user language
    }) %>% finally(~progress$close()) %>%
      finally(~enable_all(c("GetData", "DateRange", "GroupListDiv"))) # finally only takes a single function
    # enable("conceptTree") does not work
    
  })
  
  # Make the data object on which visualisations are based
  session$userData$processed_obsdata <- reactive({
    session$userData$obsdata() %...>% {
      df <- .
      
      # check if empty
      if (nrow(df) == 0) {
        # message
        showModal(modalDialog(
          title = i18n$t("labels.noObs"),
          i18n$t("labels.noObsExplanation", footer = modalButton(i18n$t("commands.dismiss")))
        ))
      } else {
        # rbind kills types so we cast datetimes here
        message(paste("Timezone of the user is", input$user_timezone))
        df$when <- lubridate::as_datetime(df$when, tz = input$user_timezone) # format is cluey timestamp "%Y-%m-%dT%H:%M:%S%z", %z shows as +0100 etc.
        
        # # parse geojson to lat lon
        df <- df %>%
          rowwise() %>%
          mutate(lon = fromJSON(where)$coordinates[1], lat = fromJSON(where)$coordinates[2])

        # we only want to count leaf concepts
        # get leafs
        leafs <- session$userData$tree$Get("name", filterFun = isLeaf)
        # filter on leafs only
        df <- df %>% filter(conceptId %in% leafs)
        #message(paste0(nrow(df), " observations after filtering on leafs ", paste0(gsub("^.*/", "", leafs), collapse = " ")))

        # transform labels to factors
        df$entityName <- as.factor(df$entityName)
        df$observationType <- as.factor(df$observationType)
        df$conceptLabel <- as.factor(df$conceptLabel)
        
        # recode observation type to new labels
        # Offence = Human activity
        # Animal = Animal sighting
        # HWC = Human Wildlife Conflict
        # Infrastructure = Point of interest
        df <- df %>%
          mutate(observationType = recode(observationType, 
                                          community = i18n$t("labels.communityWork"),
                                          offence = i18n$t("labels.humanActivity"),
                                          animal = i18n$t("labels.animalSighting"),
                                          hwc = i18n$t("labels.HWC"),
                                          infrastructure = i18n$t("labels.pointOfInterest"))
          )
        
        # rename
        df <- df %>% rename(observationId = entityId)
        
        # for debugging purposes we dump the observations
        #write.table(df, file = "observations.tab", sep = "\t", quote = FALSE, row.names = FALSE)
      }
    }
  })
  
  ## MAKE EXPORT DATA --> REMOVED THE TAB , INCLUDE LATER AGAIN
  
  session$userData$exportData <- reactive({
    session$userData$processed_obsdata() %...>% {
      df <- .
      # for (potential) export of all data, we group the concepts per observation
      df <- df %>%
        group_by(observationId) %>%
        mutate(numberConcepts = n(), # the dataframe was split per unique concept
               concepts = paste(conceptLabel, collapse = " | "),
               conceptIds = paste(conceptId, collapse = " | ")) %>%
        ungroup() %>% 
        distinct(observationId, .keep_all = TRUE) %>% # similar to group_by but allows to keep all columns
        select(-conceptLabel, -conceptId) %>%
        relocate(observationId, observationType, agentName, when, lat,lon, numberConcepts, concepts, description)
      
      df$description <- gsub("[\r\n]", " ", df$description) # take out new line symbols because of write.csv
      df
    }
  })
  
  session$userData$exportData_withColnames <- reactive({
    
    session$userData$exportData() %...>% {
      df <- .
      
      # convert POSIXct to character that includes timezone, for datatable
      df$when <- format(df$when, format = "%Y-%m-%dT%H:%M:%S%z") # %z shows as +0100 etc.
      
      # get column headers in correct language
      # the ones that don't exist will become "columns.col", with col the original name
      for (col in names(df)) {
        names(df)[which(names(df) == col)] <- i18n$t(paste0("columns.",col)) 
      }
      
      # get the remaining column headers from identically named labels (done to prevent inconsistencies)
      colnr <- which(substr(names(df),1,8) == "columns.")
      for (col in colnr) {
        names(df)[col] <- i18n$t(paste0("labels.",gsub("columns.","",names(df)[col]))) # i.e. look for labels.XXX instead of columns.XXX
      }
      
      # get the remaining column headers from identically named labels (done to prevent inconsistencies)
      colnr <- which(substr(names(df),1,7) == "labels.")
      for (col in colnr) {
        names(df)[col] <- gsub("labels.","",names(df)[col]) # i.e. revert header back to original
      }
      df
    }
  })
  
  
  output$downloadData <- downloadHandler(
    message("DOWNLOAD HANDLER STARTED"),
    filename = function() {
      paste(session$userData$selectedGroupValue, "_", session$userData$date_from, "_", session$userData$date_to, ".csv", sep = "")
    },
    content = function(file) {
      
      session$userData$exportData() %...>% {
        df <- .
        
        # convert POSIXct to character that includes timezone, for csv
        df$when <- format(df$when, format = "%Y-%m-%dT%H:%M:%S%z") # %z shows as +0100 etc.
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  ## Translate data to session logic
  # Reactive to get processed data
  
  # # RINCLUDE YEAR FILTER HERE
  session$userData$filtered_data <- reactive({
    session$userData$processed_obsdata() %...>% { df <- .

    # Debugging messages
    message("Debug: df has ", nrow(df), " rows")

    # Apply the year filter if selected_year is not "All"
    if (input$selected_year != "All") {
      df <- df %>% filter(format(when, "%Y") == input$selected_year)
    }

    return(df)  # Return the filtered dataframe
    }}
  )
  
  
  # WORKS TILL HERE 
  
session$userData$plot_data <- reactive({
    time_input <- input$time_input
    seasons <- user_defined_seasons()
    
    message("Debug - plot_data(): Waiting for filtered_data()")
    
    session$userData$filtered_data() %...>% { 
      df <- . 
      
      df <- df %>%
        mutate(
          Period = case_when(
            time_input == "hourly"  ~ format(when, "%H"),
            time_input == "monthly" ~ format(when, "%m"),
            time_input == "season"  ~ purrr::map_chr(format(when, "%m"), function(month) {
              season <- names(seasons)[sapply(seasons, function(s) month %in% s)]
              if (length(season) > 0) season else NA_character_
            }),
            TRUE ~ NA_character_
          )
        )

      message("Debug - plot_data(): Applied Period transformation.")
      
      # Ensure factor levels for Period
      if (time_input == "hourly") {
          df$Period <- factor(df$Period, levels = sprintf("%02d", 0:23), labels = paste0(sprintf("%02d", 0:23), "h"))
      } else if (time_input == "monthly") {
          df$Period <- factor(df$Period, levels = sprintf("%02d", 1:12), labels = month.abb)
      } else if (time_input == "season") {
          df$Period <- factor(df$Period, levels = names(seasons))
      }
      
      message("Debug - plot_data(): Assigned factor levels for Period.")
      message("Debug - plot_data(): Returning transformed dataframe with ", nrow(df), " rows.")
      
      return(df)
    }
  })

  
  ## Bar data for visualization
  session$userData$bar_data <- reactive({
    session$userData$plot_data() %...>% {
      df <- .
      
      # Perform transformations
      df <- df %>%
        group_by(conceptLabel) %>%
        summarise(Counts = n(), .groups = 'drop')
      message(paste(
        "Debug - bar_data(): Bar data transformation complete with",
        nrow(df),
        "rows."
      ))
      
      df  # Return the transformed data frame
    }
  })
  
  session$userData$bar_data <- reactive({
    session$userData$plot_data() %...>% { 
      df <- .
      
      # Group data by conceptLabel to generate bar_data
      bar_df <- df %>%
        group_by(conceptLabel) %>%
        summarise(Counts = n(), .groups = 'drop')

    }
  })

  session$userData$heatmap_data <- reactive({
    session$userData$plot_data() %...>% { 
      df <- .
      message("Debug - heatmap_data(): Resolving plot_data. Initial rows: ", nrow(df))
      
      # Ensure required columns exist before proceeding
      required_cols <- c("conceptLabel", "Period")
      missing_cols <- setdiff(required_cols, colnames(df))
      if (length(missing_cols) > 0) {
        message("Error - heatmap_data(): Missing required columns: ", paste(missing_cols, collapse = ", "))
        return(data.frame())  # Return an empty dataframe to avoid crashes
      }
      
      # Group and summarize
      df <- df %>%
        group_by(conceptLabel, Period) %>%
        summarise(Counts = n(), .groups = 'drop')
      message("Debug - heatmap_data(): Grouped and summarized data. Rows after grouping: ", nrow(df))
      
      # Ensure all periods are represented
      full_periods <- expand.grid(
        conceptLabel = unique(df$conceptLabel),
        Period = levels(factor(df$Period))
      )
      message("Debug - heatmap_data(): Created full period grid with ", nrow(full_periods), " rows.")
      
      df <- full_periods %>%
        left_join(df, by = c("conceptLabel", "Period")) %>%
        mutate(Counts = ifelse(is.na(Counts), 0, Counts))
      message("Debug - heatmap_data(): Applied full periods and resolved missing values. Rows after merging: ", nrow(df))
      
      # Initialize df_result
      df_result <- df
      
      # Resolve bar_data() BEFORE using it
      session$userData$bar_data() %...>% { bar_df <- .
      
      if (is.null(bar_df) || nrow(bar_df) == 0) {
        message("Warning - heatmap_data(): bar_data() is NULL or empty, skipping Top X filter.")
      } else {
        message("Debug - heatmap_data(): bar_data() has ", nrow(bar_df), " rows.")
        
        # Extract top concept labels and remove NAs
        top_concepts <- unique(bar_df$conceptLabel)
        top_concepts <- top_concepts[!is.na(top_concepts)]  # Remove any NA values
        
        if (length(top_concepts) == 0) {
          message("Warning - heatmap_data(): No valid top concepts found, skipping filter.")
        } else {
          message("Debug - heatmap_data(): Filtering for Top X concepts: ", paste(top_concepts, collapse = ", "))
          
          # Ensure conceptLabel has no NAs before filtering
          df_result <- df_result %>% filter(!is.na(conceptLabel) & conceptLabel %in% top_concepts)
          message("Debug - heatmap_data(): Applied Top X filter. Rows remaining: ", nrow(df_result))
        }
      }
      return(df_result)  # ✅ Return df_result instead of modifying df
      }
    }
  })
  

  
  output$combined_plot <- renderPlotly({
    
    # Resolve bar_data and heatmap_data asynchronously
    session$userData$bar_data() %...>% { bar_data_df <- .
    session$userData$heatmap_data() %...>% { heatmap_data_df <- .
    
    # Validate resolved data
    if (is.null(bar_data_df) || nrow(bar_data_df) == 0) {
      message("Error - combined_plot(): bar_data_df is NULL or empty!")
      return(NULL)
    }
    if (is.null(heatmap_data_df) || nrow(heatmap_data_df) == 0) {
      message("Error - combined_plot(): heatmap_data_df is NULL or empty!")
      return(NULL)
    }
    
    # Order species based on bar chart counts
    ordered_species <- bar_data_df %>% arrange(Counts) %>% pull(conceptLabel)
    
    # Reorder factor levels in heatmap
    heatmap_data_df$conceptLabel <- factor(
      heatmap_data_df$conceptLabel,
      levels = unique(c(ordered_species, heatmap_data_df$conceptLabel))
    )
    
    # Create bar chart
    bar_chart <- plot_ly(
      data = bar_data_df,
      x = ~ Counts,
      y = ~ conceptLabel,
      type = 'bar',
      orientation = 'h',
      text = ~ Counts,
      textposition = 'outside',
      marker = list(
        color = 'rgba(50, 171, 96, 0.6)',
        line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)
      )
    ) %>%
      layout(
        title = 'Total Counts per Species and Time Period',
        xaxis = list(title = 'Counts'),
        yaxis = list(title = 'Species', categoryorder = "total ascending")
      )
    
    # Create heatmap
    heatmap <- plot_ly(
      data = heatmap_data_df,
      x = ~ Period,
      y = ~ conceptLabel,
      z = ~ Counts,
      text = ~ Counts,
      texttemplate = "%{text}",
      hoverinfo = 'text',
      colorbar = list(title = 'Counts'),
      type = 'heatmap',
      colorscale = 'Greens',
      showscale = TRUE,
      reversescale = TRUE
    ) %>%
      layout(
        title = 'Counts per Species',
        xaxis = list(title = 'Time Period'),
        yaxis = list(title = 'Species')
      )
    
    # Combine the plots
    subplot(bar_chart, heatmap, nrows = 1, margin = 0.05) %>%
      layout(title = 'Activity Pattern')
    }}
  })  
  
  
  
  # ### TAB RAW CONCEPTS
  # 
  # # make container for displaying hover text for column headings
  # RawConceptsHovertext <- htmltools::withTags(table(
  #   class = "display",
  #   thead(
  #     tr(
  #       th("", title = "labels.rowNr"),
  #       th(i18n$t("columns.Concept"), title = i18n$t("labels.nameOfConcept")),
  #       th(i18n$t("columns.when"), title = i18n$t("labels.dateAndLocalTime")),
  #       th(i18n$t("columns.Agent"), title = i18n$t("labels.nameObservingAgent")),
  #       th(i18n$t("columns.observationId"), title = i18n$t("labels.idNumber")),
  #       th(i18n$t("columns.ObservationType"), title = i18n$t("labels.catOfObs")),
  #       th(i18n$t("columns.latitude"), title = i18n$t("labels.WGS84Lat")),
  #       th(i18n$t("columns.longitude"), title = i18n$t("labels.WGS84Lon"))
  #     )
  #   )
  # ))
  # 
  # # Raw concept table
  # output$tableRawConcepts <- DT::renderDataTable({
  #   message("Rendering raw concept table")
  #   # req(session$userData$processed_obsdata)
  # 
  #   session$userData$processed_obsdata() %...>% {
  #     df <- .
  # 
  #     # convert POSIXct to character that includes timezone, for datatable
  #     df$when <- format(df$when, format = "%Y-%m-%dT%H:%M:%S%z") # %z shows as +0100 etc.
  # 
  #     df <- df %>%
  #       select(conceptLabel, when, agentName, observationId, observationType, lat, lon)
  #     # colnr <- which(names(df) == "when") # need for formatDate/toLocaleString
  # 
  #     DT::datatable(df, container = RawConceptsHovertext,
  #                   extensions = "Buttons",
  #                   options = list(
  #                     language = list(url = paste0('//cdn.datatables.net/plug-ins/1.10.11/i18n/',session$userData$lang_long,'.json')),
  #                     paging = TRUE,
  #                     searching = TRUE,
  #                     fixedColumns = TRUE,
  #                     autoWidth = TRUE,
  #                     ordering = TRUE,
  #                     dom = 'frtip<"sep">B', #'<f<t>ip>', #"ftripB", #'<"sep">frtipB', # dom =
  #                     pageLength = 999,
  #                     buttons = list(
  #                       list(
  #                         extend = "copy",
  #                         text = i18n$t("commands.copy")
  #                       ),
  #                       list(
  #                         extend = "csv",
  #                         text = i18n$t("commands.csv")
  #                       ))
  #                   )
  #     ) # %>% DT::formatDate(colnr, "toLocaleString") does not include timezone in locale string
  #   }
  # }) # end output tableRawConcepts
  # 
  # 
  # ### TAB RAW OBSERVATIONS
  # 
  # # Raw observation table
  # output$tableRawObservations <- DT::renderDataTable({
  #   message("Rendering raw observation table")
  # 
  #   session$userData$exportData_withColnames () %...>% {
  #     df <- .
  # 
  #     # colnr <- which(names(df) == i18n$t("columns.timestamp")) # need for formatDate/toLocaleString
  #     show_DT_table(df, session$userData$lang_long, i18n$t("commands.copy"), i18n$t("commands.csv"))
  #     # %>% DT::formatDate(colnr, "toLocaleString") does not include timezone in locale string
  # 
  #   }
  # }) # end output tableRawObservations
  
  

    
}
  
