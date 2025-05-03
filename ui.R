# Author: Hanna Fricke
# Description: User interface for activity app. 
# TO-DO :
# [] DONT HARDCODE METHOD FUTURE BUT BASE IT ON DATA

library(shiny)
library(shiny.i18n) # for multilanguage
library(shinyjs)
library(shinyTree)
library(shinybusy)
library(shinyBS)
library(shinyWidgets)
library(plotly)

# multi language


tryCatch({
  # try to get online version
  # i18n <- Translator$new(translation_json_path = "https://focus.sensingclues.org/api/labels/list") # Production Environment
  i18n <- Translator$new(translation_json_path = "https://focus.test.sensingclues.org/api/labels/list") # Test Environment
}, error = function(e) {
  message("No labels available online, we will use the old ones from disk.")
})

if (!exists("i18n")) {
  # use the stored version
  i18n <- Translator$new(translation_json_path = "translations.json")
}
i18n$set_translation_language("en")

js_lang <- "var language =  window.navigator.userLanguage || window.navigator.language;
              Shiny.onInputChange('browser_language', language);
              console.log(language);"

ui <- fluidPage(
  useShinyjs(),
  shiny.i18n::usei18n(i18n),
  extendShinyjs(text = js_lang, functions = c()),
  
  # Get timezone from browser
  tags$script(
    "$(document).on('shiny:sessioninitialized', function(event) {
                                        var n = Intl.DateTimeFormat().resolvedOptions().timeZone;
                                        Shiny.onInputChange('user_timezone', n);});"
  ),
  
  # Load custom stylesheet
  includeCSS("www/style.css"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      HTML(
        paste0(
          "<br>",
          "<a href='https://www.sensingclues.org/portal/'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logo_white.png' width = '150'></a>",
          "<br>"
        )
      ),
      
      # Custom button styles
      tags$head(
        tags$style(
          "#GetData{background-color:#FB8C00; color:white; font-size:100%}"
        ),
        tags$style(
          "#login{background-color:#FB8C00; color:white; font-size:100%}"
        ),
        tags$style(
          "#message_more_dates{color: red; font-size: 20px; font-style: italic}"
        ),
        tags$style(
          "#downloadData{background-color:#FB8C00; color:white; font-size:100%}"
        ),
      ),
      
      h3(i18n$t("labels.obsReport")),
      uiOutput("userstatus"),
      br(),
      br(),
      disabled(dateRangeInput("DateRange", i18n$t("labels.selectPeriod"))), 
      br(),
      disabled(div(
        class = "choosechannel",
        id = "GroupListDiv",
        pickerInput(
          inputId = "GroupList",
          label = i18n$t("labels.selectGroup"),
          choices = list(),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            noneSelectedText = '',
            selectAllText = i18n$t("labels.selectAll"),
            deselectAllText = i18n$t("labels.deselectAll")
          )
        )
      )),
      br(),
      shinyTree("conceptTree", checkbox = TRUE, theme = "proton"),
      br(),
      disabled(actionButton(
        "GetData", i18n$t("commands.getdata"), icon = NULL
      )),
      br()
    ),
    
    mainPanel(
      width = 9,
      tags$head(tags$style(
        HTML("
          .sep {
          width: 20px;
          height: 1px;
          float: left;
          }
        ")
      )),
      tabsetPanel(
        type = "tabs",
        tabPanel(i18n$t("Activity Pattern"), fluidPage(fluidRow(column(
          12,
          div(
            style = "display: flex; align-items: center; gap: 20px;",
            selectInput(
              inputId = "time_input",
              label = i18n$t("Time interval"),
              choices = list(
                "Hourly" = "hourly",
                "Monthly" = "monthly",
                "Seasonal" = "season"
              ),
              selected = "hourly"  # Default selection
            ),
            selectInput(
              inputId = "agg_method",
              label = i18n$t("Aggregation method"),
              choices = list(
                "Counts" = "counts",
                "Percentage" = "percentage"
              ),
              selected = "Counts"  # Default selection
            ),
            numericInput(
              "topX",
              "Top rows:",
              value = 10,
              min = 1,
              step = 1
            ),
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              conditionalPanel(
                condition = "input.time_input == 'season'",
                div(
                  numericInput(
                    "num_seasons", 
                    "# Seasons:",
                    value = 1,
                    min = 1
                  ),
                  bsTooltip(
                    "num_seasons", 
                    "Select the number of seasons for analysis. Input the calendar month number to specify season. (1 = January, 2 = February, etc.)", 
                    placement = "right", 
                    options = list(container = "body")
                  )
                ),
                uiOutput("season_inputs")  # Now inside the conditionalPanel
              )
            )
          )
        )
        ),
        fluidRow(
          column(12, plotlyOutput("combined_plot"))))), tabPanel(i18n$t("labels.rawConceptsTab"), fluidRow(column(
            12, DT::dataTableOutput("tableRawConcepts")
          )), 
          div(
            style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
            add_busy_spinner(
              spin = "fading-circle",
              width = "100px",
              height = "100px"
            )
          )
          ),
        
        # endpanel
        
        tabPanel(
          i18n$t("labels.rawData"),
          br(),
          column(2, br(), br(), downloadButton(
            "downloadData", i18n$t("commands.download")
          )),
          fluidRow(column(
            12, DT::dataTableOutput("tableRawObservations")
          )),
          div(
            style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
            add_busy_spinner(
              spin = "fading-circle",
              width = "100px",
              height = "100px"
            )
          )
        )
      )
    )
  )
)

