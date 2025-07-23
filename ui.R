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

# js code to get the browser language - Corrected escaping
js_lang <- "var language =  window.navigator.userLanguage || window.navigator.language;
              Shiny.onInputChange('browser_language', language);
              console.log(language);"

ui <- fluidPage(
  useShinyjs(),
  shiny.i18n::usei18n(i18n),
  extendShinyjs(text = js_lang, functions = c()),
  
  # Get timezone from browser - Corrected escaping

  tags$script(
    "$(document).on('shiny:sessioninitialized', function(event) {
                                        var n = Intl.DateTimeFormat().resolvedOptions().timeZone;
                                        Shiny.onInputChange('user_timezone', n);});"
  ),
  
  # Load custom stylesheet
  includeCSS("www/style.css"),
  shiny::tagList(
    div(
      class = "header",
      
      # combine the two logos, next to each other
      div(
        # logo SC
        tags$a(
          href   = "https://sensingclues.org",
          target = "_blank",
          class = "logo", img(src = "logo_white.png"))
      ),
      
      # titel
      div(
        class = "title",
        "ACTIVITY PATTERN",
        style = "font-size: 18px;"
      ),
      # Right side: user status
      div(
        style = "min-width: 150px; text-align: right;",
        uiOutput("userstatus")
      )
    )
  ),
  div(class = "content",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "height: 90vh; overflow-y: auto;",
      # --- Collapsible About Box ---
      tags$head(
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons"),
        tags$style(HTML("
    .collapsible-section summary::-webkit-details-marker {
      display: none;
    }
    .readmore {
      font-weight: normal;
      font-size: inherit;
      color: #004d40;
      text-decoration: underline;
    }
    .collapsible-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      cursor: pointer;
      font-size: 16px;
      font-weight: bold;
      margin-bottom: 5px;
    }
    .collapsible-header .expand-icon {
      transition: transform 0.3s ease;
      font-size: 24px;
      color: #555;
    }
    details[open] .expand-icon {
      transform: rotate(180deg);
    }
  "))
      ),
      
      tags$details(
        id = "aboutCollapse",
        class = "collapsible-section",
        tags$summary(
          class = "collapsible-header",
          HTML('<span>About</span><i class="material-icons expand-icon">expand_more</i>')
        ),
        p("With this app you can explore pattern in animal observation data. Use the matrix to reveal activity trends by hour or month, view total counts per species or download the underlying datasets."),
        tags$a(
          "Learn more",
          href = "https://www.sensingclues.org/about-activity-pattern",
          class = "readmore",
          target = "_blank"
        )
      ),
      
      tags$script(HTML("
  document.addEventListener('DOMContentLoaded', function() {
    var el = document.getElementById('aboutCollapse');
    if (el) {
      var summary = el.querySelector('summary');
      summary.addEventListener('click', function(e) {
        setTimeout(function() {
          var icon = summary.querySelector('.expand-icon');
          if (el.hasAttribute('open')) {
            icon.style.transform = 'rotate(180deg)';
          } else {
            icon.style.transform = 'rotate(0deg)';
          }
        }, 100);
      });
    }
  });
")),
      # --- End Collapsible About Box ---
      
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
      br(),
      
      # --- Filter Sections ---
      div(class = "filter-section time-period-box",
          h4("Time Period"),
          # Added a container div for easier styling of the date range input width
          div(class = "date-range-input-container",
              disabled(dateRangeInput("DateRange", i18n$t("labels.selectPeriod")))
          )
      ),
      br(),
      div(
        style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
        add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px")
      ),
      
      div(class = "filter-section data-sources-box",
          h4("Data Sources"),
          disabled(div(
            class = "choosechannel",
            id = "GroupListDiv",
            pickerInput(
              inputId = "GroupList",
              label = i18n$t("labels.selectGroup"), # This label might be redundant with the H4 heading, consider removing if needed
              choices = list(),
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                noneSelectedText = '',
                selectAllText = i18n$t("labels.selectAll"),
                deselectAllText = i18n$t("labels.deselectAll")
              )
            )
          ))
      ),
      br(),
      
      div(class = "filter-section concepts-box",
          h4("Concepts"),
          p("Select concepts (one or more)"),
          shinyTree("conceptTree", checkbox = TRUE, theme = "proton")
      ),
      br(),
      # --- End Filter Sections ---
      
      disabled(actionButton(
        "GetData", i18n$t("commands.getdata"), icon = NULL
      )),
      br()
    ),
    
    mainPanel(
      width = 9,
      style = "height: 90%; overflow-y: auto;",
      tags$head(tags$style(
        # Corrected escaping for the CSS content within HTML()
        # HTML(".sep {
        #   width: 20px;
        #   height: 1px;
        #   float: left;
        #   }")
      )),
      tabsetPanel(
        type = "tabs",
        tabPanel(i18n$t("Activity Pattern"),
                 fluidPage(
                   
                   # === Time Interval Row ===
                   fluidRow(
                     column(
                       12,
                       div(
                         style = "display: flex; align-items: center; gap: 20px; margin-top: 10px;",
                         
                         # Time Interval Box
                         div(
                           style = "width: 150px;",
                           selectInput(
                             inputId = "time_input",
                             label = i18n$t("Time interval"),
                             choices = list(
                               "Hourly" = "hourly",
                               "Monthly" = "monthly"
  #                             "Seasonal" = "season"
                             ),
                             selected = "hourly",
                             width = "100%"
                           )
                         ),
                         
                         # Conditional Season Controls
                         conditionalPanel(
                           condition = "input.time_input == 'season'",
                           div(
                             style = "display: flex; align-items: center; gap: 20px;",
                             div(
                               style = "width: 150px;",
                               numericInput(
                                 "num_seasons",
                                 "# Seasons:",
                                 value = 1,
                                 min = 1,
                                 width = "100%"
                               ),
                               bsTooltip(
                                 "num_seasons",
                                 "Select the number of seasons for analysis. Input the calendar month number to specify season. (1 = January, 2 = February, etc.)",
                                 placement = "right",
                                 options = list(container = "body")
                               )
                             ),
                             div(
                               style = "display: flex; flex-wrap: wrap; gap: 10px;",
                               uiOutput("season_inputs")
                             )
                           )
                         )
                       )
                     )
                   ),
                   
                   # === TopX and Aggregation Method Row ===
                   fluidRow(
                     column(
                       12,
                       div(
                         style = "display: flex; align-items: center; gap: 20px; margin-top: 15px;",
                         
                         # Top X Filter Box (match width to Time Interval box)
                         div(
                           style = "width: 150px;",
                           numericInput(
                             inputId = "topX",
                             label = "Top rows:",
                             value = 5,
                             min = 1,
                             max = 20,
                             step = 1,
                             width = "100%"
                           )
                         ),
               
                         # Aligned Radio Buttons (inline, vertically centered)
                         div(
                           style = "display: flex; align-items: flex-end; height: 58px;",  # Adjust height to match input height
                           radioButtons(
                             inputId = "agg_method",
                             label = NULL,
                             choices = list("Counts" = "counts", "Percentage" = "percentage"),
                             selected = "counts",
                             inline = TRUE
                           )
                         )
                       )
                     )
                   ),
                   
                   # === Plot Row ===
                   fluidRow(
                     column(
                       12,
                       plotlyOutput("combined_plot")
                     )
                   ),
                   
                   # === Download Button Row ===
                   fluidRow(
                     column(
                       12,
                       div(
                         style = "margin-top: 20px;",
                         downloadButton("download_plotly", "Download activity pattern plot (.html)"),
                         downloadButton("download_csv", "Download Data (.csv)")
                       )
                     )
                   )
                 )
        ),
        # tabPanel(
        #   i18n$t("labels.rawConceptsTab"),
        #   fluidRow(column(
        #     12, DT::dataTableOutput("tableRawConcepts")
        #   )),
        #   div(
        #     style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
        #     add_busy_spinner(
        #       spin = "fading-circle",
        #       width = "100px",
        #       height = "100px"
        #     )
        #   )
        # ),
        # 
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
)