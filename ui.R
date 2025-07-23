# ui.R

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
  # i18n <- Translator$new(translation_json_path = "https://focus.sensingclues.org/api/labels/list") # Prod
  i18n <- Translator$new(translation_json_path = "https://focus.test.sensingclues.org/api/labels/list")
}, error = function(e) {
  message("No labels available online, using local")
})
if (!exists("i18n")) {
  i18n <- Translator$new(translation_json_path = "translations.json")
}
i18n$set_translation_language("en")

# JS for browser language
js_lang <- "var language = window.navigator.userLanguage || window.navigator.language;
             Shiny.onInputChange('browser_language', language);"

ui <- fluidPage(
  useShinyjs(),
  shiny.i18n::usei18n(i18n),
  extendShinyjs(text = js_lang, functions = c()),
  
  # Get timezone from browser
  tags$script(HTML("
    $(document).on('shiny:sessioninitialized', function() {
      var tz = Intl.DateTimeFormat().resolvedOptions().timeZone;
      Shiny.onInputChange('user_timezone', tz);
    });
  ")),
  
  # Load custom stylesheet
  includeCSS("www/style.css"),
  
  ## HEADER
  div(class = "header",
      div(
        tags$a(href = "https://sensingclues.org", target = "_blank",
               class = "logo", img(src = "logo_white.png"))
      ),
      div(class = "title", "ACTIVITY PATTERN"),
      div(style = "min-width:150px; text-align:right;",
          uiOutput("userstatus")
      )
  ),
  
  ## CONTENT WRAPPER (overlaps header)
  div(class = "content-wrapper",
      sidebarLayout(
        
        sidebarPanel(
          width = 3,
          #HTML("<br>"),
          
          # Collapsible About Box
          tags$head(
            tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
          ),
          tags$details(
            id = "aboutCollapse", class = "collapsible-section",
            tags$summary(class = "collapsible-header",
                         HTML('<span>About</span><i class="material-icons expand-icon">expand_more</i>')
            ),
            p("With this app you can explore pattern in animal observation data. Use the matrix to reveal activity trends by hour or month, view total counts per species or download the underlying datasets."),
            tags$a("Learn more", href = "https://www.sensingclues.org/about-activity-pattern",
                   class = "readmore", target = "_blank")
          ),
          tags$script(HTML("
          document.addEventListener('DOMContentLoaded', function() {
            var el = document.getElementById('aboutCollapse');
            if (el) {
              el.querySelector('summary').addEventListener('click', function() {
                setTimeout(function() {
                  var icon = el.querySelector('.expand-icon');
                  icon.style.transform = el.hasAttribute('open') ? 'rotate(180deg)' : 'rotate(0deg)';
                }, 100);
              });
            }
          });
        ")),
          
          # Custom button styles & spinner message
          tags$head(tags$style(HTML("
          #GetData, #downloadData, #login { background-color:#FB8C00; color:white; font-size:100%; }
          #message_more_dates { color: red; font-size: 20px; font-style: italic; }
        "))),
          
          # Filters
          div(class = "filter-section time-period-box",
              br(),
              h4("Time Period"),
              div(class = "date-range-input-container",
                  disabled(dateRangeInput("DateRange", i18n$t("labels.selectPeriod")))
              )
          ),
          br(),
          div(style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
              add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px")
          ),
          div(class = "filter-section data-sources-box",
              h4("Data Sources"),
              disabled(div(
                id = "GroupListDiv", class = "choosechannel",
                pickerInput("GroupList", label = i18n$t("labels.selectGroup"),
                            choices = list(), multiple = TRUE,
                            options = pickerOptions(
                              actionsBox = TRUE,
                              noneSelectedText = '',
                              selectAllText = i18n$t("labels.selectAll"),
                              deselectAllText = i18n$t("labels.deselectAll")
                            ))
              ))
          ),
          br(),
          div(class = "filter-section concepts-box",
              h4("Concepts"),
              p("Select concepts (one or more)"),
              shinyTree("conceptTree", checkbox = TRUE, theme = "proton")
          ),
          br(),
          disabled(actionButton("GetData", i18n$t("commands.getdata"))),
          br()
        ),
        
        mainPanel(
          width = 9,
          tags$head(tags$style(HTML(".sep { width:20px; height:1px; float:left; }"))),
          tabsetPanel(
            type = "tabs",
            
            tabPanel(i18n$t("Activity Pattern"),
                     fluidPage(
                       fluidRow(
                         column(12,
                                div(style = "display:flex;align-items:center;gap:20px;margin-top:10px;",
                                    div(style = "width:150px;",
                                        selectInput("time_input", i18n$t("Time interval"),
                                                    choices = list("Hourly"="hourly","Monthly"="monthly"),
                                                    selected = "hourly", width = "100%")),
                                    # conditionalPanel(
                                    #   "input.time_input=='season'",
                                    #   div(style="display:flex;align-items:center;gap:20px;",
                                    #       div(style="width:150px;",
                                    #           numericInput("num_seasons", "# Seasons:", value=1, min=1, width="100%"),
                                    #           bsTooltip("num_seasons","Select number of seasons","right")),
                                    #       div(style="display:flex;flex-wrap:wrap;gap:10px;", uiOutput("season_inputs"))
                                    #   )
                                    # )
                                )
                         )
                       ),
                       fluidRow(
                         column(12,
                                div(style="display:flex;align-items:center;gap:20px;margin-top:15px;",
                                    div(style="width:150px;",
                                        numericInput("topX","Top rows:",value=5,min=1,max=20,step=1,width="100%")),
                                    div(style="display:flex;align-items:flex-end;height:58px;",
                                        radioButtons("agg_method", NULL, choices=list("Counts"="counts","Percentage"="percentage"), inline=TRUE))
                                )
                         )
                       ),
                       fluidRow(column(12, plotlyOutput("combined_plot"))),
                       fluidRow(column(12,
                                       div(style="margin-top:20px;",
                                           downloadButton("download_plotly","Download activity pattern plot (.html)"),
                                           downloadButton("download_csv","Download Data (.csv)"))
                       ))
                     )
            ),
            
            tabPanel(i18n$t("labels.rawData"),
                     br(),
                     column(2, br(), br(), downloadButton("downloadData", i18n$t("commands.download"))),
                     fluidRow(column(12, DT::dataTableOutput("tableRawObservations"))),
                     div(style="position:fixed;top:45%;left:60%;transform:translate(-50%,-50%);",
                         add_busy_spinner(spin="fading-circle",width="100px",height="100px"))
            )
            
          )
        )
      )
  )
)
