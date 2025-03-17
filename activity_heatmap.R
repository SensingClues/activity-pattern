## Activity patterns
## Author: Hanna Fricke
## Date: 11-03-2025 
## Description: The goal is to create a heatmap that allows users to identify activity patterns of e.g. animal species over time (time of day, month, season)
## To-Dos
## [x] User time period - dynamic
## [x] User label for counts - dynamic
## [x] season based on user input - dynamic
## [x] name dataframes better -- obs_df is good for now but not really general (might be sounds)--> now data
## [] include some form of filter so that you really only get animals
## [x] time binning we need to improve and actually make 1 h bins. --> decide if you just want to make seperate new columns for that or
## [x] x-axis range heatmap needs to be different depending on selected period
## [] Include date range input for data
## [x] x-axis should display either 24h range or 12 months
## [x] make sure that user season cannot be paired with month or hour parameters
## [] See how you can adapt the season parameter that it can be an input that you provide as a user (e.g. a range slider)
## [x] make sure heatmap colour is also filled if factor levels are not in df
## [x] Include button that allows switching views from season to year to month
## [x] Include selection for time period of data
## [x] make sure that only one year is diplayed per view --> tested only on 2024
## [] Include the option for different aggregates--> total observations of average observation time of the day depending on season and month
## [] Include button to switch between sensor and cameratrap data
## [] x- axis for hourly view is in steps of 2 atm --> make 1
## [] include normalised view 0-1 
## [] use Shiny to read in data
## [] Include Ontology ID as ID not concept label
## [] Include box that allows to insert method of observation
## [] Visualise numbers in the bar graph and heatmap
## [] Include server side of methods

## Set up libraries
library(pacman)
p_load(ggplot2, plotly, dplyr, tidyr, quantmod, sensingcluesr) # makes loading and installing easier Lubridate needed?

library(tidyr)
## import data from SC platform - chose Demo group africa
cookie <- login_cluey("XXXX", "XXXX") # insert login 

groups <- get_groups(cookie, 
                     from = "1900-01-01", # set so that dates include everything
                     to = "2999-12-31",
                     url = "https://focus.sensingclues.org/") # to get the names of the groups (REMOVE later)

df <- get_observations(
  cookie,
  from = as.Date("2023-01-01"), # data was available from 2024 , adapt later to long e.g. from 1900-01-01
  to = Sys.Date(),
  group = 'focus-group-123' # demo group Africa
) 


## Inspect demo data --> REMOVE later
df_copy<- df %>% mutate_if(is.character, as.factor) # check what values are in the dataframe
# Get the levels of each factor column
factor_levels <- lapply(df_copy[, sapply(df_copy, is.factor)], levels)
obs_df <- subset(df,description  == "Observation animal")
dim(obs_df) == dim(subset(df, observationType == "animal")) # cross check if you really got all the animal data, seems like it
obs_df$when <- as.POSIXct(obs_df$when, format = "%Y-%m-%dT%H:%M:%S") # proper format for later

## Note to myself: I need to filter out the properties from the observation from the animal names

write.csv(obs_df, file ="C:/Users/hanna/Documents/africa_demo.csv")
## ---------------------------------------------------------------------------------------------------------------------------------------
## Make first Shiny version
# UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_year", "Select Year:", choices = c("All", unique(format(obs_df$when, "%Y")))),  # choices from all years in data
      radioButtons("time_input", "Select time period:",  # period one wants to look at
                   choices = c("Hourly" = "hourly", "Monthly" = "monthly", "Seasonal" = "season")),
      numericInput("topX", "Number of rows to display:", value = 10, min = 1, step = 1),  # User input for top X
      radioButtons("method", "Select method of observation:",  # period one wants to look at
                   choices = c("Cameratrap" = "cameratrap", "Animal sighting" = "animal sighting", "Other" = "other")),
      conditionalPanel( # only display if input is season
        condition = "input.time_input == 'season'", 
        numericInput("num_seasons", "Number of Seasons:", value = 1, min = 1), # select how many seasons you expect
        uiOutput("season_inputs"), # placeholder that reacts to server side
      )
    ),
    conditionalPanel( # only display if input is season
      condition = "input.method == 'other'", 
      numericInput("num_methods", "Number of methods:", value = 1, min = 1), # select how many seasons you expect
      uiOutput("Methods"), # placeholder that reacts to server side --> need to include server side still
    )
  ),
    mainPanel(
      plotlyOutput("combined_plot")
    )
  )


server <- function(input, output, session) {
  
  # Generate input fields per season dynamically based on number of seasons user desires
  output$season_inputs <- renderUI({
    req(input$num_seasons) # required input before going further
    lapply(1:input$num_seasons, function(i) {
      textInput(inputId = paste0("season_", i), label = paste("Season", i, "(e.g., '12,1,2' for Dec-Jan-Feb):"), value = "") 
    }) # loop through number of seasons and add suffix (e.g. 2 seasons = season_1, season_2)
  })
  
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
  
  #  Filter data based on year
  filtered_data <- reactive({
    if (input$selected_year == "All") {
      obs_df
    } else {
      obs_df %>% filter(format(when, "%Y") == input$selected_year)
    }
  })
  
  # Prepare data for plotting --> in reactive format
  plot_data <- reactive({
    data <- filtered_data() 
    time_input <- input$time_input 
    seasons <- user_defined_seasons() 
    
    data <- data %>%
      mutate(Period = case_when(
        time_input == "hourly" ~ format(when, "%H"),
        time_input == "monthly" ~ format(when, "%m"),
        time_input == "season" ~ purrr::map_chr(format(when, "%m"), function(month) {
          season <- names(seasons)[sapply(seasons, function(s) month %in% s)] 
          if (length(season) > 0) season else NA 
        }),
        TRUE ~ NA_character_
      ))
    
    if (time_input == "hourly") {
      data$Period <- factor(data$Period, levels = sprintf("%02d", 0:23), labels = paste0(sprintf("%02d", 0:23), "h"))
    } else if (time_input == "monthly") {
      data$Period <- factor(data$Period, levels = sprintf("%02d", 1:12), labels = month.abb)
    } else if (time_input == "season") {
      data$Period <- factor(data$Period, levels = names(seasons))
    }
    
    data
  })
  
  # Render combined plot
  output$combined_plot <- renderPlotly({
    data <- plot_data() 
    
    # Group data by Period and Label
    bar_data <- data %>%
      group_by(conceptLabel) %>%
      summarise(Counts = n(), .groups = 'drop')
    
    
    # Apply top X filter
    topX <- input$topX
    if (topX > 0) {
      bar_data <- bar_data %>% 
        top_n(topX, Counts) %>% 
        arrange(desc(Counts))
    }
    
    # Order species according to frequency of detection for the bar chart
    ordered_species <- bar_data %>%
      arrange(Counts) %>% 
      pull(conceptLabel) 
    
    # Heatmap Data Preparation
    heatmap_data <- data %>%
      group_by(conceptLabel, Period) %>%
      summarise(Counts = n(), .groups = 'drop')
    
    # Create a full set of all combinations of conceptLabel and Period
    full_periods <- expand.grid(
      conceptLabel = unique(heatmap_data$conceptLabel),
      Period = levels(data$Period)
    )
    
    # Left join the full combination set with the observed data to ensure all combinations
    heatmap_data_complete <- full_periods %>%
      left_join(heatmap_data, by = c("conceptLabel", "Period")) %>%
      mutate(Counts = ifelse(is.na(Counts), 0, Counts))  
    
    # Apply top X filter to heatmap data
    if (topX > 0) {
      heatmap_data_complete <- heatmap_data_complete %>%
        filter(conceptLabel %in% bar_data$conceptLabel)
    }
    ## ORDERED SPECIES NEEDS TO BE INCLUDED HERE! FILTERED
    
    heatmap_data_complete$conceptLabel <- factor(heatmap_data_complete$conceptLabel, levels = unique(c(
      ordered_species, heatmap_data_complete$conceptLabel
    )))
    
    # Bar graph with summarised counts
    bar_chart <- plot_ly(
      bar_data,
      x = ~ Counts,
      y = ~ conceptLabel,
      type = 'bar',
      orientation = 'h',
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
    
    # Now use heatmap_data_complete for your heatmap plot
    heatmap <- plot_ly(
      data = heatmap_data_complete,
      x = ~ Period,
      y = ~ conceptLabel,
      z = ~ Counts,
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
    
    subplot(bar_chart, heatmap, nrows = 1, margin = 0.05) %>%
      layout(title = 'Activity Pattern')
  })
}

# Run the application
shinyApp(ui = ui, server = server)