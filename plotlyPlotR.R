## Activity patterns
## Author: Hanna Fricke
## Date: 11-03-2025 
## Description: The goal is to create a heatmap that allows users to identify activity patterns of e.g. animal species over time (time of day, month, season)
## To-Dos
## [] User time period - dynamic
## [] User label for counts - dynamic
## [] season based on user input - dynamic
## [] name dataframes better -- obs_df is good for now but not really general (might be sounds)
## [] include some form of filter so that you really only get animals
## [] time binning we need to improve and actually make 1 h bins. --> decide if you just want to make seperate new columns for that or
## [] x-axis range heatmap needs to be different depending on selected period
## [] Include date range input into function for season
## Set up libraries
library(pacman)
p_load(ggplot2, plotly, dplyr, tidyr, sensingcluesr) # makes loading and installing easier


## import data from SC platform - chose Demo group africa
cookie <- login_cluey("userid", "password") # insert login 

groups <- get_groups(cookie, 
                     from = "1900-01-01", # set so that dates include everything
                     to = "2999-12-31",
                     url = "https://focus.sensingclues.org/") # to get the names of the groups (REMOVE later)

df <- get_observations(
  cookie,
  from = as.Date("2024-01-01"), # data was available from 2024 , adapt later to long e.g. from 1900-01-01
  to = Sys.Date(),
  group = 'focus-project-1234' # demo group Africa
) 


## Inspect demo data --> REMOVE later
df_copy<- df %>% mutate_if(is.character, as.factor) # check what values are in the dataframe
# Get the levels of each factor column
factor_levels <- lapply(df_copy[, sapply(df_copy, is.factor)], levels)
obs_df <- subset(df,description  == "Observation animal")
dim(obs_df) == dim(subset(df, observationType == "animal")) # cross check if you really got all the animal data, seems like it
obs_df$when <- as.POSIXct(obs_df$when, format = "%Y-%m-%dT%H:%M:%S") # proper format for later

## ---------------------------------------------------------------------------------------------------------------------------------------
## Make first graph 

heatmapwithbar <- function(time_input) {
  time_input <- "monthly" # test
  # Define the time format mapping
  time_format_mapping <- list(
    hourly = list(time_format = "T%H", freq = "hour"),
    daily = list(time_format = "%Y-%m-%d", freq = "day"),
    monthly = list(time_format = "%Y-%m", freq = "month")
  ) # here include all time periods that we want --> season I kept out of it for now cause that needs to be specified by used
  
  # Create column for later aggregation based on user selected time period 
  obs_df <- obs_df %>% mutate(Period = format(when, time_format_mapping[[time_input]]$time_format)) # period definition based on user input (month, day, etc.)
    
  # Group data by Period and Label (for now)
  bar_data <- obs_df %>%
    group_by(conceptLabel) %>% # Good category for grouping?
    summarise(Counts = n(), .groups = 'drop')
  
  # Order species according to frequency of detection for the bar chart
  ordered_species <- bar_data %>%
    arrange(Counts) %>%
    pull(conceptLabel)
  
  # Set the factor levels for visualization in the heatmap
  heatmap_data <- obs_df %>%
    group_by(conceptLabel, Period) %>%
    summarise(Counts = n(), .groups = 'drop')
  
  # Ensure all unique species are included in factor levels
  heatmap_data$conceptLabel <- factor(heatmap_data$conceptLabel, levels = unique(c(ordered_species, heatmap_data$conceptLabel)))
 
   # Bar graph
  bar_chart <- plot_ly(
    bar_data,
    x = ~ Counts,
    y = ~ conceptLabel, # needs to be adapted
    type = 'bar',
    orientation = 'h',
    marker = list(
      color = 'rgba(50, 171, 96, 0.6)',
      line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)
    )
  ) %>%
    layout(
      title = 'Total Counts per Species and time period',
      xaxis = list(title = 'Counts'),
      yaxis = list(title = 'Species', categoryorder = "total ascending")
    )

  
  heatmap <- plot_ly(
    data = heatmap_data,
    x = ~ Period,
    y = ~ conceptLabel,
    z = ~ Counts,
    type = 'heatmap',
    colorscale = 'Greens'
  ) %>%
    layout(
      title = 'Counts per Species',
      xaxis = list(title = 'Time Period'),
      yaxis = list(title = 'Species')
    )
  
  combined_plot <- subplot(bar_chart, heatmap, nrows = 1, margin = 0.05) %>%
    layout(title = 'Activity pattern')

  combined_plot
}

# example use
heatmapwithbar("monthly") # USER INPUT
