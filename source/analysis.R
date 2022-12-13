library(tidyverse)
library(ggplot2)
state_codes <- read.csv("~/Documents/info201/data/state_names_and_codes.csv", stringsAsFactors = FALSE)
incarceration_trends <- read.csv("~/Documents/info201/data/incarceration-trends/incarceration_trends.csv", stringsAsFactors = FALSE)
# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Performs summary statistics
summary_info <- list()
summary_info$average_black_2018 <- incarceration_trends %>%
  filter(year == 2018) %>%
  summarize(mean_pop = mean(as.integer(black_jail_pop), na.rm = TRUE)) %>%
  pull(mean_pop)

total_pop_1970 <- incarceration_trends %>%
  filter(county_name == "Los Angeles County") %>%
  filter(year == 1970) %>%
  pull(total_jail_pop)

total_pop_2018 <- incarceration_trends %>%
  filter(county_name == "Los Angeles County") %>%
  filter(year == 2018) %>%
  pull(total_jail_pop)

summary_info$total_pop_change <- incarceration_trends %>%
  summarize(total_pop_diff = total_pop_2018 - total_pop_1970) %>%
  pull(total_pop_diff)

summary_info$max_ice <- incarceration_trends %>%
  filter(year == 2018) %>%
  filter(total_jail_from_ice == max(total_jail_from_ice, na.rm = T)) %>%
  pull(unique(county_name))
  
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# Sets data up for chart, selecting values
get_year_jail_pop <- function() {
  subset(incarceration_trends, select=c(year, total_jail_pop)) %>%
  return()   
}

# Plots bar graph
plot_jail_pop_for_us <- function(){
  df <- get_year_jail_pop()
  chart <- ggplot(df, aes(x = year, y = total_jail_pop)) + 
    geom_bar(stat="identity")+
    labs(title = paste("Increase of Jail Population in U.S. (1970-2018)"),
         x = "Year",
         y = "Total Jail Population",
         caption = "This chart shows a bar chart of the how the total jail 
         population has grown in all of the U.S. from 1970 to 2018.")
  return(chart)
}

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#
# Sets up data for chart
get_jail_pop_by_states <- function(states){
  df <- subset(incarceration_trends, state %in% states) %>%
    select(total_jail_pop, state, year) %>%
    group_by(state, year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = T))
  return(df)
}

# Plots chart
plot_jail_pop_by_states <- function(states){
  df <- get_jail_pop_by_states(states)
  ggplot(df, aes(x = year, y = total_jail_pop, color = state)) +
    geom_line()+
    labs(title = paste("Growth of Prison Population by State "),
         x = "Year",
         y = "Total Jail Population",
         caption = "This chart displays the growth of the jail population 
         based on the given states from 1970 to 2018.")
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
#----------------------------------------------------------------------------#
# Prepares data 
grouped_by_state <- function(states){
  df <- subset(incarceration_trends, state %in% states) %>%
    select(white_jail_pop, state, year, white_pop_15to64, black_jail_pop, black_pop_15to64) %>%
    group_by(state, year) %>%
    na.omit() %>%
    summarise(across(c(white_jail_pop, white_pop_15to64, black_jail_pop, black_pop_15to64), list(sum = sum)))
  return(df)
}

# Creates proportion columns
get_jail_prop <- function(states){
  df <- grouped_by_state(states) %>%
    mutate(white_jail_prop = white_jail_pop_sum / white_pop_15to64_sum) %>%
    mutate(black_jail_prop = black_jail_pop_sum / black_pop_15to64_sum)
  return(df)
}

# Plots proportion of White people in jail
plot_white_jail_prop <- function(states){
  df <- get_jail_prop(states)
  ggplot(df, aes(x = year, y = white_jail_prop)) +
    geom_line()+
    labs(title = paste("Growth of White Prison Population Proportion in ", states),
         x = "Year",
         y = "Proportion of White People in Jail",
         caption = "This chart displays the how the proportion of all the white people that are in 
         jail has grown throughout the years in California.")
}


# Plots proportion of Black people in jail
plot_black_jail_prop <- function(states){
  df <- get_jail_prop(states)
  ggplot(df, aes(x = year, y = black_jail_prop)) +
    geom_line()+
    labs(title = paste("Growth of Black Prison Population Proportion in ", states),
         x = "Year",
         y = "Proportion of Black People in Jail",
         caption = "This chart displays the how the proportion of all the black people that are in 
         jail has grown throughout the years in California.")

}## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
#----------------------------------------------------------------------------#
# Prepares data set for joining and mapping
grouped_by_2018 <- function(){
  df <- incarceration_trends %>%
    select(state, year, black_jail_pop, black_pop_15to64) %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    na.omit() %>%
    summarise(across(c(black_jail_pop, black_pop_15to64), list(sum = sum)))
  return(df)
}

# Prepares data set for joining, creates proportion column
get_jail_prop_2018 <- function(){
  df <- grouped_by_2018() %>%
    rename(Code = state) %>%
    mutate(black_jail_prop = black_jail_pop_sum / black_pop_15to64_sum)
  return(df)
}

# Joins get_jail_prop to shape file
state_code_join <- function(){
  df <- state_codes %>%
    mutate(State = tolower(State)) %>%
    left_join(get_jail_prop_2018(), by="Code")
  return(df)
}

# Joins state_code_join to shape file
get_state_shape <- function(){
  state_shape <- map_data("state") %>%
  rename(State = region) %>%
  left_join(state_code_join(), by="State")
  return(state_shape)
}

blank_theme <- function(){
  theme_bw() +
    theme(
      axis.line = element_blank(),        # remove axis lines
      axis.text = element_blank(),        # remove axis labels
      axis.ticks = element_blank(),       # remove axis ticks
      axis.title = element_blank(),       # remove axis titles
      plot.background = element_blank(),  # remove gray background
      panel.grid.major = element_blank(), # remove major grid lines
      panel.grid.minor = element_blank(), # remove minor grid lines
      panel.border = element_blank()      # remove border around plot
    )
}

# Plots the map
plot_state_shape <- function(){
  ggplot(get_state_shape()) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = black_jail_prop),
      color = "white", # show state outlines
      size = .1        # thinly stroked
    ) +
    coord_map() + # use a map-based coordinate system
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(fill = "Proportion of Black People Jailed",
         title = paste("Map of Proportion of Black Population Jailed in the U.S (2018)"),
         caption = "This map displays the proportion of all the black people that are in 
         jail in the U.S, with redder shades meaning a higher proportion.") +
    blank_theme()
}
