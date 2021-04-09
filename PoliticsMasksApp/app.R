
# load a helper file
source("helpers.R")

# load libaries
library(shiny)
library(readr)
library(stringr)
library(dplyr)
library(maps)
library(mapproj)
library(RCurl)
library(tidyr)
library(tidyjson)


# Loading and Wrangling Data ----



# get mask usage data from NYT (officially attributed to “The New York Times and Dynata”)

# “Estimates from The New York Times, based on roughly 250,000 interviews conducted by 
# Dynata from July 2 to July 14.”
mask_url <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv")
mask_df <- read_csv(mask_url)

# get electoral data from NYT

# First attempt: taking data from API and unnesting .json in R

# vote_url <- getURL("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/president.json")

# R is not as easy for sorting out .json files as Python

# I bailed on trying to figure it out and used a python script
# copied from the following github 
# https://github.com/tonmcg/US_County_Level_Election_Results_08-20/blob/master/2020_US_County_Level_Presidential_Results.ipynb

# the Python script is saved as munging.py
# the output is saved in the data directory 
# as indicated in the following line
vote_df <- read_csv('data/pres_race_2020.csv')

vote_df <- vote_df %>%
  mutate(dem_pct = 100*votes_dem/(votes_dem+votes_gop),
         gop_pct = 100*votes_gop/(votes_dem+votes_gop)
         )

# join the tables together to form the total_df

total_df <- vote_df %>%
  inner_join(mask_df, by = c("geoid" = "COUNTYFP")) %>%
  mutate(never = 100*NEVER,
         rare = 100*RARELY,
         some = 100*SOMETIMES,
         freq = 100*FREQUENTLY,
         always = 100*ALWAYS) %>%
  select(geoid,
         dem_pct,
         gop_pct,
         never,
         rare,
         some,
         freq,
         always)


# Define UI ----
ui <- fluidPage(
  titlePanel("The politics of mask usage"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This app displays two sets of data collected by the NYTimes:",
               br(),
               "1) 2020 Presidential election results (percent Dem/GOP)",
               br(),
               "2) Mask usage (In response to  this question:",
               br(),
               "How often do you wear a mask in public when you expect to be 
               within six feet of another person?",
               br(),
               "(Attribution: The New York Times and Dynata)"),
      
      #selector for political variable
      selectInput("polvar",
                  h3("Choose a political variable to display"),
                  choices = list("Dem vote share",
                                 "GOP vote share"),
                  selected = "GOP vote share"),

      # selector for mask usage variable
      selectInput("maskvar",
                  h3("Choose a mask survey response (will display percent with that response)"),
                  choices = list("Never wear a mask",
                                 "Rarely wear a mask",
                                 "Sometimes wear a mask",
                                 "Frequently wear a mask",
                                 "Always wear a mask"),
                  selected = "Never wear a mask")
      ),
  mainPanel(
    
    fluidRow(
      column(6,plotOutput("vote_map")),
      column(6,plotOutput("mask_map"))
    )
  )
  )
)




# Define server logic ----
server <- function(input, output) {
  output$vote_map <- renderPlot({
    data <- switch(input$polvar, 
                   "Dem vote share" = total_df$dem_pct,
                   "GOP vote share" = total_df$gop_pct)
    
    color <- switch(input$polvar, 
                    "Dem vote share" = "blue",
                    "GOP vote share" = "red")
    
    legend <- switch(input$polvar, 
                     "Dem vote share" = "% Dem vote share",
                     "GOP vote share" = "% GOP vote share")
    
    
    percent_map(data, color, legend)
  })
  
  output$mask_map <- renderPlot({
    data <- switch(input$maskvar,
                   "Never wear a mask" = total_df$never,
                   "Rarely wear a mask" = total_df$rare,
                   "Sometimes wear a mask" = total_df$some,
                   "Frequently wear a mask" = total_df$freq,
                   "Always wear a mask" = total_df$always)
    
    
    color <- switch(input$maskvar,
                    "Never wear a mask" = "lightgreen",
                    "Rarely wear a mask" = "green",
                    "Sometimes wear a mask" = "darkgreen",
                    "Frequently wear a mask" = "purple",
                    "Always wear a mask" = "black")
    
    legend <- switch(input$maskvar,
                     "Never wear a mask" = "% never masking",
                     "Rarely wear a mask" = "% rarely masking",
                     "Sometimes wear a mask" = "% sometimes masking",
                     "Frequently wear a mask" = "% frequently masking",
                     "Always wear a mask" = "% always masking")
    
    percent_map(data, color, legend)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)