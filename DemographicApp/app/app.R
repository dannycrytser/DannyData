
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


# Loading and Wrangling Data ----

# read racial census data in counties. This is from the 2010 US Census
counties <- readRDS("data/counties.rds")
counties <- as_tibble(counties)
counties <- counties %>%
    mutate(population = total.pop)

counties <- counties %>%
    select(-total.pop)

# get covid data from NYT

covid_url <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_df <- read_csv(covid_url)

# get electoral data from NYT

# commented out because I don't know how to mess with geojson

# electoral_url <- getURL("https://int.nyt.com/newsgraphics/elections/map-data/2020/national/precincts-with-results.geojson.gz")

# load big county results from Harvard (https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ)

votes_df <- read_csv("data/countypres_2000-2016.csv")


# load poverty stats
# these are in overly tidy data format, so we'll pivot them out to make it easier to visualize
# also, the names are formatted in an annoying way, so later on when we join everything together
# we will use fips (Federal Information Processing Standard) instead of county_name. 

poverty_data <- read_csv("data/PovertyEstimates.csv")

# We pivot out the overly tidy data so that we can join it more easily. 

poverty_wide <- poverty_data %>%
    pivot_wider(names_from = Attribute,
                values_from = Value) 

# We will use the following function to add 0's in where they were erroneously removed, 
# as in the case of Autauga County, Alabama (which had its FIPS code changed from 
# 01001 to 1001). You could certainly shorten this approach. TODO: Shorten this. 

prepend_zero <- function(number_value){
    char_value <- as.character(number_value)
    if(str_length(char_value) < 5){
        char_value <- paste("0",char_value, sep = "")
    }
    char_value
}

# To use custom functions in dplyr/mutate you have to vectorize them (o/w they only test 
# the triggering condition on the first input --- very annoying!)

prepend_zero <- Vectorize(prepend_zero)

# create the more standard (string-valued) fips column

# we only want a couple columns, one for overall poverty and one for
# childhood poverty (and the fips key for joining)


poverty_df <- poverty_wide %>%
    mutate(fips = prepend_zero(FIPStxt))
    
poverty_final <- poverty_df %>%
    rename(overall_poverty = PCTPOVALL_2019,
           childhood_poverty = PCTPOV017_2019) %>%
    select(fips,
           overall_poverty,
           childhood_poverty)

    
# There are quite a few missing cells, we just impute the median
    
# (A better approach might be to group by state and then impute the 
# statewise median, but we're trying to go quickly.)
    
#     
# poverty_final$overall_poverty <- poverty_final$overall_poverty %>%
#     simple.impute()
# 
# poverty_final$childhood_poverty <- poverty_final$childhood_poverty %>%
#     simple.impute()




# 

votes_df <- votes_df %>% 
    mutate(county_name = str_replace_all(paste(tolower(state), ",",tolower(county), sep=""), "\\.",""))

votes_df <- votes_df %>%
    filter((office == "President") & (year == 2016) & (party == 'democrat' | party == 'republican')) %>%
    select(party, candidatevotes, totalvotes, county_name)


# Final table for votes contains: 
# county_name, total_share, dem_votes, dem_share, gop_votes, gop_share

# We can use a cute tool from the tidyverse called pivot_wider()
# which replaces the old spread() function. 

votes_final <- votes_df %>%
    group_by(county_name) %>%
    mutate(place = row_number(desc(candidatevotes))) %>%
    filter(place <= 2) %>%
    select(county_name, party, candidatevotes, totalvotes) %>%
    mutate(share = 100*candidatevotes/totalvotes) %>%
    select(-totalvotes, -candidatevotes) %>%
    pivot_wider(names_from = party, names_glue = "{party}_{.value}", values_from = share)


# format NYT data to have same county name field as census data

covid_df <- covid_df %>% 
    mutate(county_name = str_replace_all(paste(tolower(state), ",",tolower(county), sep=""), "\\.",""))

# create a covid_df that only has county, date, and case/death values

covid_county <- covid_df %>%
    select(date, cases, deaths, county_name, fips)
    
# we don't need to summarize because the NYT covid data has the most recent
# cumulative values. 

# window functions are easy to use in R

covid_county_grouped <- covid_county %>%
    group_by(county_name)

covid_county_ranked <- covid_county_grouped %>%
    mutate(date_rank = row_number(desc(date)))


# need to check that no counties are repeated. 
# they aren't, this command gives 0 rows)

covid_final <- covid_county_ranked %>%
    filter(date_rank == 1) %>%
    select(county_name, cases, deaths, fips)




# we want to join all the tables together

total_data <- counties %>%
    left_join(covid_final, by = c("name" = "county_name"))  %>%
    left_join(votes_final, by = c("name"="county_name")) %>%
    left_join(poverty_final, by = "fips")

total_data <- total_data %>%
    mutate(per_cap_death_pct = 100*deaths/population, 
           case_death_pct = 100*deaths/cases,
           infection_pct = 100*cases/population)


# This tibble has population, but we need to make sure that it has no 
# missing values. 

missing_df <- total_data[rowSums(is.na(total_data)) > 0,]

# We see that a fairly small number of rows (less than  2.5%) are missing values
# We used a left join to hold onto all the information from the census tibble
# (because this is the one used in the original shiny app) so 
# there tends to be more missing data in the cases/deaths columns

# Some of these are fairly important counties 
# (Dade county, Virginia (beach, suffolk, hampton, nn, norfolk), New york (all city boros, SLC), St Louis MO, 
# St Louis MN, St Tammany LA, St Joseph IN, St Clair IL, St Lucie FL, )

# It is apparent right off the bat that the abbreviation St (Saint) is 
# causing problems with the join, probably because the nyt set is using St.) 
# We can save ourselves some trouble by removing . 

# It looks like the problem is that certain county names are formatted differently in the NYT 
# set versus the census set. Possibly there's a more elegant solution, but here's the plan 

# The issue with the boroughs/counties of New York City
# seems to be that they are combined into a single "county"
# named New York city. 

# The issue with the cities in Virginia seems to be that the counties are called, for example,
# "Newport News" in the census data set and "Newport News City" in the Covid data set. 

# The city of Newport News is an independent city (not part of another county)

# There are many independent cities in Virginia:
# Alexandria, Bristol, Buena Vista, Charlottesville, 
# Chesapeake, Colonial Heights, Covington, Danville,
# Emporia, 
# St Louis County and St Louis City are separate 


# Define UI ----
ui <- fluidPage(
    titlePanel("COVID-19 and Race"),

    sidebarLayout(
        sidebarPanel(
            helpText("This app displays four types of information:
                     \n \t * Demographic/racial data from 2010 Census
                     \n \t * COVID pandemic data collected by NYT
                     \n \t * Presidential vote (2016 General election)
                     \n \t * Poverty data (USDA Economic Research Service)"),
        selectInput("var",
                    h3("Choose a demographic variable to display"),
                    choices = list("Percent White",
                                   "Percent Black",
                                   "Percent Hispanic",
                                   "Percent Asian"),
                    selected = "Percent White"),
        
        sliderInput("range",h3("Range of demographic interest:"),
                    min = 0, max = 100, value = c(0,100)),
        
        selectInput("panvar",
                    h3("Choose a pandemic-related variable to display"),
                    choices = list("Infection rate",
                                   "Infected death rate",
                                   "Death rate per capita"),
                    selected = "COVID Cases"),
        
        selectInput("polvar",
                    h3("Choose a political variable to display"),
                    choices = list("Democratic voter share",
                                   "Republican voter share"),
                    selected = "Democratic voter share"),
        
        selectInput("econvar",
                    h3("Choose an economic variable to display"),
                    choices = list("Overall poverty rate",
                                   "Childhood poverty rate"),
                    selected = "Overall poverty rate")
        ),
        
        
        mainPanel(
            
        fluidRow(
            column(6,plotOutput("census_map")),
            column(6,plotOutput("COVID_map"))
                 ),
        fluidRow(
            column(6,plotOutput("pol_map")), 
            column(6,plotOutput("econ_map"))
        )
        )
    )
)


    
    
    

# Define server logic ----
server <- function(input, output) {
    output$census_map <- renderPlot({
        data <- switch(input$var, 
                       "Percent White" = total_data$white,
                       "Percent Black" = total_data$black,
                       "Percent Hispanic" = total_data$hispanic,
                       "Percent Asian" = total_data$asian)
        
        color <- switch(input$var, 
                        "Percent White" = "darkgreen",
                        "Percent Black" = "lightcyan",
                        "Percent Hispanic" = "palegreen4",
                        "Percent Asian" = "violet")
        
        legend <- switch(input$var, 
                         "Percent White" = "% White",
                         "Percent Black" = "% Black",
                         "Percent Hispanic" = "% Hispanic",
                         "Percent Asian" = "% Asian")
        
        
        percent_map(data, color, legend, input$range[1], input$range[2]+.1)
    })
    
    output$COVID_map <- renderPlot({
        data <- switch(input$panvar,
                       "Infection rate" = total_data$infection_pct,
                       "Infected death rate" = total_data$case_death_pct,
                       "Death rate per capita" = total_data$per_cap_death_pct
                       )
        
        color <- switch(input$panvar,
                        "Infection rate" = "red",
                        "Infected death rate" = "darkgreen",
                        "Death rate per capita" = "darkorange")
        
        legend <- switch(input$panvar,
                         "Infection rate" = "%Pop infected w/ COVID",
                         "Infected death rate" = "%Cases ending in death",
                         "Death rate per capita" = "%Pop dead of COVID")
        
        percent_map(data, color, legend, 0, 100)
    })
    
    output$pol_map <- renderPlot({
        data <- switch(input$polvar,
                       "Democratic voter share" = total_data$democrat_share,
                       "Republican voter share" = total_data$republican_share)
        
        color <- switch(input$polvar,
                        "Democratic voter share" = "blue",
                        "Republican voter share" = "red")
        
        legend <- switch(input$polvar,
                         "Democratic voter share" = "%Pop voting Dem (2016)",
                         "Republican voter share" = "%Pop voting GOP (2016)")
        
        percent_map(data, color, legend, 0, 100)
    })
    
    output$econ_map <- renderPlot({
        data <- switch(input$econvar,
                       "Overall poverty rate" = total_data$overall_poverty,
                       "Childhood poverty rate" = total_data$childhood_poverty)
        
        color <- switch(input$econvar,
                       "Overall poverty rate" = "darkred",
                       "Childhood poverty rate" = "burlywood")
        
        legend <- switch(input$econvar,
                         "Overall poverty rate" = "%People in poverty",
                         "Childhood poverty rate" = "%Children (0-17) in poverty")
        
        percent_map(data, color, legend, 0, 100)
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)