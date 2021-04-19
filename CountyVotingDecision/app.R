# ----LOADING LIBRARIES  ----

# we probably don't need the whole tidyerse but 
# it's a bit easier


library(shiny)
library(tidyverse)
library(RCurl)
library(rpart)
library(maps)
library(mapproj)
library(rpart.plot)

# ----LOADING DATA ----

# get 2010 US census data (data on county-level racial composition)

census_df <- readRDS("data/census.rds")
census_df <- as_tibble(census_df)
census_df <- census_df %>%
    rename(total_pop = total.pop)

# get population density data 

density_url <- getURL("https://raw.githubusercontent.com/ykzeng/covid-19/master/data/census-population-landarea.csv")
density_df <- read_csv(density_url)

# get USDA county-level education statistics

educ_url <- getURL("https://www.ers.usda.gov/webdocs/DataFiles/48747/Education.csv")
educ_df <- read_csv(educ_url)

# get poverty stats

pov_url <- getURL("https://www.ers.usda.gov/webdocs/DataFiles/48747/PovertyEstimates.csv")
pov_df <- read_csv(pov_url)

# get unemployment stats 

emp_url <-  getURL("https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.csv")
emp_df <- read_csv(emp_url)

# get pop stats 

pop_url <- getURL("https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv")
pop_df <- read_csv(pop_url)

# the last data set we want is a .csv containing countywide election data
# we got this for a previous project and the code to generate it is 
# contained in the script munging.py

vote_df <- read_csv('data/pres_race_2020.csv')




# ---- WRANGLING DATA ----

# There are a few changes we have to make to the data sets before we can join



# 1) We need to widen the emp_df and pov_df tables

pov_df <- pov_df %>%
    pivot_wider(names_from = Attribute,
                values_from = Value) 

emp_df <- emp_df %>%
    pivot_wider(names_from = Attribute,
                values_from = Value)

# 2) The census_df is missing a FIPS column, and instead it has a name column

# we can repair this by joining it to another table that contains both name and FIPS

# glimpsing the dataframes we've loaded so far, we see that the 
# employment, population, and poverty dataframes have poorly formatted
# FIPStxt columns (in fact they are recorded as double values when they 
# should be character strings)

# as the FIPS is the joining key we need it to be in a standard format

# the following code can be used to fix the faulty FIPS entries

prepend_zero <- function(number_value){
    char_value <- as.character(number_value)
    if(str_length(char_value) < 5){
        char_value <- paste("0",char_value, sep = "")
    }
    char_value
}

prepend_zero <- Vectorize(prepend_zero)


# Now we can actually go through and fix the FIPStxt columns in 
# emp_df, pop_df, and pov_df.

# While we do this we add in duplicates of the (valid) FIPS columns
# in the other two data sets

density_df <- density_df %>%
    mutate(fips = prepend_zero(fips))

pov_df <- pov_df %>%
    mutate(fips = prepend_zero(FIPStxt))

emp_df <- emp_df %>%
    mutate(fips = prepend_zero(fips_txt))

pop_df <- pop_df %>%
    mutate(fips = prepend_zero(FIPStxt))

vote_df <- vote_df %>%
    rename(fips = geoid)

educ_df <- educ_df %>%
    rename(fips = 'FIPS Code')

# Add in dem_pct and gop_pct to the vote_df

vote_df <- vote_df %>%
    mutate(dem_pct = 100*votes_dem/(votes_dem+votes_gop),
           gop_pct = 100*votes_gop/(votes_dem+votes_gop)
    )

# Add in categorical variable (did the county vote Dem/GOP in 2020 Pres)
# to vote_df

# Probably unnecessarily we write a little function to make 
# the call to mutate a bit simpler

majority_vote <- function(votes_dem, votes_gop){
    vote = 'D'
    if(votes_dem < votes_gop){
        vote = 'R'
    }
    vote
}

# whenever you're using a function in mutate, don't forget to vectorize!

majority_vote <- Vectorize(majority_vote)

# now we create the majority vote categorical var

vote_df <- vote_df %>%
    mutate(majority = majority_vote(votes_dem, votes_gop))
    
vote_df$majority <- as.factor(vote_df$majority)



# we also include a state-level majority to decide how many states we 
# classify incorrectly



state_df <- vote_df %>%
    group_by(t_state_name) %>%
    summarise(total_dem = sum(votes_dem), 
              total_gop = sum(votes_gop)) %>%
    mutate(state_majority = majority_vote(total_dem,
                                          total_gop))


    # %>% group_by(t_state_name) 
    # %>% summarise(total_dem = sum(votes_dem), total_gop = sum(votes_gop))
    # %>% mutate(state_majority  = majority_vote(total_dem, total_gop))

state_df$state_majority <- as.factor(state_df$state_majority)

vote_df <- vote_df %>%
    inner_join(state_df, by = 't_state_name')


# ---- JOINING + SELECTING DATA ----

# combine the dataframes together

# the order matters, because the helper function we're using is designed
# to be applied mainly to census_df, so we want to make sure the counties
# come out in the order specified by census_df. 

df <- census_df %>%
    left_join(density_df, by = "fips") %>%
    left_join(educ_df, by = "fips") %>%
    left_join(emp_df, by = "fips") %>%
    left_join(pop_df, by = "fips") %>%
    left_join(pov_df, by = "fips") %>%
    left_join(vote_df, by = "fips")


# choose variables of interest. 
# there are hundreds of variables in df currently, far more than we need

# we'll just choose a few that, based on popular discourse, seem likely to
# influence the election outcome of a county. 

# From census_df we'll take total.pop as well as white, black, hispanic, asian

# From density_df we'll just take population density (ppl/sq mi.) which 
# is labeled as POP060210

# From educ_df we will take the variables (all self-explanatory): 

# Percent of adults with less than a high school diploma, 2015-19
# Percent of adults with a high school diploma only, 2015-19
# Percent of adults completing some college or associate's degree, 2015-19
# Percent of adults with a bachelor's degree or higher, 2015-19

# From emp_df we will take the variables 

# Unemployment_rate_2019
# Median_Household_Income_2019
# Med_HH_Income_Percent_of_State_Total_2019
# Rural_urban_continuum_code_2013
# Urban_influence_code_2013  
# Metro_2013

# The first three of these are self-explanatory ('HH'=Household)

# The last three are ordinal variables that describe how rural/urban
# a county is. 

# The documentation for these is available for download along with the data sets
# at https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/

# For example, the Urban Influence code assigns values on a 1-12 scale
		
# 1	In large metro area of 1+ million residents			
# 2	In small metro area of less than 1 million residents			
# ...		
# 11	Noncore not adjacent to metro or micro area and contains a town of at least 2,500 residents			
# 12	Noncore not adjacent to metro or micro area and does not contain a town of at least 2,500 residents			

# The population statistics dataset is particularly big, 166 variables

# Choosing the following: 

# POP_ESTIMATE_2019
# N_POP_CHG_2019 (population change in the year 2019)
# R_NATURAL_INC_2019 (natural increase = birth rate-death rate)
# INTERNATIONAL_MIG_2019 (international migration to this FIPS)

# The povery dataset has only 34 variables, we'll take
# PCTPOVALL_2019 (pct of population in poverty in 2019)
# POV017_2019	Estimate of people age 0-17 in poverty 2019

# The vote dataset only contributes the variable majority

good_columns <- c( 
                  "fips",
                  "total_pop",
                  "white",
                  "black",
                  "hispanic",
                  "asian",
                  "Percent of adults with less than a high school diploma, 2015-19", 
                  "Percent of adults with a high school diploma only, 2015-19",
                  "Percent of adults completing some college or associate's degree, 2015-19",
                  "Percent of adults with a bachelor's degree or higher, 2015-19",
                  "Unemployment_rate_2019",
                  "Median_Household_Income_2019",
                  "Med_HH_Income_Percent_of_State_Total_2019",
                  "Rural_urban_continuum_code_2013",
                  "Urban_influence_code_2013",
                  "Metro_2013",
                  "POP_ESTIMATE_2019",
                  "N_POP_CHG_2019",
                  "R_NATURAL_INC_2019",
                  "INTERNATIONAL_MIG_2019",
                  "PCTPOVALL_2019",
                  "POV017_2019",
                  "POP060210",
                  "majority")

main_df <- df %>%
    select(all_of(good_columns))

main_df <- main_df %>%
    rename("no_high_sch" = "Percent of adults with less than a high school diploma, 2015-19",
           "high_sch%" = "Percent of adults with a high school diploma only, 2015-19",
           "some_col%" = "Percent of adults completing some college or associate's degree, 2015-19",
           "college%" = "Percent of adults with a bachelor's degree or higher, 2015-19",
           "unemployment" = "Unemployment_rate_2019",
           "med_hh_income" = "Median_Household_Income_2019",
           "med_hh_income_pct_state" = "Med_HH_Income_Percent_of_State_Total_2019",
           "rural_urban_continuum" = "Rural_urban_continuum_code_2013",
           "urban_influence" = "Urban_influence_code_2013",
           "metro_code" = "Metro_2013",
           "population" = "POP_ESTIMATE_2019",
           "pop_change" = "N_POP_CHG_2019",
           "nat_incr_rate" = "R_NATURAL_INC_2019",
           "intl_migration" = "INTERNATIONAL_MIG_2019",
           "poverty_pct" = "PCTPOVALL_2019",
           "child_poverty" = "POV017_2019",
           "ppl_per_sq_mi" = "POP060210"
           )


# ---- CALIBRATING DECISION TREES ----

# we need to split main_df into a training set and a test set


# create ids 

main_df <- main_df %>% 
    mutate(id = row_number())
# check the ids 
head(main_df$id)

# make a training set
train <- main_df %>% sample_frac(.70)

# make a test set
test  <- anti_join(main_df, train, by = 'id')

# R's decision tree library complains a lot if there is a lot of missing data 
# We take the lazy route and just omit the data if something is missing

# we want to predict outcome (majority) with the freedom
# to use every variable except for fips (too specific)

# train tree
tree_majority <- rpart(majority~.-fips,
                       method = "class", 
                       data = na.omit(main_df),
                       cp = 0.001)


# plot tree
prp(tree_majority, 
    uniform=TRUE,
    main="Which way does a county vote?", 
    Margin = 0.1,
    border.col = "black",
    split.cex = 0.7, 
    extra = 0)

# create attractive png of tree
post(tree_majority, file = "decision_tree.png",
     title = "CountyVotingTree")

# if we view the summary, we see the variables used are:

# college_or_more, white, black, total_pop, intl_migration, med_hh_income_pct_state

# Our current training accuracy is 0.92281, pretty good. 

# this generates the test predictions

tree_pred <- predict(tree_majority, test, type = "class")

table(tree_pred, test$majority)

# the test accuracy is 0.8843683, barely less than the training accuracy

# As an aside 

# ---- USER INTERFACE ----

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How to predict an election with decision trees"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("tree_plot")
        )
    )
)
# ---- SERVER LOGIC ----

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$tree_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        tree_majority <- tree(majority~.-fips, train)
        
        
    })
}

# ---- RUN APP ---- 

# Run the application 
shinyApp(ui = ui, server = server)
