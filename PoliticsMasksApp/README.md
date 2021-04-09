This is a Shiny app that displays information collected by the NYT. 

1) The first dataset displays county-level data from the 2020 Presidential election.
This data is officially attributed to The New York Times and Dynata.

The original set had quite a bit more, but I cut it down to Democratic 
and Republican vote shares. The data is tagged by FIPS, which is sort of like a ZIP code
attached to a county (FIPS and ZIP are not related -- multiple ZIPs can correspond to the 
same FIPS and vice versa). 

If you want to access the JSON file it's available from the NYT API <a href = "https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/president.json"> here </a>

You can also download state-by-state as in the following, using hyphen ('-') to separate South Dakota <a href = "https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/race-page/south-dakota/president.json" > here </a>

These JSON files are nested somewhat deeply and sort of annoying to deal with, so I punted and 
took a bit of Python script to unnest the JSON file from <a href = "https://github.com/tonmcg/US_County_Level_Election_Results_08-20/blob/master/2020_US_County_Level_Presidential_Results.ipynb">
github user tonmcg </a>

My excerpt/riff on tonmcg's python code is saved as munging.py and running this Python script yields the 
csv file pres_race_2020.csv saved in the data directory. 

2) The second data set displays responses from a mask usage poll taken between July 2 and July 14 of 2020. 
It's a bit more amenable to tidying, as it was saved as a simple .csv tagged with a single column instead
of as a nested JSON. <a href = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv">
Link to the NYT mask data: 
</a>

Apart from the Python script used to sort out the presidential election data,
everything else is handled in app.R, including a small amount of 
joining and mutating using tidyverse tools. 

REMARKS ON THE APP: 

It seems like there are marked statewide trends in the 
