import sys


import pandas as pd
import numpy as np
import re
import requests
from lxml import html
from IPython.display import display, display_pretty, Javascript, HTML


# connect to data

election_url = "https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/national-map-page/national/president.json"

# make an http request for the page
election_request = requests.request(
    method='GET', 
    url=election_url,
    headers={ "Accept": "application/json" }
)

election_response = election_request.json()

election_data = election_response['data']['races']

## parse response into dataframe, and select and rename final columns
election_data_df = pd.DataFrame(election_data)[['state_name', 'counties']].rename(columns={"state_name": "t_state_name"})

# after https://stackoverflow.com/a/49962887
# unnest 'counties' column, turning object keys in dataframe columns and object values into rows, select certain keys from each array, and rename those keys (columns)
election_data_df = pd.DataFrame(
    [
        dict(y, t_state_name=i) for i, x in zip(
            election_data_df['t_state_name'],
            election_data_df['counties']
        ) for y in x
    ]
)[['fips', 'name', 'votes', 't_state_name', 'results']].rename(columns={"fips": "geoid", "votes": "total_votes", "name": "t_county_name"})

# after https://stackoverflow.com/a/38231651
## unravel dictionary (JSON object) into other columns, choose final columns, rename them, and cast their data types
election_data_df = pd.concat(
    [
        election_data_df.drop(['results'], axis=1), 
        election_data_df['results'].apply(pd.Series)
    ], 
    axis=1
)[['geoid', 't_county_name', 'total_votes', 't_state_name', 'trumpd', 'bidenj']].rename(columns={"trumpd": "votes_gop", "bidenj": "votes_dem"}).astype({'votes_gop': 'int64', 'votes_dem': 'int64'})

# create state FIPS codes from the 5-digit 'geoid'
election_data_df['state_fips'] = election_data_df['geoid'].str[:2]

election_data_df.to_csv('data/pres_race_2020.csv',index=False)