In the US, the two biggest news topics of 2020 were the presidential election and the covid-19 pandemic. The virus itself became a highly politicized topic as the pandemic wore on. The question that I came up with was: does political leaning determine the sentment of social media posts related to Covid? Strongly negative terms like "liar" and "sham" have taken on a political aspect, so it seemed natural to see if there was meaningful difference between "liberal" and "conservative" tweets about the virus. 

For this project I looked at a large set of tweets related to Covid-19 (available at the Kaggle link below). I then filtered by user bio (an optional field for Twitter users), using bio keywords to classify users as either "conservative" or "liberal." This considerably thinned the number of tweets I had to work with, but I still had several thousand left over. Then I used NLP to classify the sentiment of each tweet in the filtered data set as either "positive" or "negative."

Using some pretty basic statistical hypothesis testing, I found that there was no significant difference between the sentiment of "liberal" users and "conservative" users. Anecdotally it did occur that conservative tweets about Covid-19 were slightly more negative. I think that more work could be done to expand the bio-filtering (the project discarded a very high proportion of the tweets). 

In order to use the Jupyter Notebook, you'll need the .csv file from this link (too large for Github!). https://www.kaggle.com/gpreda/covid19-tweets

