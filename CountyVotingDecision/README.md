This is a project that aims to predict how a county will vote using demographic information from a variety of sources. 

Using simple decision trees with the rpart R package we see that the single most important demographic variable is the % of the county 
population that has completed college. After that initial split, racial variables as well as economic and immigration-related variables
control the voting outcome. 

While the data used here are not granular enough to predict an individual's voting tendencies, they are useful
for planning political activism at the county level. One application of this model: identify all counties
that are incorrectly classed as Democratic by the decision tree; these should have a lot of attributes that would incline them to vote
Democratic, and could be targeted for political activism. 
