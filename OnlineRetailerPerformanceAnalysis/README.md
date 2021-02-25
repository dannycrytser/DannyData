Here is an exploratory and explanatory data analysis I carried out on some online marketing data, with an eye toward improving performance. 

* The main R script (IXIS_data_science_project.R) will run as long as you have all required libraries installed (tidyverse, openxlsx, lubridate). Library "zoo" is used in the last section but only for ggplot calls that have been commented out.   
* I have the data stored in a folder called Data within the main directory. <i> The R script will not run if the .csv files aren't in the Data folder </i>
* If you don't want to create that folder to hold the .csv files, you can edit the script to change the read_csv commands. 
* There is a substantial amount of output to the terminal, mostly headers and views of various tibbles. These can be commented out. 
* The only thing written by the R script should be a .xlsx file called IXIS_Data_Science.xlsx. 
* This contains two sheets with the selected data.
* I've commented out all the commands generating images at the end, but they can be uncommented.
* The second part is a .ppt and .pdf of the client deliverable slide deck.
