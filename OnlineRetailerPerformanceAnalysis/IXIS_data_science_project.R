# ===============INTRODUCTION =====================================
# Hello, my name is Danny Crytser, and welcome to my R script
# for the IXIS Data Analysis project. 

# I'm going to be a bit verbose in my comments, so feel free to skim forward
# to the code if you're already familiar with the tools used for data
# analysis in R. There are also quite a lot of print commands in the script, 
# all of which could be commented out if we just wanted the final Excel workbook
# as our output. 

# My hope is that someone with a basic knowledge of R could read this document
# and use it as a guideline/template for data analysis in future projects. 


# ================LOADING TOOLS AND DATA ==================================

# #First we load a few basic tools for data tidying and analysis.
# The first of these is the tidyverse, a collection of tools for manipulating,
# cleaning, and presenting data. This will give us the basic ability to 
# import the .csv files as 'tibbles,' a kind of table preferred for data 
# analysis. 

# The second of these is openxlsx, which lets us export dataframes
# as Excel workbooks that can be opened by people without using R or RStudio. 

# The last is lubridate, which makes working with dates a bit easier.

library(tidyverse)
library(openxlsx)
library(lubridate)


# The next step is to read the csv files into dataframes. I've put
# the .csv files into a folder called "Data."

to_cart_df <- read_csv('Data/DataAnalyst_Ecom_data_addsToCart.csv')
session_counts_df <- read_csv('Data/DataAnalyst_Ecom_data_sessionCounts.csv')

# Next, we print the first few rows of these tables, simply by 
# calling on them. This has the advantage of displaying the 
# size of the tibble. 

# (I prefer to just call the tibble itself because it displays the
# actual dimensions of the tibble instead of the 
# dimensions of the header. But anyone who prefers the header could
# un-comment these commands and comment out the calls to the dataframes.)


print("Here is the to_cart table") 
to_cart_df
#head(to_cart_df)
print("Here is the session_counts table") 
session_counts_df
#head(session_counts_df)

# From the terminal we learn that the to_cart tibble has 12 rows
# and 3 columns, and the session_counts tibble has 7734 rows
# and 6 columns. The tibbles helpfully display all the data types
# and we can see that the columns of to_cart are all dbl (double
# precision float), one for year, one for month, and one for addsToCart.

# ======================== LOOKING FOR MISSING DATA ==============================================

# Before we start pre-processing and wrangling the data, we should see how much of it is missing.
# The following two commands will print the number of missing values for each column to 
# the terminal. We'll have to scroll up to see it, but it's quite useful. 
print("Here is the number of missing values in the columns of the to_cart_df tibble:")
map(to_cart_df, ~sum(is.na(.)))
print("Here is the number of missing values in the columns of the session_counts_df tibble:")
map(session_counts_df, ~sum(is.na(.)))
# On finding this output in the terminal, we see that there are no missing values. Great!
# That means we can move on to wrangling and manipulating the data.... or does it?


# BRIEF ASIDE ON BROWSERS:
# If we look at the count of values we get something a little surprising. 

# print("Count of values for session_counts:")
# print(dplyr::count(session_counts_df, dim_browser), n=100)

# There are a fair number (364) of records in which the browser variable is listed as "error". 
# I don't plan to use this in my presentation, but it seems worth looking into
# as a possible technical issue. 

# =================PREPROCESSING AND WRANGLING THE DATA ====================

# In this section of the script, we carry out some minor changes to the data, 
# ensuring that date columns of the two tibbles are in the same data type and format. 


# The tibble session_counts has three string columns
# (browser type, device category, and date), and three dbl columns
# (for sessions, transactions, and quantity).

# We are eventually going to create a sheet that includes differences
# in all possible metrics (Sessions, Transactions, QTY, ECR, and Adds to Cart))
# This will entail combining the data from the to_cart tibble with the
# session_counts tibble, keyed together by month. 
# That means we will have to put the month in a format common to the two tibbles. 

# We will try to do this in a reproducible and non-ad-hoc
# fashion, so that it could be reproduced as needed.


#Here we do a bit of wrangling with the session_counts_df data frame:
# First, we split the dim_date string variable along the '/' delimiter
# Usually this would return a list of values for month, day, year. 
# However, we're not really interested in that, so the

split_session_counts <- session_counts_df %>%
  mutate(dim_date = parse_date(dim_date, "%m/%d/%y"))
head(split_session_counts)

# The header command in the terminal tells us that there were no errors in convering the date string
# (which we've renamed simplly as date). 

# It is useful to have the date changed over to the correct data type, but we only need the month for the first sheet. 
# We do this using commands from the lubridate library, which are useful for extracting part of a datetime value. 
# The last column in this is a string concatenation that will put dim_date as a mm/yyyy character string.

counts <- split_session_counts %>%
  mutate(month = month(dim_date), 
         year = year(dim_date), 
         my_date = paste(as.character(month), '/',as.character(year), sep = ""),
         month_date = as.Date(paste(as.character(month), '/01/',as.character(year), sep = ""), '%m/%d/%Y'))
head(counts)


# The following command casts the (numeric) dim_month and dim_year columns of the to_cart_df
# to character strings, then pastes them together with a slash so that it is in the same
# format as the date format from the counts tibble. The last thing in the pipeline
# is a drop command to get rid of the dim_year and dim_month columns.

print("Here is the modified cart table")
cart <- to_cart_df %>%
  mutate(dim_date = as.Date(paste(as.character(dim_month), '/1/', as.character(dim_year), sep=""), '%m/%d/%Y'))
cart


# Inspecting the terminal output shows that we have dim_date reformatted as mm/yyyy character
# strings in both columns. This will be ideal for the joins will we do to create the second sheet.



# ========================= SUMMARIZING AND AGGREGATING ==========================================



# Now that we have our data in a reasonably tidy form, it's time to aggregate. 

# Our first task is to group the data in the counts by browser and month. 

by_month_device <- counts %>%
  group_by(month_date, dim_deviceCategory)
# This chunk of code calculates summary stats for the grouped data. 
session_summary <- summarize(by_month_device, 
          monthly_sessions = sum(sessions), 
          monthly_transactions = sum(transactions), 
          monthly_QTY = sum(QTY))

# This chunk of code adds in the formula for ECR.  

session_summary <- session_summary %>%
  mutate(ECR = 1.0*monthly_transactions/(monthly_sessions))

# We can use a tool from the tidyverse to make sure the rows are arranged
# by date. 

session_summary <- session_summary %>%
  arrange(month_date)

# This will print out 40 rows of the grouped data, which we will eventually put into our .xlsx file.
print("Session_summary")
print(session_summary, n = 40)

# This is nice, but the month_date column has an unnecessary '-01' tag on each entry. 
# Let's remove that using some extract and paste commands. We'll have to ungroup in order to 
# drop the grouping variable month_date. 



session_summary <- session_summary %>%
  ungroup()
session_summary <-session_summary %>%
  mutate(my_date = paste(as.character(month(month_date)),'/', as.character(year(month_date)), sep=""))

# Now we drop the old month_date column
session_summary <- session_summary %>%
  select(-month_date)

# We want our my_date variable to be at the front, and dplyr provides us with a relocate()
# tool for just that purpose. 

session_summary <- session_summary %>%
  relocate(my_date)
#The following command allows us to display more columns than R would ordinarily allow.
options(tibble.width = Inf) 

# Now we should see the my_date variable in the first column. 
print("Session_summary, with simplified date")
print(session_summary, n = 40)

# On looking at this table, it is apparent that dim_deviceCategory is too long of a column name, 
# so lets change that. We can also remove the word monthly_ from all of the names.

# Now, we could have avoided doing this by naming things differently when we 
# called the summarize function, but I wanted to emphasize the distinction between 
# daily and monthly data when we were aggregating, and also I want to show off the rename function :) 

# The tidyverse/dplyr provides us with a rename function, aptly named "rename."


session_summary <- session_summary %>%
  rename(device = dim_deviceCategory,
         sessions = monthly_sessions,
         transactions = monthly_transactions,
         QTY = monthly_QTY)

print("Final version of session_summary table:")
head(session_summary)
# That looks -much- better. 

# The session_summary tibble contains all the information that needs to go in the first sheet
# of our .xlsx file. 

# To create the second file, we have to combine the counts and cart dataframes. 
# The common column is dim_date, so that is the "by" value in the inner join. 
counts <- counts %>%
  mutate(new_date = as.Date(paste(year(dim_date), '-',month(dim_date),'-01', sep="")))




# Now we want to group the table by the new_date so that we can get summary statistics. 
# In R this is accomplished first by piping the combined_df to the group_by command 
# (which has the grouping variable as a parameter) and then piping the result to 
# a summarize command. 

# (Both of these comes from the tidyverse and together form an analogue
# for the sort of summary statistics you could get in SQL using GROUP BY.)

monthly_counts <- counts %>%
  group_by(new_date) %>%
  summarize(total_sessions = sum(sessions),
            total_transactions = sum(transactions),
            total_quantity = sum(QTY)) %>%
  mutate(ECR = total_transactions/total_sessions)

# Now we can combine the two data frames together to get a unified set of 
# metrics for the website. 

combined_df <-  monthly_counts %>%
  inner_join(cart, by = c("new_date" = "dim_date"))
head(combined_df)

# We could add another metric: adds to cart per transaction

combined_with_add_ratio <-  combined_df %>%
  mutate(APT = addsToCart/total_sessions)

print("combined_df with adds to cart per session")
head(combined_with_add_ratio)

# We won't do a lot of work with the add_ratio in the rest of this section, but it will come up during the 
# visualization section 

# We'll change the name of the new_date colum to simply my_date (mm/yyyy). We do this in a few
# steps using tools from tidyverse: first we cut and paste a new string variable, then we drop
# the old date variable, then we relocate the new date variable to the first column. 

grouped <- combined_df %>%
  mutate(date = paste(month(new_date),'/',year(new_date), sep="")) %>%
  select(-new_date) %>%
  relocate(date)

print("Here is the grouped dataframe")
print(grouped, n=12)

# We want to see month-by-month changes in these summary statistics, so we use the lag command
# from basic R to mutate (create new columns). Thus the new column "prev_sessions" will have the 
# number of total sessions from the previous month, "prev_transaction" will have the number of 
# transactions from the previous month, etc. There will be missing values for this in the first month. 
lagged <- grouped %>%
  mutate(prev_sessions = lag(total_sessions),
         prev_transactions = lag(total_transactions),
         prev_quantity = lag(total_quantity),
         prev_adds_to_cart = lag(addsToCart),
         prev_ECR = lag(ECR))
# Now if we print the lagged table we'll have all the previous values among the columns,
# and they should all be missing values in the first row. We set the width to be unbounded so all the new columns display.

print("Here is the lagged dataframe")
options(tibble.width = Inf) 
print(lagged, n=12)

# Now let's create the change columns (don't worry we'll reorganize the columns later on
# so that the lagged columns are next to their originals). 

# The absolute difference is just the current monthly value minus the previous monthly value,
# and the relative difference is the absolute difference divided by the previous monthly value. 

# (Note: there are a lot of columns being added here, )

lagged <- lagged %>%
  mutate(rel_sess_change = (total_sessions - prev_sessions)/prev_sessions,
         abs_sess_change = total_sessions- prev_sessions,
         rel_trans_change = (total_transactions - prev_transactions)/prev_transactions,
         abs_trans_change = total_transactions - prev_transactions,
         rel_qty_change = (total_quantity - prev_quantity)/prev_quantity,
         abs_qty_change = total_quantity - prev_quantity, 
         rel_adds_cart_change = (addsToCart - prev_adds_to_cart)/prev_adds_to_cart, 
         abs_adds_cart_change = addsToCart - prev_adds_to_cart,
         rel_ECR_change = (ECR - prev_ECR)/prev_ECR, 
         abs_ECR_change = ECR - prev_ECR)




# We want all the derived columns (lag, absolute difference, and relative difference)
# to be next to their originals. The following somewhat lengthy pipeline of relocations
# does this. 

lagged <- lagged %>%
  relocate(prev_sessions, .after = total_sessions) %>%
  relocate(abs_sess_change, .after = prev_sessions) %>%
  relocate(rel_sess_change, .after = abs_sess_change) %>%
  relocate(prev_transactions, .after = total_transactions) %>%
  relocate(abs_trans_change, .after = prev_transactions) %>%
  relocate(rel_trans_change, .after = abs_trans_change)  %>%
  relocate(prev_quantity, .after = total_quantity) %>%
  relocate(abs_qty_change, .after = prev_quantity) %>%
  relocate(rel_qty_change, .after = abs_qty_change) %>%
  relocate(prev_adds_to_cart, .after = addsToCart) %>%
  relocate(abs_adds_cart_change, .after = prev_adds_to_cart) %>%
  relocate(rel_adds_cart_change, .after = abs_adds_cart_change) %>%
  relocate(prev_ECR, .after = ECR) %>%
  relocate(abs_ECR_change, .after = prev_ECR) %>%
  relocate(rel_ECR_change, .after = abs_ECR_change)
  

# There are other ways to do this, such as defining a location function, 
# but this is one way to do it just with a series of switches. 

print("Here is the final lagged tibble:")
print(lagged, n =12)

# Before we create the workbook, we should create a smaller tibble that only includes the 
# records from the two most recent months in the record set. 
# The tidyverse provides us with a handy slicing function (called slice), 
# and we can simply pipe the lagged tibble to slice with the rows we want as a list of parameters.

two_recent <- lagged %>%
  slice(n()-1,n())
print("Here are the two most recent records")
print(two_recent)


# ==================== CREATING THE WORKBOOK ============================

# In this section, we'll put the datasets we created into a "Workbook" and
# then export them as an Excel file. We already imported the necessary openxlsx
# library at the beginning of the project, so we don't need to import anything new. 

# The first step is to create a Workbook object, which we label wb. 

wb <- createWorkbook()

# Right now wb is an empty Workbook, meaning it has no "sheets" that we can write data to.
# So we add in some blank sheets (these will not contain any data at first). The names
# "First_sheet" and "Second_sheet" are just strings -- we could have named them "One" and "Two" or anything else. 

addWorksheet(wb, "First_sheet")
addWorksheet(wb, "Second_sheet")

# Now wb has two blank sheets with the given names. Writing data to a workbook requires
# you to specify three parameters: the workbook, the sheet (identified with a string name), 
# and the dataframe you want to add.


writeData(wb, "First_sheet", session_summary)
writeData(wb, "Second_sheet", two_recent)

# Now the sheets of the Workbook object wb contain the data we want them to contain,
# and all that is left is to save the Workbook to our computer. 

saveWorkbook(wb, file = "IXIS_Data_Science.xlsx", overwrite = TRUE)

# The workbook should be saved now! If we upload it to Google Drive or open it in Microsoft Excel,
# we see the data that we wanted. 

# ================ ANALYSIS AND VISUALIZATION ============================

# The last step of this process is creating some visualizations for our client presentable. 
# We've already noticed a few things during the previous steps:

# -- ECR is consistently in the mid to high 3 percent range for desktops
# -- ECR is in the mid 2 percent to low 3 percent range for tablets
# -- ECR is the low 1 to low 2 percent range for mobiles. 

# This means that a higher percentage of desktop sessions lead to a transaction than 
# tablets and a higher percentage of tablet sessions lead to a transaction than mobile. 

# (This is not surprising: lots of the time I'll idly browse through something on mobile but 
# I'll wait to buy it until I get home where I can view it on my desktop.)


# Also, in the last two months, there has been an _enormous_ surge in the number of sessions
# -- The total session count is up by 224,195 in June 2013 over May 2013, an increase of %20. 

# The single biggest surge took place in April 2013, when the number of sessions increased
# by half a million. However, the ECR was lower by about one tenth in April 2013 compared with 
# March 2013, so we had fewer transactions per session on average.

# Action step: keep doing whatever marketing action led to the surge in April 2013 sessions, 
# but make it easier for clients to start a transaction. 

# However, we trace this to our to_cart table, there's basically no change in -- in fact, 
# the to_cart statistic dropped between May 2013 and June 2013 (there's quite a bit of variance
# in the to_cart variable). So another thing we would like to focus on is getting more to_cart 
# events from sessions). Even though more people are visiting the site, the number of toCart events
# is not increasing. 

# Action step: make it easier/more interesting for people to add items to the cart. 

# Lets use ggplot2 to get some line graphs that can display these trends for us. 
# We already loaded ggplot2 when we loaded the tidyverse. 

# Before we get into visualization, wrangle some dates back into date form so that they 
# order correctly when we visualize. 
library("zoo")


# We do a little ad hoc variable wrangling in order to ensure the dataframe orders
# the rows correctly. 


# session_summary <- session_summary %>%
#   mutate(new_date = as.yearmon(my_date, "%m/%Y"))

# We are going to create a visualization that only displays a few months. 
# One way to do that is with the filter function from dplyr. 

# short_months <- c('2/2013','3/2013','4/2013','5/2013','6/2013')
# holiday_months <- c('10/2012','11/2012','12/2012')
# print(lagged)
# short_session <- lagged %>%
#   filter(date %in% short_months)
# 
# holiday_session <- lagged %>%
#   filter(date %in% holiday_months)
# 
# holiday_plot<- ggplot(holiday_session) + 
#   geom_line(aes(date, 100*rel_sess_change, group = 1), color = 'blue')+
#   geom_line(aes(date, 100*rel_ECR_change, group = 1), color = 'red')+
#   theme_minimal() + 
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title.y = element_text(size=22, face = "bold"),
#         axis.text.y = element_text(size = 14),
#         axis.title.x = element_text(size=22, face = "bold"),
#         axis.text.x = element_text(size = 14)) + 
#   geom_label(
#     label="Sessions", 
#     x=2.5,
#     y=19,
#     label.padding = unit(0.55, "lines"), # Rectangle size around label
#     label.size = 1,
#     color = "black",
#     fill="#69b3a2"
#   ) + 
#   geom_label(
#     label="ECR", 
#     x=1.5,
#     y=6,
#     label.padding = unit(0.55, "lines"), # Rectangle size around label
#     label.size = 1,
#     color = "black",
#     fill="#69b3a2"
#   ) + 
#   labs(y = "Relative change (%)",
#        x = "Date (m/yyyy)")
# holiday_plot
# ggsave("holiday_plot.png")
# Now we create a ggplot object that has the change in the number of sessions
# as well as the change in the ECR (transactions over sessions). 

# None of the geometries we're using in this plot are very complicated,
# but it does take a little work to make sure the labels go where we want them. 
# sessions_plot <- ggplot(short_session) + 
#   geom_line(aes(date, 100*rel_sess_change, group = 1), color = 'blue')+
#   geom_line(aes(date, 100*rel_ECR_change, group = 1), color = 'red')+
#   theme_minimal() + 
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title.y = element_text(size=22, face = "bold"),
#         axis.text.y = element_text(size = 14),
#         axis.title.x = element_text(size=22, face = "bold"),
#         axis.text.x = element_text(size = 14)) + 
#   geom_label(
#     label="Sessions", 
#     x=2.5,
#     y=50,
#     label.padding = unit(0.55, "lines"), # Rectangle size around label
#     label.size = 1,
#     color = "black",
#     fill="#69b3a2"
#   ) + 
#   geom_label(
#     label="ECR", 
#     x=2.5,
#     y=-10,
#     label.padding = unit(0.55, "lines"), # Rectangle size around label
#     label.size = 1,
#     color = "black",
#     fill="#69b3a2"
#   ) + 
#   labs(y = "Relative change (%)",
#        x = "Date (m/yyyy)")
# 
# # Now we output the ggplot and save it to the disk. 
# sessions_plot
# ggsave("sessions_plot.png")
# 
# 
# #Here is another ggplot that we create just to have on-hand. 
# 
# ECR_plot <- ggplot(lagged, aes(date, ECR, group = 1)) + 
#   geom_line()
# ggsave("ECR_plot.png")

# This is a ggplot that I wrote and then decided to skip, but I 
# didn't want to delete in case it could be changed into 
# something useful. 

# ECR_device_plot <- ggplot(session_summary, aes(x = my_date, 
#                                                y = ECR,
#                                                fill = factor(device), 
#                                                group = device))


# Before creating our next plot, which is just a simple bar diagram,
# it's helpful to create a little data frame that will save us some
# work in the ggplot mapping. 

# This will show the average ECR by device over all months. 
# 
# WARNING/CAVEAT: Technically, we're being a little sloppy here. It's not considered
# great form to average ratios like this, because an average of ratios
# should always be weighted by the relative amounts.

# However, in -this- case the numbers of transactions and sessions are 
# close enough in each time unit that it doesn't make a huge amount of difference.
# If we were writing this code for production we would want to check.

# device_grouped <- session_summary %>%
#   group_by(device) %>%
#   summarize(avg_ECR = mean(ECR))
# 
# 
# print(device_grouped)

# Another ggplot that I've abandoned but could come back to some day. 

# device_ECR_simple <- ggplot(data = device_grouped,
#                             aes(x = device,
#                                 y = avg_ECR,
#                                 fill = device))+
#   geom_bar()
# ggsave("ECR_device_simple_plot.pdf")

# Here is a ggplot that is fairly simple in what it aims to do
# but has a few moving parts. 
# The first geometry is just a bar graph (geom_col),
# followed by some labels that we set at 90 percent of the height
# of the bars to ensure each label falls in the interior of its
# respective bar.
# The rest of the settings are cosmetic changes to make sure 
# that the graph isn't too busy but has clear axis labels. 

# ECR_device_plot <-ggplot(data = device_grouped,
#                          aes(fill = device,
#                          x = device,
#                          y = avg_ECR)) + 
#   geom_col(show.legend = FALSE) + 
#   geom_text(aes(label = device, y = 0.9*avg_ECR), size = 12) +
#   theme_minimal()+
#   labs(y = "Average ECR")+
#   theme(axis.title.y = element_text(size=22, face = "bold"),
#         axis.text.y = element_text(size = 14),
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# ggsave("small_ECR_device_plot.png")


