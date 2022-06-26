# Martians Are Coming! 
# Alana McPhail 
# June 26, 2022

# Loading libraries 
library(dplyr)


# Read the data into a dataframe (check to make sure that column names do not have spaces in them)
ufo <- read.csv("ufo_subset.csv")

# renaming columns which had spaces 
ufo <- ufo %>% 
  rename(durationSeconds = duration..seconds., durationHoursMin = duration..hours.min., date = date.posted)


# Clean up the rows that do not have Country or Shape information
ufo1 <- ufo %>% 
  filter(country != "", shape != "")


# Convert Datetime and Date_posted columns into appropriate formats


# NUFORC officials comment on sightings that may be hoax. Figure out a way (go through the Comments and decide how a proper filter should look like) and remove these sightings from the dataset.
ufo2 <- ufo1 %>% 
  filter(!grepl("HOAX", comments, ignore.case = T))

# Add another column to the dataset (report_delay) and populate with the time difference in days, between the date of the sighting and the date it was reported.
# Filter out the rows where the sighting was reported before it happened.
# Create a table with the average report_delay per country.
# Check the data quality (missingness, format, range etc) of the duration(seconds) column. Explain what kinds of problems you have identified and how you chose to deal with them, in your comments.
# Create a histogram using the duration(seconds) column.
