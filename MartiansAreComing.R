# Martians Are Coming! 
# Alana McPhail 
# June 26, 2022

# Loading libraries 
library(dplyr)
library(ggplot2)

# Read the data into a dataframe (check to make sure that column names do not have spaces in them)
ufo <- read.csv("~/Documents/MBiotech/MSC2011/Assignments/Assignment4-Ashley-/ufo_subset.csv")

#' it is useful to create copies of the original dataset when modifying it so you can be sure to
#' retain the original dataset  
ufo_final <- ufo %>% 
  # renaming columns which had spaces 
  rename(durationSeconds = duration..seconds., 
         durationHoursMin = duration..hours.min.,
         #' perhaps can rename date.posted as datePosted to keep it consistent and easier to remember
         #' which column in the original dataset this new name corresponds to
         datePosted = date.posted) %>% 
  # Clean up the rows that do not have Country or Shape information
  filter(country != "", shape != "") %>% 
  # Convert Datetime and Date_posted columns into appropriate formats
  mutate(datetime = gsub(" .*", "", datetime)) %>% 
  # NUFORC officials comment on sightings that may be hoax. Figure out a 
  # way (go through the Comments and decide how a proper filter should look like) 
  # and remove these sightings from the dataset.
  filter(!grepl("HOAX", comments, ignore.case = T)) %>%
  # Add another column to the dataset (report_delay) and populate with the 
  # time difference in days, between the date of the sighting and the date it was reported.
  mutate(report_delay = as.Date(datePosted) - as.Date(datetime)) %>% 
  # Filter out the rows where the sighting was reported before it happened.
  # it is not necessary to convert to numeric since it is a double
  mutate(numericDelay = as.numeric(gsub("([0-9]+).*$", "\\1", report_delay))) %>% 
  filter(numericDelay >= 0)

# Create a table with the average report_delay per country.
ufoSummary <- ufo_final %>% 
  group_by(country) %>%
  summarise(mean(report_delay))

# Check the data quality (missingness, format, range etc) of the 
# duration(seconds) column. Explain what kinds of problems you have identified 
# and how you chose to deal with them, in your comments.

  ## checking range of seconds.  0.02 - 52623200.00
range(ufo_final$durationSeconds)

  ## checking structure of duration seconds
str(ufo_final$durationSeconds)

  ## Checking to see number of Nas: 0 
sum(is.na(ufo_final$durationSeconds))

  ## Checking to see number of blanks: 0 
sum(ufo_final$durationSeconds == "")

# Some values have a decimal, others do not! Making the duration second column all have 2 decimals. 
ufo_final$durationSeconds <- format(round(ufo_final$durationSeconds), nsmall = 2)


# Create a histogram using the duration(seconds) column.
hist(log(as.numeric(ufo_final$durationSeconds)), main = "Log of UFO duration in seconds", 
     xlab = "Log(Duration in Seconds)", col = "blue")


#' Ashley review: Code was really well commented and the variable names were variable 
#' readable. This made the code very easy to follow. The code functioned as expected and 
#' was able to run from source without any errors. 
 



