# Title: Working with ONS population data
# Author: Jeremy Horne
# Date created: 2020-10-14

# Load packages required for this script:
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(reshape2))

# Set up raw data location:
File_location <- "C:\\Users\\horne\\OneDrive\\Documents\\R\\David - Population\\Raw\\"

# Create a blank data frame to store the merged files in:
Master_data <- data.frame()

# Read in files
for (i in list.files(File_location)) {
  temp <- read_csv(paste0(File_location,i))
  Master_data <- bind_rows(Master_data,temp)
}

# Remove all of the total rows:
Master_data <- Master_data %>% filter(AGE_GROUP != "All ages") %>% 
  mutate(AGE_GROUP = str_replace_all(AGE_GROUP," and over",""))

# Group ages into bands of 5:
Master_data <- Master_data %>% mutate(AGE_BAND = cut(as.numeric(Master_data$AGE_GROUP), seq(0, 90, 5), 
                                                     include.lowest = TRUE,right=FALSE))

# Group and summarise the data by age band:
Pop_by_age_area <- Master_data %>% group_by(AGE_BAND,AREA_NAME) %>% summarise_if(is.numeric,sum)

# Put the data into a tidy format - so that 'year' is a single column:
Pop_by_age_area_year <- Pop_by_age_area %>% 
  melt(id.vars=c("AREA_NAME","AGE_BAND"),variable.name="Year",value.name="Population") %>% 
  mutate(Year=as.numeric(as.character(Year)))

# Visualise for an individual area, e.g. East Sussex - first create an 'area' dataset
Chart_data <- Pop_by_age_area_year %>% filter(AREA_NAME=="East Sussex") 

# Now plot this as a line for each age bracket
Plot <- ggplot(Chart_data,aes(x=Year,y=Population,color=AGE_BAND)) +
  geom_line() +
  theme_minimal() # this gets rid of the background panel colours and generally makes things easier to read
Plot
