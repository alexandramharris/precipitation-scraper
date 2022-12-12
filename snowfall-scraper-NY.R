# Snow totals ----
# Alexandra Harris

# Source: https://forecast.weather.gov/product.php?site=ALY&issuedby=ALY&product=PNS&format=ci


# Set up ----
library(tidyverse)
library(rvest)
library(stringr)
library(tidyr)
library(googlesheets4)
library(lubridate)

# Scraper ----

# Read webpage
webpage <- read_html("https://forecast.weather.gov/product.php?site=ALY&issuedby=ALY&product=PNS&format=ci")

# Select element
metadata_nodes <- html_nodes(webpage, "pre.glossaryProduct")

# Trim as text
metadata_text <- str_trim(metadata_nodes)

# Convert to dataframe
metadata_df <- as.data.frame(metadata_text)

# Extract below metadata text only
metadata <- as.data.frame(gsub(".*\\*\\*\\*\\*\\*METADATA\\*\\*\\*\\*\\*", "", metadata_df$metadata_text))

# Rename 
colnames(metadata)[1] = "col"

# Separate
scraper <- metadata %>% 
  separate_rows(col, sep=":") %>% 
  separate(col, paste('col', 1:14, sep=":"), sep=",", extra="drop")

# Drop blank rows
scraper <- scraper %>% drop_na

# Clean white space
scraper <- scraper %>% mutate_all(funs(trimws))

# Rename columns
colnames(scraper)[1] = "Date"
colnames(scraper)[2] = "Time"
colnames(scraper)[3] = "State"
colnames(scraper)[4] = "County"
colnames(scraper)[5] = "Location"
colnames(scraper)[6] = "Unknown"
colnames(scraper)[7] = "Unknown2"
colnames(scraper)[8] = "Latitude"
colnames(scraper)[9] = "Longitude"
colnames(scraper)[10] = "Precipitation"
colnames(scraper)[11] = "Inches"
colnames(scraper)[12] = "Unit"
colnames(scraper)[13] = "Method"
colnames(scraper)[14] = "Measurement"

# Format date
scraper$Date <- as.Date(scraper$Date , format = "%m/%d/%Y")

# Add day of week
scraper$`Day reported` <- weekdays(scraper$Date) 

# Format time
# tk

# Concatenate day and time
# tk

# Filter for counties
scraper <- scraper %>% 
  filter(!is.na(County) & (County == "Albany" | County == "Saratoga" | County == "Schenectady" | County == "Rensselaer" | County == "Greene" | County == "Ulster" | County == "Delaware" | County == "Sullivan" | County == "Columbia" | County == "Greene" | County == "Dutchess" | County == "Putnam" | County == "Orange" | County == "
Washington" | County == "Warren" | County == "Schoharie" | County == "Montgomery" | County == "Fulton" | County == "Hamilton"))

# Move inches to end
scraper <- select(scraper, Date, Time, State, County, Location, Unknown, Unknown2, Latitude, Longitude, Precipitation, Method, Measurement, `Day reported`, Inches, Unit)
  
# Export ----

# Check authorized users and authorize account
gs4_auth()
1

# Export scraper data to Google Sheet
sheet_write(scraper, ss = "LINK GOES HERE", sheet = "scraper")

