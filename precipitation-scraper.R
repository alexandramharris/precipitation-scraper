# Precipitation Scraper ----
# Alexandra Harris

# Source: https://forecast.weather.gov/product.php?site=ALY&issuedby=ALY&product=PNS&format=ci


# Set up ----
library(tidyverse)
library(rvest)
library(stringr)
library(tidyr)
library(googlesheets4)
library(lubridate)
library(tokencodr)


# New York version ----

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
colnames(scraper)[6] = "Location 2"
colnames(scraper)[7] = "Location 3"
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
scraper$Time <- sub("^([0-9]{3}) ", "0\\1 ", scraper$Time)
scraper$Time <- strptime(scraper$Time, format = "%I%M %p")
scraper$Time <- strftime(scraper$Time, format = "%I:%M %p")
scraper$Time <- sub("^0", "", scraper$Time)
scraper$Time <- gsub("AM", "a.m.", scraper$Time)
scraper$Time <- gsub("PM", "p.m.", scraper$Time)

# Concatenate day and time
scraper$Reported <- paste(scraper$`Day reported`, scraper$Time)

# Format daily dates and times
scraper$`Daily date` <- format(as.Date(scraper$Date, "%Y-%m-%d"), "%b. %d")
scraper$`Date reported` <- paste(scraper$`Daily date`, "at", scraper$Time)

# Filter for counties
scraper <- scraper %>% 
  filter(!is.na(County) & (County == "Albany" | County == "Saratoga" | County == "Schenectady" | County == "Rensselaer" | County == "Greene" | County == "Ulster" | County == "Delaware" | County == "Sullivan" | County == "Columbia" | County == "Greene" | County == "Dutchess" | County == "Putnam" | County == "Orange" | County == "Washington" | County == "Warren" | County == "Schoharie" | County == "Montgomery" | County == "Fulton" | County == "Hamilton"))

# Move inches to end
scraper <- select(scraper, Date, Time, State, County, Location, `Location 2`, `Location 3`, Latitude, Longitude, Precipitation, Method, Measurement, `Day reported`, Reported,`Daily date`, `Date reported`, Inches, Unit)

# Trim numbers after location name
scraper$Location <- sub("\\d.*", "", scraper$Location)

# Create daily dataset
snow_daily <- scraper %>% 
  filter(Measurement == "24-hourly Snowfall")

# Create storm total dataset
snow_storm <- scraper %>% 
  filter(Measurement == "Storm Total Snow")

# Create daily rainfall dataset
rain_daily <- scraper %>% 
  filter(Measurement == "24-hourly Rainfall")

# Create 12-hour rainfall dataset
rain_12 <- scraper %>% 
  filter(Measurement == "12-hourly Rainfall")

# Create storm rainfall total dataset
rain_storm <- scraper %>% 
  filter(Measurement == "Storm Total Rainfall")

# Create peak wind gust dataset
peak_wind_gust <- scraper %>% 
  filter(Measurement == "Peak Wind Gust")

# Rename inches to Number
colnames(peak_wind_gust)[17] = "Number"

# Create ice storm accrual dataset
ice_storm <- scraper %>% 
  filter(Measurement == "Storm Total Ice Accrual")

# Rename T to Trace
ice_storm$Inches[ice_storm$Inches == "T"] <- "Trace"

# Create a value for trace to work on legend
ice_storm <- ice_storm %>% 
  mutate(Legend = ifelse(Inches == "Trace", 0.01, Inches))


# Export ----

# Authorize locally
# gs4_auth("alexandra.harris@timesunion.com")

# Authorize for actions
source("functions/func_auth_google.R")          
auth_google(email = "alexandra.harris@timesunion.com",
            service = "gsheet_precipitation",
            token_path = ".secret/gsheet_precipitation")

# Check if blank
if (nrow(snow_storm)) {
# Export snow storm data to Google Sheet
  sheet_write(snow_storm, ss = "https://docs.google.com/spreadsheets/d/1zjPTqwk-SM18CNr0JbEKVj8Ez6ZnifRpXUy8DYAvuPM/edit#gid=1787663814", sheet = "snow_storm")
  print("Exported snow storm data")
} else {
  print("Snow storm data is blank")
}

# Check if blank
if (nrow(snow_daily)) {
# Export daily snow data to Google Sheet
  sheet_write(snow_daily, ss = "https://docs.google.com/spreadsheets/d/1zjPTqwk-SM18CNr0JbEKVj8Ez6ZnifRpXUy8DYAvuPM/edit#gid=1787663814", sheet = "snow_daily")
  print("Exported daily snow data")
} else {
  print("Daily snow data is blank")
}

# Check if blank
if (nrow(rain_storm)) {
# Export rain storm data to Google Sheet
  sheet_write(rain_storm, ss = "https://docs.google.com/spreadsheets/d/1zjPTqwk-SM18CNr0JbEKVj8Ez6ZnifRpXUy8DYAvuPM/edit#gid=1787663814", sheet = "rain_storm")
  print("Exported rain storm data")
} else {
  print("Rain storm data is blank")
}

# Check if blank
if (nrow(rain_daily)) {
# Export daily rain data to Google Sheet
  sheet_write(rain_daily, ss = "https://docs.google.com/spreadsheets/d/1zjPTqwk-SM18CNr0JbEKVj8Ez6ZnifRpXUy8DYAvuPM/edit#gid=1787663814", sheet = "rain_daily")
  print("Exported daily rain data")
} else {
  print("Daily rain data is blank")
}

# Check if blank
if (nrow(rain_12)) {
  # Export daily rain data to Google Sheet
  sheet_write(rain_12, ss = "https://docs.google.com/spreadsheets/d/1zjPTqwk-SM18CNr0JbEKVj8Ez6ZnifRpXUy8DYAvuPM/edit#gid=1787663814", sheet = "rain_12")
  print("Exported 12-hour rain data")
} else {
  print("12-hour rain data is blank")
}

# Check if blank
if (nrow(peak_wind_gust)) {
  # Export rain storm data to Google Sheet
  sheet_write(peak_wind_gust, ss = "https://docs.google.com/spreadsheets/d/1zjPTqwk-SM18CNr0JbEKVj8Ez6ZnifRpXUy8DYAvuPM/edit#gid=1787663814", sheet = "peak_wind_gust")
  print("Exported peak wind gust data")
} else {
  print("Peak wind gust data is blank")
}

# Check if blank
if (nrow(ice_storm)) {
  # Export ice storm data to Google Sheet
  sheet_write(ice_storm, ss = "https://docs.google.com/spreadsheets/d/1zjPTqwk-SM18CNr0JbEKVj8Ez6ZnifRpXUy8DYAvuPM/edit#gid=1787663814", sheet = "ice_storm")
  print("Exported ice storm data")
} else {
  print("Ice storm data is blank")
}


# Connecticut version ----

# Sources:

# https://forecast.weather.gov/product.php?site=NWS&issuedby=ALY&product=PNS&format=ci&version=1&glossary=1&highlight=off
# https://forecast.weather.gov/product.php?site=NWS&issuedby=OKX&product=PNS&format=ci&version=1&glossary=1&highlight=off
# https://forecast.weather.gov/product.php?site=NWS&issuedby=BOX&product=PNS&format=ci&version=1&glossary=1&highlight=off


# Scraper ----

# Set source links
links <- c("https://forecast.weather.gov/product.php?site=NWS&issuedby=ALY&product=PNS&format=CI&version=1&glossary=1&highlight=off", 
           "https://forecast.weather.gov/product.php?site=NWS&issuedby=OKX&product=PNS&format=ci&version=1&glossary=1&highlight=off", 
           "https://forecast.weather.gov/product.php?site=NWS&issuedby=BOX&product=PNS&format=ci&version=1&glossary=1&highlight=off")

# Initialize dataframes
ct_snow_daily <- data.frame()
ct_snow_storm <- data.frame()
ct_rain_daily <- data.frame()
ct_rain_storm <- data.frame()

# Iterate over source links
for(i in 1:length(links)){
   # Read webpage
  ct_webpage <- read_html(links[i])
  
  # Select element
  ct_metadata_nodes <- html_nodes(ct_webpage, "pre.glossaryProduct")
  
  # Extract text from href -- we just need to add this
  ct_text <- html_text(ct_metadata_nodes)
  
  # Trim text
  ct_metadata_text <- str_trim(ct_text)
  
  # Convert to dataframe
  ct_metadata_df <- data.frame(ct_metadata_text)
  
  # Extract below metadata text only
  ct_metadata <- as.data.frame(gsub(".*\\*\\*\\*\\*\\*METADATA\\*\\*\\*\\*\\*", "", ct_metadata_df$ct_metadata_text))
  
  # Rename 
  colnames(ct_metadata)[1] = "col"
  
  # Separate
  ct_scraper <- ct_metadata %>% 
    separate_rows(col, sep=":") %>% 
    separate(col, paste('col', 1:14, sep=":"), sep=",", extra="drop")
  
  # Drop blank rows
  ct_scraper <- ct_scraper %>% drop_na
  
  # Clean white space
  ct_scraper <- ct_scraper %>% mutate_all(funs(trimws))
  
  # Rename columns
  colnames(ct_scraper)[1] = "Date"
  colnames(ct_scraper)[2] = "Time"
  colnames(ct_scraper)[3] = "State"
  colnames(ct_scraper)[4] = "County"
  colnames(ct_scraper)[5] = "Location"
  colnames(ct_scraper)[6] = "Location 2"
  colnames(ct_scraper)[7] = "Location 3"
  colnames(ct_scraper)[8] = "Latitude"
  colnames(ct_scraper)[9] = "Longitude"
  colnames(ct_scraper)[10] = "Precipitation"
  colnames(ct_scraper)[11] = "Inches"
  colnames(ct_scraper)[12] = "Unit"
  colnames(ct_scraper)[13] = "Method"
  colnames(ct_scraper)[14] = "Measurement"
  
  # Format date
  ct_scraper$Date <- as.Date(ct_scraper$Date , format = "%m/%d/%Y")
  
  # Add day of week
  ct_scraper$`Day reported` <- weekdays(ct_scraper$Date) 
  
  # Format time
  ct_scraper$Time <- sub("^([0-9]{3}) ", "0\\1 ", ct_scraper$Time)
  ct_scraper$Time <- strptime(ct_scraper$Time, format = "%I%M %p")
  ct_scraper$Time <- strftime(ct_scraper$Time, format = "%I:%M %p")
  ct_scraper$Time <- sub("^0", "", ct_scraper$Time)
  ct_scraper$Time <- gsub("AM", "a.m.", ct_scraper$Time)
  ct_scraper$Time <- gsub("PM", "p.m.", ct_scraper$Time)
  
  # Concatenate day and time
  ct_scraper$Reported <- paste(ct_scraper$`Day reported`, ct_scraper$Time)
  
  # Format daily dates and times
  ct_scraper$`Daily date` <- format(as.Date(ct_scraper$Date, "%Y-%m-%d"), "%b. %d")
  ct_scraper$`Date reported` <- paste(ct_scraper$`Daily date`, "at", ct_scraper$Time)
  
  # Filter for state
  ct_scraper <- ct_scraper %>% 
    filter(!is.na(State) & (State == "CT"))
  
  # Move inches to end
  ct_scraper <- select(ct_scraper, Date, Time, State, County, Location, `Location 2`, `Location 3`, Latitude, Longitude, Precipitation, Method, Measurement, `Day reported`, Reported,`Daily date`, `Date reported`, Inches, Unit)
  
  # Trim numbers before & after location name
  ct_scraper$Location <- sub("^[0-9.]+ [NESW]+ ", "", ct_scraper$Location)
  ct_scraper$Location <- sub(" [0-9.]+ [NESW]+$", "", ct_scraper$Location)
  
  # Create daily dataset
  ct_snow_daily_temp <- ct_scraper %>% 
    filter(Measurement == "24-hourly Snowfall" | Measurement == "24 hour snowfall")
  
  # Create storm total dataset
  ct_snow_storm_temp <- ct_scraper %>% 
    filter(Measurement == "Storm Total Snow" | Measurement == "STORM TOTAL SNOWFALL" | Measurement == "Storm total snowfall")
  
  # Create daily rainfall dataset
  ct_rain_daily_temp <- ct_scraper %>% 
    filter(Measurement == "24-hourly Rainfall" | Measurement == "24 hour rainfall")
  
  # Create storm rainfall dataset
  ct_rain_storm_temp <- ct_scraper %>% 
    filter(Measurement == "Storm Total Rainfall")
  
  # Combine data
  ct_snow_daily <- rbind(ct_snow_daily, ct_snow_daily_temp)
  ct_snow_storm <- rbind(ct_snow_storm, ct_snow_storm_temp)
  ct_rain_daily <- rbind(ct_rain_daily, ct_rain_daily_temp)
  ct_rain_storm <- rbind(ct_rain_storm, ct_rain_storm_temp)
}


# Export ----

# Check if blank
if (nrow(ct_snow_storm)) {
# Export snow storm data to Google Sheet
  sheet_write(ct_snow_storm, ss = "https://docs.google.com/spreadsheets/d/1YDLQYb19d8jmqnuGx6NdWVh4h5JgolmKFBsEKwGulPM/edit?usp=sharing", sheet = "snow_storm")
  print("Exported snow storm data")
} else {
  print("Snow storm data is blank")
}

# Check if blank
if (nrow(ct_snow_daily)) {
# Export daily snow data to Google Sheet
  sheet_write(ct_snow_daily, ss = "https://docs.google.com/spreadsheets/d/1YDLQYb19d8jmqnuGx6NdWVh4h5JgolmKFBsEKwGulPM/edit?usp=sharing", sheet = "snow_daily")
  print("Exported daily snow data")
} else {
  print("Daily snow data is blank")
}

# Check if blank
if (nrow(ct_rain_storm)) {
# Export rain storm data to Google Sheet
  sheet_write(ct_rain_storm, ss = "https://docs.google.com/spreadsheets/d/1YDLQYb19d8jmqnuGx6NdWVh4h5JgolmKFBsEKwGulPM/edit?usp=sharing", sheet = "rain_storm")
  print("Exported rain storm data")
} else {
  print("Rain storm data is blank")
}

# Check if blank
if (nrow(ct_rain_daily)) {
# Export daily rain data to Google Sheet
  sheet_write(ct_rain_daily, ss = "https://docs.google.com/spreadsheets/d/1YDLQYb19d8jmqnuGx6NdWVh4h5JgolmKFBsEKwGulPM/edit?usp=sharing", sheet = "rain_daily")
  print("Exported daily rain data")
} else {
  print("Daily rain data is blank")
}
