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

# Create storm rainfall total dataset
rain_storm <- scraper %>% 
  filter(Measurement == "Storm Total Rainfall")

  
# Export ----

# Authorize
gs4_auth("alexandra.harris@timesunion.com")

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


# Schedule with Launchd (Mac) ----

# Save as .plist:
  # <?xml version="1.0" encoding="UTF-8"?>
  # <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  # <plist version="1.0">
  # <dict>
  # <key>Label</key>
  # <string>com.nws-snowfall-scraper</string>
  # <key>ProgramArguments</key>
  # <array>
  # <string>/usr/local/bin/Rscript</string>
  # <string>/Users/PATH HERE</string>
  # </array>
  # <key>StartInterval</key>
  # <integer>60</integer>
  # <key>KeepAlive</key>
  # <true/>
  # <key>PowerType</key>
  # <string>ACPower</string>
  # </dict>
  # </plist>

# Save to /Libary/LaunchDaemons

# Run in terminal:
  # sudo launchctl load /Library/LaunchDaemons/com.nws-snowfall-scraper.plist

# Check:
  # launchctl list
