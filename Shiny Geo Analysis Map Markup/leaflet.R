# Merging columns for the map.
library(tidyverse)
thisdf <- read_csv("C:/Users/rithv/Downloads/converted1.csv")
second_parts <- str_split(thisdf$in.county_and_puma, ", ", simplify = TRUE)[, 2]
newsecond_parts <- unique(second_parts)
newcombined_county_puma <- unique(thisdf$in.county_and_puma)

excel_data <- read_excel("C:/Users/rithv/Downloads/exceldata1.xls")
merged_data <- inner_join(thisdf, excel_data, by = c("in.county" = "countyno"))
excel_data1 <- read_excel("C:/Users/rithv/Downloads/uscounties.xlsx")
merged_data1 <- inner_join(merged_data, excel_data1, by = c("`County Name` = county"))
merged_data1 <- inner_join(merged_data, excel_data1, by = c("County Name" = "county"))
result2 <- merge(merged_data, aggregated_excel_data, by.x = "County Name", by.y = "county", all.x = TRUE)

aggregated_excel_data <- excel_data1 %>% 
  group_by(county) %>%   # Assuming 'County' is the column with county names
  summarise(state_name = first(state_name))  # Assuming 'State' is the column with state names
result2$county_name[your_dataframe$county_id == "G4500650"] <- "McCormick"

setwd("C:/Users/rithv/Documents/IDS Project")
write.csv(result2, file ="mergedstatescounties.csv",row.names = FALSE)
result2$state_name[result2$in.county == "G4500650"] <- "South Carolina"

result4 <- read.csv("C:/Users/rithv/Downloads/Final_Merged_For_Shiny_Geo.csv")



library(dplyr)
library(leaflet)

# Summarize data to find the time zone with maximum consumption for each house
summary_data <- result4 %>%
  group_by(bldg_id) %>%
  summarize(max_time_zone = time_split_numeric[which.max(out.total_energy_consumption)])

# Join summary data with the original dataset to retain all rows for each bldg_id
merged_data <- left_join(result4, summary_data, by = "bldg_id")

# Create a palette for time zones
zone_palette <- c("blue", "green", "yellow", "orange", "red", "purple")  # Corresponding to 1-6

# Create leaflet map
m <- leaflet(data = merged_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lat = ~in.weather_file_latitude,
    lng = ~in.weather_file_longitude,
    popup = ~paste("House Number: ", bldg_id, "<br>",
                   "Time Zone: ", max_time_zone, "<br>",
                   "Total Consumption: ", out.total_energy_consumption, "kWh"),
    color = ~zone_palette[as.integer(max_time_zone)],
    fillOpacity = 0.7
  ) %>%
  addLegend(
    position = "bottomright",
    colors = zone_palette,
    labels = c("Late Night", "Early Morning", "Morning", "Noon", "Evening", "Night"),  # Corresponding to 1-6
    title = "Time Zones"
  )

# Display the map
m