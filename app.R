# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(jsonlite)
library(sf)
library(viridis)
library(plotly)

# Read your data
sorted_data <- read.csv("Final_Merged_For_Shiny_Geo.csv")


file_path <- "Team2_Final_SEW_Ordinal_Modelling1.csv"
model <- read.csv(file_path)


total_energy_data <- sorted_data %>%
  summarise(Current_Year = sum(out.total_energy_consumption),
            Next_Year = sum(Next_year_Pred))

# Calculate percentage increase for total energy
total_energy_data$Percentage_Increase <- ((total_energy_data$Next_Year - total_energy_data$Current_Year) / total_energy_data$Current_Year) * 100

# Reshape the data for ggplot
reshaped_data <- reshape2::melt(total_energy_data, id.vars = NULL,
                                measure.vars = c("Current_Year", "Next_Year", "Percentage_Increase"),
                                variable.name = "Energy_Type", value.name = "Value")

# Define UI for the Shiny app
ui <- fluidPage(
  
  navbarPage("eSC ~ Energy Consumption Comparison",
             
             # First tab for the comparison of current and future energy consumption
             tabPanel("Current vs. Future Energy",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("in_county", "Select County", unique(sorted_data$in.county)),
                          selectInput("time_split", "Select Time of Day", unique(sorted_data$time_split))
                        ),
                        mainPanel(
                          plotOutput("energyComparisonPlot")
                        )
                      )
             ),
             
             # Second tab for the comparison of total energy consumption
             tabPanel("Total Energy Comparison",
                      sidebarLayout(
                        sidebarPanel(
                          HTML("Explore the comparison of current and future energy consumption for the month of July in South Carolina. This section provides insights into the key drivers of energy usage, aiming to help an energy company (eSC) understand potential challenges in meeting electricity demand during hot summers. By analyzing historical and predicted energy data, users can gain valuable information to encourage energy-saving practices and contribute to both efficient energy usage and environmental sustainability.")
                        ),
                        mainPanel(
                          plotOutput("totalEnergyPlot")
                        )
                      )
             ),
             
             tabPanel("Energy Categories Distribution",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("in_county_pie", "Select County", unique(sorted_data$in.county)),
                          selectInput("time_split_pie", "Select Time of Day", unique(sorted_data$time_split), selected = unique(sorted_data$time_split)[1])
                        ),
                        mainPanel(
                          plotOutput("energyCategoryPieChart")
                        )
                      )
             ),
             
             tabPanel("Aggregated Panel",
                      sidebarLayout(
                        sidebarPanel(
                          HTML("Explore the analysis of sorted energy consumption data. This section provides insights into the aggregated energy demand based on the selected time and city."),
                          selectInput("new_in_city", "Select City", unique(sorted_data$in.weather_file_city)),
                          sliderInput("new_time_slider_city", "Select Time Range",
                                      min = min(sorted_data$time_split_numeric),
                                      max = max(sorted_data$time_split_numeric),
                                      value = c(min(sorted_data$time_split_numeric), max(sorted_data$time_split_numeric)),
                                      step = 1)
                        ),
                        mainPanel(
                          plotOutput("new_energyPlot")
                        )
                      )
             ),
             
             tabPanel("Leaflet Maps",
                      fluidRow(
                        tags$style(HTML(".leaflet-container { border: 2px solid black; margin-bottom: 10px; }")),
                        leafletOutput("map_current_leaflet"),
                        leafletOutput("map_future_leaflet")
                      )
             ),
             
             tabPanel("Energy Consumption Analysis",
                      plotOutput("plot_a"),
                      plotOutput("plot_b"),
                      plotOutput("plot_c"),
                      plotOutput("plot_d"))
  )
)




# Define server logic
server <- function(input, output) {
  
  output$energyComparisonPlot <- renderPlot({
    # Filter data based on user input
    filtered_data <- subset(sorted_data, in.county == input$in_county & time_split == input$time_split)
    
    # Check if the filtered data is not empty
    if (nrow(filtered_data) > 0) {
      # Summarize data for current year
      current_year_data <- filtered_data %>%
        group_by(time_split) %>%
        summarise(Current_Year = sum(out.total_energy_consumption))
      
      # Summarize data for the next year
      next_year_data <- filtered_data %>%
        group_by(time_split) %>%
        summarise(Next_Year = sum(Next_year_Pred))
      
      # Combine the two datasets
      combined_data <- merge(current_year_data, next_year_data, by = "time_split", all = TRUE)
      
      # Calculate percentage increase
      combined_data$Percentage_Increase <- ((combined_data$Next_Year - combined_data$Current_Year) / combined_data$Current_Year) * 100
      
      # Reshape the data for ggplot
      reshaped_data <- reshape2::melt(combined_data, id.vars = "time_split",
                                      measure.vars = c("Current_Year", "Next_Year", "Percentage_Increase"),
                                      variable.name = "Energy_Type", value.name = "Value")
      
      # Create a bar plot for current and future energy consumption
      ggplot(reshaped_data, aes(x = time_split, y = Value, fill = Energy_Type, label = ifelse(Energy_Type == "Percentage_Increase", paste(round(Value, 2), "%"), ""))) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        geom_text(position = position_dodge(width = 0.7), vjust = -0.5) +
        labs(title = "Comparison of Current and Future Energy Consumption",
             x = "Time of Day",
             y = "Energy Consumption",
             fill = "Energy Type") +
        theme_minimal() +
        theme(legend.position = "top") +
        scale_fill_manual(values = c("Current_Year" = "#66c2a5", "Next_Year" = "#fc8d62", "Percentage_Increase" = "#8da0cb"))
    } else {
      # If filtered data is empty, display a message or a default plot
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters.",
                 color = "red", size = 5, hjust = 0.5, vjust = 0.5)
    }
  })
  
  output$totalEnergyPlot <- renderPlot({
    # Filter data based on user input
    filtered_data <- subset(sorted_data)
    
    # Check if the filtered data is not empty
    if (nrow(filtered_data) > 0) {
      # Reshape the data for ggplot
      reshaped_data <- reshape2::melt(total_energy_data, id.vars = NULL,
                                      measure.vars = c("Current_Year", "Next_Year", "Percentage_Increase"),
                                      variable.name = "Energy_Type", value.name = "Value")
      
      # Create a line plot for total energy consumption
      p <- ggplot(reshaped_data, aes(x = Energy_Type, y = Value, color = Energy_Type, linetype = Energy_Type)) +
        geom_line() +
        geom_point() +
        labs(title = "Total Energy Consumption",
             x = NULL,
             y = "Total Energy Consumption") +
        theme_minimal() +
        theme(legend.position = "top") +
        scale_color_manual(values = c("Current_Year" = "#66c2a5", "Next_Year" = "#fc8d62", "Percentage_Increase" = "#8da0cb"))
      
      # Add labels outside the plot using annotate
      p <- p +
        annotate("text", x = 2, y = max(reshaped_data$Value) - 0.2, 
                 label = paste("Percentage Increase: ", round(total_energy_data$Percentage_Increase, 2), "%"),
                 color = "#8da0cb", size = 4, hjust = 0, vjust = -0.5)
      
      print(p)
    } else {
      # If filtered data is empty, display a message or a default plot
      ggplot() + theme_void() +
        annotate("text", x = 1, y = 1, label = "No data available for the selected filters.",
                 color = "red", size = 5, hjust = 0.5, vjust = 0.5)
    }
  })
  
  output$energyCategoryPieChart <- renderPlot({
    # Filter data based on user input
    filtered_data_pie <- subset(sorted_data, in.county == input$in_county_pie & time_split %in% input$time_split_pie)
    
    # Check if the filtered data is not empty
    if (nrow(filtered_data_pie) > 0) {
      # Summarize data for energy categories
      category_data <- filtered_data_pie %>%
        summarise(
          Kitchen = sum(out.kitchen_energy_consumption),
          Laundry = sum(out.laundry_energy_consumption),
          Heating_Cooling = sum(out.heating_cooling_energy_consumption),
          Water_Heating = sum(out.water_heating_energy_consumption),
          Electrical_Appliances = sum(out.electrical_appliances_energy_consumption),
          Outdoor_Appliances = sum(out.outdoor_appliances_energy_consumption),
          Renewable_Energy = sum(out.renewable_energy_energy_consumption),
          Total_Energy = sum(out.total_energy_consumption)
        )
      
      # Reshape the data for ggplot
      reshaped_category_data <- reshape2::melt(category_data, id.vars = "Total_Energy", variable.name = "Energy_Category", value.name = "Value")
      
      # Create a pie chart for energy categories
      ggplot(reshaped_category_data, aes(x = "", y = Value, fill = Energy_Category)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y") +
        labs(title = "Energy Categories Distribution",
             fill = "Energy Category") +
        theme_void() +
        theme(legend.position = "right") +
        scale_fill_manual(values = c(
          "Kitchen" = "#66c2a5",
          "Laundry" = "#fc8d62",
          "Heating_Cooling" = "#8da0cb",
          "Water_Heating" = "#e78ac3",
          "Electrical_Appliances" = "#a6d854",
          "Outdoor_Appliances" = "#ffd92f",
          "Renewable_Energy" = "#66c2a5"
        ))
    } else {
      # If filtered data is empty, display a message or a default plot
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters.",
                 color = "red", size = 5, hjust = 0.5, vjust = 0.5)
    }
  })
  
  
  output$new_energyPlot <- renderPlot({
    # Filter data based on user input
    new_filtered_data <- subset(sorted_data, 
                                in.weather_file_city == input$new_in_city & 
                                  between(time_split_numeric, input$new_time_slider_city[1], input$new_time_slider_city[2]))
    
    # Check if the filtered data is not empty
    if (nrow(new_filtered_data) > 0) {
      # Data aggregation
      new_grouped_data <- new_filtered_data %>%
        group_by(time_split, in.weather_file_city) %>%
        summarize(Total_Energy_Demand = sum(Next_year_Pred, na.rm = TRUE),
                  Avg_Latitude = mean(in.weather_file_latitude, na.rm = TRUE),
                  Avg_Longitude = mean(in.weather_file_longitude, na.rm = TRUE),
                  City_Name = first(in.weather_file_city[!is.na(in.weather_file_city)])) %>%
        ungroup()
      
      # Create a bar plot for total energy consumption with vibrant colors
      ggplot(new_grouped_data, aes(x = time_split, y = Total_Energy_Demand, fill = in.weather_file_city)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        labs(title = "Aggregated Energy Demand",
             x = "Time of Day",
             y = "Total Energy Demand",
             fill = "City") +
        theme_minimal() +
        theme(legend.position = "top") +
        scale_fill_viridis_d(option = "A", direction = -1)  # Adjust the option and direction as needed
    } else {
      # If filtered data is empty, display a message or a default plot
      ggplot() + theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters.",
                 color = "red", size = 5, hjust = 0.5, vjust = 0.5)
    }
  })
  
  
  
  
  
  # Your data processing code here (Summarize, Join, Create Palette, etc.)
  # Create a palette for time zones
  zone_palette <- c("blue", "green", "yellow", "orange", "red", "purple")  # Corresponding to 1-6
  summary_data<- sorted_data %>%
    group_by(bldg_id) %>%
    summarize(max_time_zone = time_split_numeric[which.max(out.total_energy_consumption)])
  
  # Join summary data with the original dataset to retain all rows for each bldg_id
  sorted_data1 <- left_join(sorted_data, summary_data, by = "bldg_id")
  
  # Render Leaflet map for current year consumption
  output$map_current <- renderLeaflet({
    leaflet(data = sorted_data1) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~in.weather_file_latitude,
        lng = ~in.weather_file_longitude,
        popup = ~paste("House Number: ", bldg_id, "<br>",
                       "Time Zone: ", max_time_zone, "<br>",
                       "Current Year Consumption: ", out.total_energy_consumption, "kWh"),
        color = ~zone_palette[as.integer(max_time_zone)],
        fillOpacity = 0.7
      ) %>%
      addLegend(
        position = "bottomright",
        colors = zone_palette,
        labels = c("Late Night", "Early Morning", "Morning", "Noon", "Evening", "Night"),
        title = "Time Zones"
      )
  })
  
  # Render Leaflet map for future consumption predictions
  output$map_future <- renderLeaflet({
    leaflet(data = sorted_data1) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~in.weather_file_latitude,
        lng = ~in.weather_file_longitude,
        popup = ~paste("House Number: ", bldg_id, "<br>",
                       "Time Zone: ", max_time_zone, "<br>",
                       "Future Consumption Prediction: ", Next_year_Pred, "kWh"),
        color = ~zone_palette[as.integer(max_time_zone)],
        fillOpacity = 0.7
      ) %>%
      addLegend(
        position = "bottomright",
        colors = zone_palette,
        labels = c("Late Night", "Early Morning", "Morning", "Noon", "Evening", "Night"),
        title = "Time Zones"
      )
  })
  
  # ... (previous code)
  
  # New Leaflet map for current year consumption in the fourth tabPanel
  output$map_current_leaflet <- renderLeaflet({
    leaflet(data = sorted_data1) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~in.weather_file_latitude,
        lng = ~in.weather_file_longitude,
        popup = ~paste("House Number: ", bldg_id, "<br>",
                       "Time Zone: ", max_time_zone, "<br>",
                       "Current Year Consumption: ", out.total_energy_consumption, "kWh"),
        color = ~zone_palette[as.integer(max_time_zone)],
        fillOpacity = 0.7
      ) %>%
      addLegend(
        position = "bottomright",
        colors = zone_palette,
        labels = c("Late Night", "Early Morning", "Morning", "Noon", "Evening", "Night"),
        title = "Time Zones"
      ) %>%
      addControl(
        html = '<div style="position: absolute; top: 10px; right: 10px; background: white; padding: 5px; border: 1px solid gray; border-radius: 5px;">Current Year Consumption</div>',
        position = "topright",
        layerId = "current_info"
      )
  })
  
  # New Leaflet map for future consumption predictions in the fourth tabPanel
  output$map_future_leaflet <- renderLeaflet({
    leaflet(data = sorted_data1) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~in.weather_file_latitude,
        lng = ~in.weather_file_longitude,
        popup = ~paste("House Number: ", bldg_id, "<br>",
                       "Time Zone: ", max_time_zone, "<br>",
                       "Future Consumption Prediction: ", Next_year_Pred, "kWh"),
        color = ~zone_palette[as.integer(max_time_zone)],
        fillOpacity = 0.7
      ) %>%
      addLegend(
        position = "bottomright",
        colors = zone_palette,
        labels = c("Late Night", "Early Morning", "Morning", "Noon", "Evening", "Night"),
        title = "Time Zones"
      ) %>%
      addControl(
        html = '<div style="position: absolute; top: 10px; right: 10px; background: white; padding: 5px; border: 1px solid gray; border-radius: 5px;">Future Expected Consumption</div>',
        position = "topright",
        layerId = "future_info"
      )
  })
  
  # Plot A: Total Energy Consumption by Income Category
  output$plot_a <- renderPlot({
    total_energy_consumption <- model %>%
      mutate(income_category = case_when(
        in.income <= 39999 ~ "Low",
        in.income > 39999 & in.income <= 99999 ~ "Middle",
        in.income > 99999 ~ "High"
      )) %>%
      group_by(income_category) %>%
      summarize(TotalEnergyConsumption = sum(out.total_energy_consumption, na.rm = TRUE))
    
    total_energy_consumption$income_category <- factor(total_energy_consumption$income_category, levels = c("Low", "Middle", "High"))
    
    ggplot(total_energy_consumption, aes(x = TotalEnergyConsumption, y = income_category, fill = income_category)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Energy Consumption by Income Category",
           x = "Total Energy Consumption",
           y = "Income Category") +
      theme_minimal() +
      scale_fill_manual(values = c("#A6CEE3", "#FFD700", "#98FB98"))  # Customize pastel color values
  })
  
  # Plot B: Total Energy Consumption by Square Feet Category
  output$plot_b <- renderPlot({
    quantiles <- quantile(model$in.sqft, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE)
    
    model$sqft_category <- cut(model$in.sqft,
                               breaks = quantiles,
                               labels = c("Low", "Medium", "High"),
                               include.lowest = TRUE)
    
    total_energy_by_sqft <- model %>%
      group_by(sqft_category) %>%
      summarize(TotalEnergy = sum(out.total_energy_consumption, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(total_energy_by_sqft, aes(x = TotalEnergy, y = sqft_category, fill = sqft_category)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Energy Consumption by Square Feet Category",
           x = "Total Energy Consumption",
           y = "Sqft Category") +
      theme_minimal() +
      scale_fill_manual(values = c("#FFB6C1", "#87CEEB", "#98FB98"))  # Customize pastel color values
  })

  
  # Encoding in.federal_poverty_level
  model$encoded_poverty_level <- case_when(
    model$in.federal_poverty_level %in% c(1, 2, 3, 4, 5) ~ as.character(model$in.federal_poverty_level),
    TRUE ~ "Other"
  )
  
  # Encoding in.heating_fuel
  model$encoded_heating_fuel <- case_when(
    model$in.heating_fuel %in% c(1, 2, 3, 4, 5) ~ as.character(model$in.heating_fuel),
    TRUE ~ "Other"
  )
  
  # Debug Print Statements
  cat("Unique Values in in.federal_poverty_level:", unique(model$in.federal_poverty_level), "\n")
  cat("Unique Values in encoded_poverty_level:", unique(model$encoded_poverty_level), "\n")
  cat("Unique Values in in.heating_fuel:", unique(model$in.heating_fuel), "\n")
  cat("Unique Values in encoded_heating_fuel:", unique(model$encoded_heating_fuel), "\n")
  
  
  # Plot C: Total Energy Consumption for Different Federal Poverty Level
  output$plot_c <- renderPlot({
    total_energy_bypoverty <- model %>%
      group_by(encoded_poverty_level) %>%
      summarize(TotalEnergyConsumption = sum(out.total_energy_consumption, na.rm = TRUE))
    
    ggplot(total_energy_bypoverty, aes(x = TotalEnergyConsumption, y = reorder(encoded_poverty_level, -TotalEnergyConsumption), fill = encoded_poverty_level)) +
      geom_col() +
      labs(title = "Total Energy Consumption for Different Federal Poverty Level",
           x = "Total Energy Consumption",
           y = "Federal Poverty Level") +
      theme_minimal() +
      scale_fill_manual(values = c("#FFDAB9", "#87CEEB", "#98FB98", "#FF69B4", "#FFA07A", "#FF6347")) +  # Customize pastel color values
      annotate("text", x = -Inf, y = Inf, label = "Federal Poverty Level Encoding:", vjust = 2, hjust = 0, size = 4) +
      annotate("text", x = -Inf, y = Inf, label = "1: 0-100%, 2: 100-150%, 3: 150-200%, 4: 200-300%, 5: 300-400%, Other: Other", vjust = 1, hjust = 0, size = 3)
  })
  
  # Plot D: Total Energy Consumption for Different Fuel Type
  output$plot_d <- renderPlot({
    total_energy_by_Heatingfuel <- model %>%
      group_by(encoded_heating_fuel) %>%
      summarize(TotalEnergyConsumption = sum(out.total_energy_consumption, na.rm = TRUE))
    
    ggplot(total_energy_by_Heatingfuel, aes(x = TotalEnergyConsumption, y = reorder(encoded_heating_fuel, -TotalEnergyConsumption), fill = encoded_heating_fuel)) +
      geom_col() +
      labs(title = "Total Energy Consumption for Different Fuel Type",
           x = "Total Energy Consumption",
           y = "Fuel Type") +
      theme_minimal() +
      scale_fill_manual(values = c("#FFDAB9", "#87CEEB", "#98FB98", "#FF69B4", "#FFA07A", "#FF6347")) +  # Include color for "Other"
      annotate("text", x = -Inf, y = Inf, label = "Fuel Type Encoding:", vjust = 2, hjust = 0, size = 4) +
      annotate("text", x = -Inf, y = Inf, label = "1: Electricity, 2: Fuel Oil, 3: Natural Gas, 4: Other Fuel, 5: Propane, Other: Other", vjust = 1, hjust = 0, size = 3)
  })
  
  
  
  # Arrange plots in a 2x2 grid side by side
  output$plots_2x2_grid <- renderPlotly({
    subplot(
      plotly::plotlyOutput("plot_a"),
      plotly::plotlyOutput("plot_b"),
      plotly::plotlyOutput("plot_c"),
      plotly::plotlyOutput("plot_d"),
      nrows = 2, margin = 0.05
    )
  })
  





}
# Run the Shiny app
shinyApp(ui = ui, server = server)
