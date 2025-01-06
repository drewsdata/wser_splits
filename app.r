# Load required libraries
library(hms)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(janitor)
library(here)

wser_splits <- read_csv(here("data","wser_split_data_2017_2024.csv")) %>% 
  clean_names()

# UI Definition
ui <- fluidPage(
  titlePanel("WSER 100 Results Analysis"),
  
  tabsetPanel(
    # Finish Time Distribution Tab
    tabPanel("Finish Time Distribution",
             sidebarLayout(
               sidebarPanel(
                 # Year range selector
                 tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                 sliderInput("year_range",
                             "Select Years:",
                             min = min(wser_splits$year),
                             max = max(wser_splits$year),
                             value = c(min(wser_splits$year), max(wser_splits$year)),
                             step = 1,
                             sep = ""),
                 
                 # Create a flex container for gender and result type
                 div(style = "display: flex; gap: 20px;",
                     # Gender selector
                     div(style = "flex: 1;",
                         radioButtons("gender",
                                      "Select Gender:",
                                      choices = list("All" = "All",
                                                     "Female" = "F",
                                                     "Male" = "M"),
                                      selected = "All")
                     ),
                     # Result selector (replaced buckle type)
                     div(style = "flex: 1;",
                         radioButtons("result",
                                      "Select Result:",
                                      choices = list(
                                        "All finishes" = "all_finishes",
                                        "Bronze Buckle (24 to 30 hours)" = "bronze",
                                        "Silver Buckle (sub 24 hours)" = "silver",
                                        "DNF" = "dnf"
                                      ),
                                      selected = "all_finishes")
                     )
                 ),
                 
                 # Age range selector
                 sliderInput("age_range",
                             "Age Range:",
                             min = min(wser_splits$age, na.rm = TRUE),
                             max = max(wser_splits$age, na.rm = TRUE),
                             value = c(min(wser_splits$age, na.rm = TRUE), max(wser_splits$age, na.rm = TRUE)))
               ),
               mainPanel(
                 plotlyOutput("finish_dist_plot"),
                 DTOutput("finish_summary_table")
               )
             )
    ),
    
    # Checkpoint Analysis Tab
    tabPanel("Checkpoint Analysis",
             sidebarLayout(
               sidebarPanel(
                 # Year range selector (unchanged)
                 tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                 sliderInput("year_range_checkpoint",
                             "Select Years:",
                             min = min(wser_splits$year),
                             max = max(wser_splits$year),
                             value = c(min(wser_splits$year), max(wser_splits$year)),
                             step = 1,
                             sep = ""),
                 
                 # Create a flex container for gender and result type
                 div(style = "display: flex; gap: 20px;",
                     # Gender selector
                     div(style = "flex: 1;",
                         radioButtons("gender_checkpoint",
                                      "Select Gender:",
                                      choices = list("All" = "All",
                                                     "Female" = "F",
                                                     "Male" = "M"),
                                      selected = "All")
                     ),
                     # Result selector
                     div(style = "flex: 1;",
                         radioButtons("result_checkpoint",
                                      "Select Result:",
                                      choices = list(
                                        "All finishes" = "all_finishes",
                                        "Bronze Buckle (24 to 30 hours)" = "bronze",
                                        "Silver Buckle (sub 24 hours)" = "silver",
                                        "DNF" = "dnf"
                                      ),
                                      selected = "all_finishes")
                     )
                 ),
                 
                 # Age range selector
                 sliderInput("age_range_checkpoint",
                             "Age Range:",
                             min = min(wser_splits$age, na.rm = TRUE),
                             max = max(wser_splits$age, na.rm = TRUE),
                             value = c(min(wser_splits$age, na.rm = TRUE), max(wser_splits$age, na.rm = TRUE))),
                 
                 # Checkpoint selector (unchanged)
                 selectInput("checkpoint",
                             "Select Checkpoint for Analysis:",
                             choices = c("Lyon Ridge (10.3M : 16.6K)" = "lyon_ridge",
                                         "Red Star Ridge  (15.8M : 25.4K)" =  "red_star_ridge",
                                         "Duncan Canyon  (24.4M : 39.3K)" =  "duncan_canyon",
                                         "Robinson Flat  (30.3M : 48.8K)" =  "robinson_flat",
                                         "Miller's Defeat  (34.4M : 55.4K)" =  "millers_defeat",
                                         "Dusty Corners  (38M : 61.2K)" =  "dusty_corners",
                                         "Last Chance  (43.3M : 69.7K)" =  "last_chance",
                                         "Devil's Thumb  (47.8M : 76.9K)" =  "devils_thumb",
                                         "El Dorado  (52.9M : 85.1K)" =  "el_dorado",
                                         "Michigan Bluff  (55.7M : 89.6K)" =  "michigan_bluff",
                                         "Foresthill  (62M : 99.8K)" =  "foresthill",
                                         "Peachstone (Cal-2)  (70.7M : 113.8K)" =  "peachstone_cal_2",
                                         "Rucky Chucky  (78M : 125.5K)" =  "rucky_chucky",
                                         "Green Gate  (79.8M : 128.4K)" =  "green_gate",
                                         "Auburn Lake Trails  (85.2M : 137.1K)" =  "auburn_lake_trails",
                                         "Quarry Road  (90.7M : 146K)" =  "quarry_road",
                                         "Pointed Rocks  (94.3M : 151.8K)" =  "pointed_rocks",
                                         "Robie Point  (98.9M : 159.2K)" =  "robie_point",
                                         "Finish (Placer H.S. Track)  (100.2M : 161.3K)" =  "finish"))
               ),
               mainPanel(
                 plotlyOutput("checkpoint_plot"),
                 DTOutput("checkpoint_summary_table")
               )
             )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  convert_to_hours <- function(time_str) {
    if (is.character(time_str)) {
      # Vectorized handling of DNF values
      result <- numeric(length(time_str))
      result[time_str == "dnf"] <- NA
      
      # Process non-DNF values
      non_dnf <- time_str != "dnf"
      if (any(non_dnf)) {
        parts <- strsplit(time_str[non_dnf], ":")
        hours <- sapply(parts, function(x) {
          if (length(x) == 3) {
            return(as.numeric(x[1]) + as.numeric(x[2])/60 + as.numeric(x[3])/3600)
          } else if (length(x) == 2) {
            return(as.numeric(x[1]) + as.numeric(x[2])/60)
          } else {
            return(NA)
          }
        })
        result[non_dnf] <- hours
      }
      return(result)
    }
    return(time_str)
  }
  
  # Reactive filtered dataset for Finish Time Distribution
  filtered_wser_splits <- reactive({
    # Initial filtering without result type
    df <- wser_splits %>%
      mutate(
        time_hours = convert_to_hours(time),
        result_type = case_when(
          time == "dnf" ~ "dnf",
          !is.na(convert_to_hours(time)) & convert_to_hours(time) < 24 ~ "silver",
          !is.na(convert_to_hours(time)) & convert_to_hours(time) >= 24 & convert_to_hours(time) <= 30 ~ "bronze",
          TRUE ~ "other"
        ),
        across(ends_with("_time"), convert_to_hours)
      ) %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2],
             age >= input$age_range[1],
             age <= input$age_range[2])
    
    # Apply gender filter
    if (input$gender != "All") {
      df <- df %>% filter(gender == input$gender)
    }
    
    # Filter based on selected result
    filtered_df <- switch(input$result,
                          "all_finishes" = df %>% filter(result_type %in% c("silver", "bronze")),
                          "silver" = df %>% filter(result_type == "silver"),
                          "bronze" = df %>% filter(result_type == "bronze"),
                          "dnf" = df %>% filter(result_type == "dnf"),
                          df  # default case
    )
    
    return(filtered_df)
  })
  
  # Reactive filtered dataset for Checkpoint Analysis
  filtered_wser_splits_checkpoint <- reactive({
    df <- wser_splits %>%
      mutate(
        time_hours = convert_to_hours(time),
        across(ends_with("_time"), convert_to_hours),
        result_type = case_when(
          time == "dnf" ~ "dnf",
          !is.na(convert_to_hours(time)) & convert_to_hours(time) < 24 ~ "silver",
          !is.na(convert_to_hours(time)) & convert_to_hours(time) >= 24 & convert_to_hours(time) <= 30 ~ "bronze",
          TRUE ~ "other"
        )
      ) %>%
      filter(year >= input$year_range_checkpoint[1],
             year <= input$year_range_checkpoint[2],
             age >= input$age_range_checkpoint[1],
             age <= input$age_range_checkpoint[2])
    
    # Apply gender filter
    if (input$gender_checkpoint != "All") {
      df <- df %>% filter(gender == input$gender_checkpoint)
    }
    
    # Filter based on selected result
    filtered_df <- switch(input$result_checkpoint,
                          "all_finishes" = df %>% filter(result_type %in% c("silver", "bronze")),
                          "silver" = df %>% filter(result_type == "silver"),
                          "bronze" = df %>% filter(result_type == "bronze"),
                          "dnf" = df %>% filter(result_type == "dnf"),
                          df  # default case
    )
    
    return(filtered_df)
  })
  
  # Reactive filtered dataset for Position Changes
  filtered_wser_splits_position <- reactive({
    wser_splits %>%
      mutate(
        time_hours = convert_to_hours(time),
        across(ends_with("_time"), convert_to_hours)
      ) %>%
      filter(year >= input$year_range_position[1],
             year <= input$year_range_position[2],
             gender %in% input$gender_position,
             age >= input$age_range_position[1],
             age <= input$age_range_position[2])
  })
  
  # Finish Time Distribution Plot
  output$finish_dist_plot <- renderPlotly({
    
    # Define a color for male # Example: a shade of blue
    male_color <- "#91E5E2"
    
    if (input$result == "dnf") {
      # For DNF cases, show a bar chart of DNF counts by year and gender
      # Conditional color for males
      if (input$gender == "M") {
        p <- ggplot(filtered_wser_splits(), aes(x = as.factor(year), fill = gender)) +
          geom_bar(position = "dodge") +
          scale_fill_manual(values = c("M" = male_color), labels = c("M")) +
          theme_minimal() +
          labs(title = "Distribution of DNFs by Year",
               x = "Year",
               y = "Number of DNFs")
      } else {
        p <- ggplot(filtered_wser_splits(), aes(x = as.factor(year), fill = gender)) +
          geom_bar(position = "dodge") +
          theme_minimal() +
          labs(title = "Distribution of DNFs by Year",
               x = "Year",
               y = "Number of DNFs")
      }
      
    } else {
      # For finish times, show the original histogram
      # Conditional coloring based on gender input
      if(input$gender == "M"){
        p <- ggplot(filtered_wser_splits(), aes(x = time_hours, fill = gender)) +
          geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
          scale_fill_manual(values = c("M" = male_color), labels = c("M")) +
          theme_minimal() +
          labs(title = "Distribution of Finish Times",
               x = "Finish Time (hours)",
               y = "Count") +
          facet_wrap(~year)
      } else{
        
        p <- ggplot(filtered_wser_splits(), aes(x = time_hours, fill = gender)) +
          geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
          theme_minimal() +
          labs(title = "Distribution of Finish Times",
               x = "Finish Time (hours)",
               y = "Count") +
          facet_wrap(~year)
      }
    }
    
    ggplotly(p)
  })
  
  # Update the finish_summary_table to handle DNF cases and include percentages
  # The early part of the script remains unchanged until the finish_summary_table output definition
  
  output$finish_summary_table <- renderDT({
    # First get the base dataset with total counts before applying result filter
    base_data <- wser_splits %>%
      mutate(
        time_hours = convert_to_hours(time),
        result_type = case_when(
          time == "dnf" ~ "dnf",
          !is.na(convert_to_hours(time)) & convert_to_hours(time) < 24 ~ "silver",
          !is.na(convert_to_hours(time)) & convert_to_hours(time) >= 24 & convert_to_hours(time) <= 30 ~ "bronze",
          TRUE ~ "other"
        )
      ) %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2],
        age >= input$age_range[1],
        age <= input$age_range[2]
      )
    
    if (input$gender != "All") {
      base_data <- base_data %>% filter(gender == input$gender)
    }
    
    # Calculate total counts for percentage denominators
    total_counts <- base_data %>%
      group_by(year) %>%
      summarise(total_year = n())
    
    gender_counts <- base_data %>%
      group_by(year, gender) %>%
      summarise(total_gender_year = n())
    
    if (input$result == "dnf") {
      summary_table <- base_data %>%
        filter(result_type == "dnf") %>%
        group_by(year, gender) %>%
        summarise(
          dnf_count = n()
        ) %>%
        # Join with total counts
        left_join(total_counts, by = "year") %>%
        left_join(gender_counts, by = c("year", "gender")) %>%
        # Calculate percentages
        mutate(
          percent_all = round(dnf_count / total_year, 4),
          percent_gender = round(dnf_count / total_gender_year, 4)
        ) %>%
        # Remove helper columns
        select(-c(total_year, total_gender_year))
      
      # Conditionally remove percentage columns based on gender selection
      if (input$gender == "All") {
        summary_table <- summary_table %>%
          select(-percent_gender)
      } else {
        summary_table <- summary_table %>%
          select(-percent_all)
      }
      
      summary_table <- summary_table %>%
        arrange(year, gender)
      
    } else {
      filtered_data <- base_data %>%
        filter(case_when(
          input$result == "all_finishes" ~ result_type %in% c("silver", "bronze"),
          input$result == "silver" ~ result_type == "silver",
          input$result == "bronze" ~ result_type == "bronze",
          TRUE ~ FALSE
        ))
      
      summary_table <- filtered_data %>%
        group_by(year, gender) %>%
        summarise(
          finishers = n(),
          avg_time = as_hms(round(mean(time_hours, na.rm = TRUE) * 3600, 0)),
          median_time = as_hms(round(median(time_hours, na.rm = TRUE) * 3600, 0)),
          min_time = as_hms(round(min(time_hours, na.rm = TRUE) * 3600, 0)),
          max_time = as_hms(round(max(time_hours, na.rm = TRUE) * 3600, 0))
        ) %>%
        # Join with total counts
        left_join(total_counts, by = "year") %>%
        left_join(gender_counts, by = c("year", "gender")) %>%
        # Calculate percentages
        mutate(
          percent_all = round(finishers / total_year, 4),
          percent_gender = round(finishers / total_gender_year, 4)
        ) %>%
        # Remove helper columns
        select(-c(total_year, total_gender_year))
      
      # Conditionally remove percentage columns based on gender selection
      if (input$gender == "All") {
        summary_table <- summary_table %>%
          select(-percent_gender)
      } else {
        summary_table <- summary_table %>%
          select(-percent_all)
      }
      
      summary_table <- summary_table %>%
        arrange(year, gender)
    }
    
    DT::datatable(
      summary_table,
      options = list(paging = FALSE)
    ) %>%
      formatPercentage(
        # Dynamically set columns to format as percentage based on which ones exist
        names(summary_table)[names(summary_table) %in% c("percent_all", "percent_gender")],
        digits = 0
      )
  })
  
  # Checkpoint Analysis Plot
  output$checkpoint_plot <- renderPlotly({
    checkpoint_col <- paste0(input$checkpoint, "_time")
    ggplotly(
      ggplot(filtered_wser_splits_checkpoint(), 
             aes_string(x = "age", y = checkpoint_col, color = "gender")) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "loess") +
        theme_minimal() +
        scale_y_time(labels = function(x) strftime(x, format = "%H:%M:%S")) +
        labs(title = paste("Time vs Age at", input$checkpoint),
             x = "Age",
             y = "Time (hours)")
    )
  })
  
  # Checkpoint Summary Table
  output$checkpoint_summary_table <- renderDT({
    checkpoint_col <- paste0(input$checkpoint, "_time")
    DT::datatable(
      filtered_wser_splits_checkpoint() %>%
        filter(!is.na(!!sym(checkpoint_col))) %>%
        group_by(year, gender) %>%
        summarise(
          runners = n(),
          avg_decimal_hours = mean(!!sym(checkpoint_col), na.rm = TRUE),
          avg_seconds = avg_decimal_hours,
          avg_time = as_hms(round(avg_seconds,0)),
          median_decimal_hours = median(!!sym(checkpoint_col), na.rm = TRUE),
          median_seconds = median_decimal_hours,
          median_time = as_hms(round(median_seconds, 0)),
          min_decimal_hours = min(!!sym(checkpoint_col), na.rm = TRUE),
          min_seconds = min_decimal_hours,
          min_time = as_hms(round(min_seconds,0)),
          max_decimal_hours = max(!!sym(checkpoint_col), na.rm = TRUE),
          max_seconds = max_decimal_hours,
          max_time = as_hms(round(max_seconds,0))
        ) %>%
        select(-c(avg_decimal_hours,avg_seconds)) %>%
        select(-c(median_decimal_hours,median_seconds)) %>%
        select(-c(min_decimal_hours,min_seconds)) %>%
        select(-c(max_decimal_hours,max_seconds)) %>%
        arrange(year, gender),
      options = list(
        paging = FALSE)
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)