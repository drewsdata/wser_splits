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
wser_cp_table <- read_csv(here("data","wser_cp_table.csv"))
wser_course_checkpoints <- read_csv(here("data","wser_course_checkpoints.csv"))

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
                 
                 # Checkpoint selectors
                 selectInput("start_checkpoint",
                             "Select Start Checkpoint:",
                             choices = setNames(wser_cp_table$cp_column, 
                                                wser_cp_table$cp_display_name)
                 ),
                 
                 selectInput("end_checkpoint",
                             "Select End Checkpoint:",
                             choices = NULL
                 ),
                 # Add colored divider
                 tags$div(
                   style = "margin: 4px 0;  /* Add space above and below */
                           border-bottom: 4px solid #87CEEB; 
                           width: 100%;" 
                 ),
                 # Course Checkpoints header with hyperlink
                 tags$h4("Checkpoint distances  ",
                         tags$a("(Check here for WSER offical aid stations)", 
                                href = "https://www.wser.org/course/aid-stations/",
                                target = "_blank",  # Opens in new tab
                                style = "color: #4682B4; text-decoration: none;")  # Steel blue color, no underline
                 ),
                 DTOutput("course_checkpoints_table")
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
server <- function(input, output, session) {
  
  # Add output for course checkpoints table
  output$course_checkpoints_table <- renderDT({
    datatable(
      wser_course_checkpoints,
      options = list(
        pageLength = 25,
        #scrollY = "300px",
        #scrollCollapse = TRUE,
        dom = 't',  # Only show table, no search/pagination controls
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
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
  
  # Update end_checkpoint choices
  observe({
    req(input$start_checkpoint)
    
    start_cp_number <- wser_cp_table %>%
      filter(cp_column == input$start_checkpoint) %>%
      pull(cp_number)
    
    choices <- wser_cp_table %>%
      filter(cp_number > start_cp_number) %>%
      # Use setNames to properly create the choices vector
      { setNames(.$cp_column, .$cp_display_name) }
    
    updateSelectInput(session, "end_checkpoint", choices = choices)
  })
  
  # Finish Time Distribution Plot
  output$finish_dist_plot <- renderPlotly({
    # Define colors for consistent gender representation
    gender_colors <- c("M" = "#91E5E2", "F" = "#FFB6C6")
    
    # Get the filtered data first
    plot_data <- filtered_wser_splits()
    
    # Check if we have any data
    if (nrow(plot_data) == 0) {
      # Create an empty plot with a message
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No data available for the selected filters",
                 size = 6) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }
    
    # Function to determine appropriate breaks based on data range
    get_breaks <- function(x) {
      if (max(x) <= 10) {
        # For small counts, use integers from 0 to max
        return(seq(0, ceiling(max(x)), by = 1))
      } else {
        # For larger counts, use pretty breaks
        return(pretty(c(0, max(x)), n = 8))
      }
    }
    
    if (input$result == "dnf") {
      # Get count data for DNF plot
      count_data <- plot_data %>%
        count(year, gender) %>%
        pull(n)
      
      # DNF plot with consistent colors
      p <- ggplot(plot_data) +
        geom_bar(aes(x = as.factor(year), 
                     fill = gender,
                     text = paste0("Count: ", stat(count))),
                 position = "dodge") +
        scale_fill_manual(values = gender_colors) +
        scale_y_continuous(breaks = get_breaks) +
        theme_minimal() +
        labs(title = "DNFs by Year",
             x = "Year",
             y = "Count")
      
      ggplotly(p, tooltip = c("gender","text")) %>% 
        config(displayModeBar = FALSE)
      
    } else {
      # Get count data for histogram
      count_data <- plot_data %>%
        group_by(year) %>%
        summarise(count = n()) %>%
        pull(count)
      
      # Finish times histogram with consistent colors
      p <- ggplot(plot_data, aes(x = time_hours, fill = gender)) +
        geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
        scale_fill_manual(values = gender_colors) +
        scale_x_continuous(breaks = seq(0, 30, by = 5)) +
        scale_y_continuous(breaks = get_breaks) +
        theme_minimal() +
        labs(title = "Distribution of Finish Times",
             x = "Finish Time (hours)",
             y = "Count") +
        facet_wrap(~year)
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    }
  })
  
  # Summary data table with conditional display
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
  
  # Define colors for consistent gender representation at the start of server function
  gender_colors <- c("M" = "#91E5E2", "F" = "#FFB6C6")  # Blue for men, Pink for women
  
  # Modified checkpoint plot code
  output$checkpoint_plot <- renderPlotly({
    req(input$start_checkpoint, input$end_checkpoint)
    
    start_checkpoint_col <- paste0(input$start_checkpoint, "_time")
    end_checkpoint_col <- paste0(input$end_checkpoint, "_time")
    
    # Create a lookup map for checkpoint names
    checkpoint_names <- setNames(as.character(wser_cp_table$cp_display_name), wser_cp_table$cp_column)
    
    # Calculate the difference before plotting
    plot_data <- filtered_wser_splits_checkpoint() %>%
      mutate(
        time_diff = !!sym(end_checkpoint_col) - !!sym(start_checkpoint_col),
        time_diff_hms = as_hms(round(time_diff, 0)) # Time in HH:MM:SS format
      ) %>%
      filter(!is.na(time_diff))  # Remove NA values
    
    # Check if we have any data
    if (nrow(plot_data) == 0) {
      # Create an empty plot with a message
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No data available for the selected filters",
                 size = 6) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }
    
    # Check if we have enough data for smoothing (at least 5 points per group)
    min_group_size <- plot_data %>%
      group_by(gender) %>%
      summarise(n = n()) %>%
      pull(n) %>%
      min()
    
    base_plot <- ggplot(plot_data) +
      geom_point(aes(x = age, 
                     y = time_diff,
                     color = gender,
                     text = paste0("gender: ", gender,
                                   "<br>age: ", age,
                                   "<br>", 
                                   checkpoint_names[input$start_checkpoint], " - ", 
                                   checkpoint_names[input$end_checkpoint], ": ",
                                   time_diff_hms)),
                 alpha = 0.6) +
      theme_minimal() +
      scale_y_time(labels = function(x) strftime(x, format = "%H:%M:%S")) +
      scale_color_manual(values = gender_colors) +
      labs(title = paste("Time vs Age Between",
                         checkpoint_names[input$start_checkpoint],
                         "and",
                         checkpoint_names[input$end_checkpoint]),
           x = "Age",
           y = "Time (hh:mm:ss)")
    
    # Add smooth lines only if we have enough data
    if (min_group_size >= 5) {
      p <- base_plot +
        geom_smooth(aes(x = age, 
                        y = time_diff,
                        color = gender), 
                    method = "loess",
                    se = TRUE)
    } else {
      p <- base_plot
    }
    
    plt <- ggplotly(p, tooltip = "text") %>% 
      config(displayModeBar = FALSE)
    
    # Remove hover info from smooth lines if they exist
    for(i in seq_along(plt$x$data)) {
      if(plt$x$data[[i]]$type == "scatter" && plt$x$data[[i]]$mode == "lines") {
        plt$x$data[[i]]$hoverinfo <- "skip"
      }
    }
    
    plt
  })
  
  # Checkpoint Summary Table
  output$checkpoint_summary_table <- renderDT({
    req(input$start_checkpoint, input$end_checkpoint)
    
    start_checkpoint_col <- paste0(input$start_checkpoint, "_time")
    end_checkpoint_col <- paste0(input$end_checkpoint, "_time")
    
    summary_data <- filtered_wser_splits_checkpoint() %>%
      filter(!is.na(!!sym(start_checkpoint_col)), !is.na(!!sym(end_checkpoint_col))) %>%
      mutate(
        time_diff = !!sym(end_checkpoint_col) - !!sym(start_checkpoint_col)
      )
    
    DT::datatable(
      summary_data %>%
        group_by(year, gender) %>%
        summarise(
          runners = n(),
          avg_decimal_hours = mean(time_diff, na.rm = TRUE),
          avg_seconds = avg_decimal_hours,
          avg_time = as_hms(round(avg_seconds,0)),
          median_decimal_hours = median(time_diff, na.rm = TRUE),
          median_seconds = median_decimal_hours,
          median_time = as_hms(round(median_seconds, 0)),
          min_decimal_hours = min(time_diff, na.rm = TRUE),
          min_seconds = min_decimal_hours,
          min_time = as_hms(round(min_seconds,0)),
          max_decimal_hours = max(time_diff, na.rm = TRUE),
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