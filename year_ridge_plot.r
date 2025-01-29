###################################

library(ggplot2)
library(dplyr)
library(lubridate)
library(ggridges)
library(hms)
library(scales)

# Read the CSV file
data <- read.csv("/home/drew/projects/r/test_wser_plot_data.csv") %>% 
  setNames(c("event_year","finish_time","runner_age","result_type")) %>% 
  select(event_year,finish_time) %>% 
  mutate(event_year = as.factor(event_year))  %>% 
  mutate(finish_time = lubridate::hms(finish_time))

# Create the base horizontal density plot
myplot <- ggplot(data, aes(x = finish_time, y = factor(event_year), fill = factor(event_year))) +
  geom_density_ridges(rel_min_height = 0.01,
                      jittered_points = TRUE,point_shape = '*', point_size = 3, point_alpha = 2, alpha = 0.7,
                      # rel_max_height = 0.1,
                      scale = 0.9, 
                      bandwidth = 2000) +
  scale_x_time(labels = function(x) format(x, "%H:%M:%S")) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Distribution of Times by Year with Summary Statistics",
    x = "Time (HH:MM:SS)",
    y = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

# summary stats
data <- read.csv("/home/drew/projects/r/test_wser_plot_data.csv") %>% 
  setNames(c("event_year","finish_time","runner_age","result_type")) %>% 
  select(event_year,finish_time) %>% 
  mutate(event_year = as.factor(event_year)) 

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

data <- data %>% 
  mutate(
    finish_time = convert_to_hours(finish_time)
  ) 

summary_table <- data %>%
  group_by(event_year) %>%
  summarise(
    # finishers = n(),
    mean_time = as_hms(round(mean(finish_time, na.rm = TRUE) * 3600, 0)),
    median_time = as_hms(round(median(finish_time, na.rm = TRUE) * 3600, 0)),
    min_time = as_hms(round(min(finish_time, na.rm = TRUE) * 3600, 0)),
    max_time = as_hms(round(max(finish_time, na.rm = TRUE) * 3600, 0))
  )

# Reshape summary statistics to long format
summary_long <- tidyr::pivot_longer(
  summary_table,
  cols = c(min_time, mean_time, median_time, max_time),
  names_to = "stat_type",
  values_to = "time_value"
) %>%
  mutate(
    stat_type = gsub("_time$", "", stat_type),
    # y_min = as.numeric(factor(event_year)) - 0.45,
    # y_max = as.numeric(factor(event_year)) + 0.45
    y_min = as.numeric(factor(event_year)) - 0.01,
    y_max = as.numeric(factor(event_year)) + 0.85
  )

myplot +
  geom_segment(
    data = summary_long,
    aes(
      x = time_value,
      xend = time_value,
      y = y_min,
      yend = y_max,
      color = stat_type
    ),
    linetype = "solid",
    linewidth = 0.5
  ) +
  scale_x_time(
    labels = function(x) format(x, "%H:%M:%S"),
    breaks = date_breaks("2 hours"),
    minor_breaks = date_breaks("1 hour")
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_fill_brewer(
    palette = "Pastel1",
    name = "Event Year") +
  scale_color_manual(
    values = c("min" = "blue", "mean" = "red", "median" = "purple", "max" = "darkgreen"),
    name = "Statistics"
  ) +
  labs(
    title = "Finish Time Distribution by Year with Summary Statistics",
    x = "Time to Finish",
    y = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 16) # Centering the title
  )
