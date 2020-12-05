library(tidyverse)
library(lubridate)

dta <- read_csv(here::here("data", "dta.csv"))

#######################################
# Student Name list for drop-down selectInput
student_name <- dta %>%
  select(student_name)

# min & max dates for dateRangeInput
min_date <- min(dta$date, na.rm = TRUE)
max_date <- max(dta$date, na.rm = TRUE)

# Data table for Data tab
completedata_table <- dta %>%
  select(-c(minutes_in_office)) %>%
  mutate(time_in = make_time(time_in),
         time_out = make_time(time_out),
         time_lost = round(time_lost, 0)) %>% 
  select(date, student_name, grade, teacher_name, time_in, time_out, time_lost, everything()) %>% 
  reactable(
    .,
    striped = TRUE,
    searchable = TRUE,
    width = 1200,
    selection = "multiple",
    borderless = TRUE,
    onClick = "select",
    theme = reactableTheme(
      rowSelectedStyle = list(backgroundColor = "#98f5ff", boxShadow = "inset 2px 0 0 0 #ffa62d")
    )
  )
