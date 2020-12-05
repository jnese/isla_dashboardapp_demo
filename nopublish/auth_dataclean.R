library(googlesheets4)
library(tidyverse)

gs4_auth(email = "jnese@uoregon.edu")

sheet_id <- "1ZuuB21hCNHFgKeN4v9AnQuu7EamCXDx7RK_gU464ib4"

dta_raw <- read_sheet(sheet_id, sheet = "Signed_In") %>% 
  janitor::clean_names()

dta <- dta_raw %>%
  filter(!is.na(grade)) %>% 
  separate(student_name, c("last", "first"), ",") %>% 
  mutate(student_name = paste0(first, " ", last)) %>% 
  select(student_name, everything()) %>% 
  #  select(-last, -first) %>% 
  mutate_at(vars(teacher_name, does_students_behavior_pose_a_safety_risk_to_them_or_others:did_the_student_reenter_the_classroom),
            ~na_if(., "")) %>% 
  mutate(teacher_name = replace_na(teacher_name, "missing")) %>% 
  mutate(time_in = na_if(time_in, "Never Signed In")) %>% 
  mutate(time_out = na_if(time_out, "Never Signed Out")) %>% 
  unnest(cols = c(time_in, time_out)) %>% 
  mutate(time_lost = lubridate::int_length(lubridate::interval(time_in, time_out))/60,
         date = lubridate::as_date(time_in)) %>% 
  drop_na(grade) %>% 
  mutate(what_happened = ifelse(!is.na(what_happened), "[teacher report of what happened]", NA_character_),
         notes = ifelse(!is.na(notes), "[ISLA facilitator notes]", NA_character_))

anon_names <- readxl::read_excel(here::here("nopublish", "anon_names.xlsx"), sheet = "names") %>% 
  mutate(teacher_anon = str_trim(teacher_anon, side = "both"),
         student_anon = str_trim(student_anon, side = "both"))

#######################################

dta <- tibble(
  teacher_name = unique(dta$teacher_name),
  teacher_anon = pull(filter(anon_names, !is.na(teacher_anon)), teacher_anon)
) %>% 
  right_join(dta) %>% 
  select(-teacher_name) %>% 
  rename(teacher_name = teacher_anon) 

dta <- tibble(
  student_name = unique(dta$student_name),
  student_anon = pull(filter(anon_names, !is.na(student_anon)), student_anon)
) %>% 
  right_join(dta) %>% 
  select(-c(student_name, last, first)) %>% 
  rename(student_name = student_anon) 


rio::export(dta, here::here("data", "dta.csv"))
