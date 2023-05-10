library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(stringr)
library(reshape2)
library(shiny)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(DT)
library(scales)

rm(list=ls())

# setwd("D:/DATA 332")
setwd("~/DATA-332/Final")

raw_data <- read.csv("Most-Recent-Cohorts-Institution.csv")

clean_data_student <- raw_data%>%
  select(UNITID,INSTNM, STABBR, LOAN_EVER, FEMALE,  AGE_ENTRY,MD_EARN_WNE_P10, MD_FAMINC)

clean_data_addmission <- raw_data%>%
  select(UNITID, UGDS, ADM_RATE)

state_dictionary <- read.csv("State_name.csv")

data_join <- left_join(clean_data_student, clean_data_addmission, by = "UNITID") %>% 
  filter(UGDS != "NULL") %>% 
  mutate(ADM_RATE_CLEAN = str_replace_all(ADM_RATE, "NULL", NA_character_)) %>%
  transform(ADM_RATE = as.numeric(ADM_RATE_CLEAN), UGDS = as.numeric(UGDS)) %>%
  mutate(ADM_Rate_P = ADM_RATE*100)%>%
  select(-ADM_RATE_CLEAN) %>%
  mutate(SCHOOL_SIZE = ifelse(UGDS < 5000, "Small", ifelse(UGDS < 15000, "Medium", "Large")))

data_for_analysis <- left_join(data_join, state_dictionary, by = "STABBR")

data_for_analysis <- data_for_analysis %>%
  select(-STABBR) %>%
  rename(
    Percent_students_with_loans = LOAN_EVER,
    Percent_Female_students = FEMALE,
    Mean_entry_age = AGE_ENTRY,
    Median_family_income = MD_FAMINC,
    Median_earnings_after_10yrs = MD_EARN_WNE_P10,
    Admissions_rate_percent = ADM_Rate_P,
    School_size = SCHOOL_SIZE,
    Institution_name = INSTNM
  ) %>%
  select(-ADM_RATE)

# Make all columns numeric values
data_for_analysis$Percent_students_with_loans = as.numeric(data_for_analysis$Percent_students_with_loans)
data_for_analysis$Percent_Female_students = as.numeric(data_for_analysis$Percent_Female_students)
data_for_analysis$Mean_entry_age = as.numeric(data_for_analysis$Mean_entry_age)
data_for_analysis$Median_earnings_after_10yrs = as.numeric(data_for_analysis$Median_earnings_after_10yrs)
data_for_analysis$Median_family_income = as.numeric(data_for_analysis$Median_family_income)

# Add column for percentage of male students
data_for_analysis$Percent_Male_students =  (1 - data_for_analysis$Percent_Female_students)

# Round all numbers
data_for_analysis$Percent_students_with_loans = round(100*data_for_analysis$Percent_students_with_loans, 2)
data_for_analysis$Percent_Female_students = round(100*data_for_analysis$Percent_Female_students, 2)
data_for_analysis$Percent_Male_students = round(100*data_for_analysis$Percent_Male_students, 2)
data_for_analysis$Mean_entry_age = round(data_for_analysis$Mean_entry_age, 0)
data_for_analysis$Admissions_rate_percent = round(data_for_analysis$Admissions_rate_percent, 2)
data_for_analysis$Median_family_income = round(data_for_analysis$Median_family_income, 0)
data_for_analysis$Median_earnings_after_10yrs = round(data_for_analysis$Median_earnings_after_10yrs, 0)

data_for_analysis <- data_for_analysis[rowSums(is.na(data_for_analysis)) < 3, ]

write_csv(data_for_analysis, "data_for_analysis.csv")
