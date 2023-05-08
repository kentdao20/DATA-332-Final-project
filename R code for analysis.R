
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

setwd("D:/DATA 332")

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

data_for_analysis <- left_join(data_join, state_dictionary, by = "STABBR") %>%
  na.omit()

data_for_analysis <- data_for_analysis%>%
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
  ) #%>%
  #mutate(Percent_students_with_loans = round(100*Percent_students_with_loans,0),
         #Percent_Female_students = round(100*Percent_Female_students,0),
         #Mean_entry_age = round(Mean_entry_age, 0),
         #Admissions_rate_percent = round(Admissions_rate_percent, 0),
         #Median_family_income = round(Median_family_income,0),
         #Median_earnings_after_10yrs = round(Median_earnings_after_10yrs,0)
  #) %>%
  select(-ADM_RATE)

  
write_csv(data_for_analysis, "data_for_analysis.csv")



