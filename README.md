# DATA 332 Final project

## Author: 


## Overview:
   The data we are working with is the college scorecard, which was taken from the government website data.gov. This dataset is recording the data about universities and their percentage of acceptance, gender, financial and income after college. We will be working with this data to visualize the colleges base on the population, their income 10 years after college, acceptance rate the percent of students who are having financial aid and students who are still 

### Cleaning data:

**1. Load the original data into r and select columns that we need:**

```r
raw_data <- read.csv("Most-Recent-Cohorts-Institution.csv")

clean_data_student <- raw_data%>%
  select(UNITID,INSTNM, STABBR, LOAN_EVER, FEMALE,  AGE_ENTRY,MD_EARN_WNE_P10, MD_FAMINC)
```
 
**2. Choose the data relate to the addmission rate:**

```r
clean_data_addmission <- raw_data%>%
  select(UNITID, UGDS, ADM_RATE)
```

**3. Make a dictionary for states:**
```r
state_dictionary <- read.csv("State_name.csv")
```

**4. Join the data relate the states to the addmission rate by the UNITID of the school: **
```r
data_join <- left_join(clean_data_student, clean_data_addmission, by = "UNITID") %>% 
  filter(UGDS != "NULL") %>% 
  mutate(ADM_RATE_CLEAN = str_replace_all(ADM_RATE, "NULL", NA_character_)) %>%
  transform(ADM_RATE = as.numeric(ADM_RATE_CLEAN), UGDS = as.numeric(UGDS)) %>%
  mutate(ADM_Rate_P = ADM_RATE*100)%>%
  select(-ADM_RATE_CLEAN) %>%
  mutate(SCHOOL_SIZE = ifelse(UGDS < 5000, "Small", ifelse(UGDS < 15000, "Medium", "Large")))
```

**5. Left join the states name into the data set:**
```r
data_for_analysis <- left_join(data_join, state_dictionary, by = "STABBR")
```

**6. Change the data into numeric value and change value into numeric:**
```r
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

data_for_analysis$Percent_students_with_loans = as.numeric(data_for_analysis$Percent_students_with_loans)
data_for_analysis$Percent_Female_students = as.numeric(data_for_analysis$Percent_Female_students)
data_for_analysis$Mean_entry_age = as.numeric(data_for_analysis$Mean_entry_age)
data_for_analysis$Median_earnings_after_10yrs = as.numeric(data_for_analysis$Median_earnings_after_10yrs)
data_for_analysis$Median_family_income = as.numeric(data_for_analysis$Median_family_income)
```

**7. Add a column for male data after subtract from female and round up value:**
```r
data_for_analysis$Percent_Male_students =  (1 - data_for_analysis$Percent_Female_students)

# Round all numbers
data_for_analysis$Percent_students_with_loans = round(100*data_for_analysis$Percent_students_with_loans, 2)
data_for_analysis$Percent_Female_students = round(100*data_for_analysis$Percent_Female_students, 2)
data_for_analysis$Percent_Male_students = round(100*data_for_analysis$Percent_Male_students, 2)
data_for_analysis$Mean_entry_age = round(data_for_analysis$Mean_entry_age, 0)
data_for_analysis$Admissions_rate_percent = round(data_for_analysis$Admissions_rate_percent, 2)
data_for_analysis$Median_family_income = round(data_for_analysis$Median_family_income, 0)
data_for_analysis$Median_earnings_after_10yrs = round(data_for_analysis$Median_earnings_after_10yrs, 0)

```
**8. Drop all the values that have more than 3 n/a in the data:**
```r
data_for_analysis <- data_for_analysis[rowSums(is.na(data_for_analysis)) < 3, ]

```

**9. Save data as a CSV:**
```r
write_csv(data_for_analysis, "data_for_analysis.csv")
```

### Data graph:
**1. Charts** 



### Shiny Application:
**1. UI:**
```r
ui <- fluidPage(theme = shinytheme("yeti"),useShinyjs(),
                inlineCSS(appCSS),
                titlePanel("Impact of School Size on Higher Education (USA)", 
                           windowTitle = "scorecard app"),
                sidebarLayout(
                  sidebarPanel(width = 3,
                               pickerInput("state_input", 
                                           label = "Select states of interest", 
                                           choices = states, #Select the states of interest. 
                                           multiple = TRUE, # Multiselection is possible
                                           options = list(`actions-box` = TRUE), 
                                           selected = "California"),
                               sliderInput("admin_input", 
                                           "Select an admission rate range",
                                           min = 0, max = 100, value = c(0, 100), post="%"),
                               pickerInput("School_size", # Select school size
                                           label = "Select school sizes", 
                                           choices = schools, 
                                           selected = schools,
                                           multiple = TRUE, # Multiselection is possible
                                           options = list(`actions-box` = TRUE)),
                               htmlOutput("lines")
                               
                  ),
                  
                
                  mainPanel(width = 8,
                            tabsetPanel( id = 'tabs', selected = 'Total',
                                         tabPanel("Total", value = 'Total', plotOutput("row_1_T"), plotOutput("row_2_T"), 
                                                  plotOutput("row_3_T")),
                                         tabPanel("Small School List", value = 'Small', tableOutput("small_T")),
                                         tabPanel("Medium School List",value = 'Medium', tableOutput("medium_T")),
                                         tabPanel("Large School List", value = 'Large', tableOutput("large_T")))
                  )
                )
)
```
This UI is make with a side bar to choose showing either small, medium or large school or all 3 if you want to. Next to it will have 4 main tabs: Graphs, pivot of small, medium and large school. 

**2. Shiny App Link**

https://kienkcp.shinyapps.io/College_Analysis/

