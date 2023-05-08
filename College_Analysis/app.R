#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(gridExtra)
library(cowplot)
library(shinyjs)
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

appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"
data <- read.csv("data_for_analysis.csv", stringsAsFactors = FALSE)

states <- sort(unique(data$State_Name))

schools <- c("Small", "Medium", "Large")


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


server <- function(input, output) {
  
  output$lines <- renderText({
    paste( 
      "Large schools: 15,000 + students", 
      "Medium schools: 5,000 - 15,000 students", 
      "Small schools: 1 - 5,000 students", sep="<br>")
  })
 
  School_size <- reactive(input$tabs)
  fill_cols <- reactive({
    if (School_size() == 'Total'){
      fill_cols = c("black", "blue", "light blue")
    }
    else if (School_size() == 'Small'){
      fill_cols = c("light blue")
    }
    else if (School_size() == 'Medium'){
      fill_cols = c("blue")
    }
    else {
      fill_cols = c("black")
    }
  })
  
  data_filtered <- reactive({
    
    if (input$admin_input[1] == 0 & input$admin_input[2] == 100) {
      data %>% 
       
        filter(School_size %in% input$School_size,
               State_Name %in% input$state_input)
    }
    else {
      data %>% 
       
        filter(Admissions_rate_percent < input$admin_input[2],
               Admissions_rate_percent > input$admin_input[1],
               
               School_size %in% input$School_size,
               State_Name %in% input$state_input)        
    }
  })
  
  count_schools <- reactive({
    nrow(data_filtered())
  })
  
 
  school_plot <- reactive({
    data_filtered() %>% group_by(School_size) %>% summarise(count = n()) %>% 
      arrange(count, desc(count)) %>% 
      ggplot(aes(x = School_size, y = count, fill = School_size)) +
      geom_bar(colour = "black", stat = "identity", alpha = 0.3) +
      geom_text(aes(label = count), position = position_dodge(width=1), vjust = -0.25, size = 5) +
      theme_bw() +
      theme(text = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_fill_manual(values = fill_cols()) +
      ggtitle("Number of Schools by Size") +
      xlab("School Size") + 
      ylab("Count") +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
 
  Percent_Female_students_dis_plot <- reactive({
    data_filtered() %>% filter(!is.na(Percent_Female_students)) %>% group_by(School_size) %>%  
      ggplot(aes((x = Percent_Female_students), fill = School_size)) +
      geom_density(alpha = .3) +
      scale_fill_manual(values = fill_cols()) +
      theme_bw() +
      theme(text = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      ggtitle("Distirubtion of the Percent of Female Students") +
      xlab("Percentage of students (%)") + 
      ylab("Density") +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(fill = guide_legend(title = "School Size"))
  })
  
  
  median_10yr_earn <- reactive({
    data_filtered() %>% filter(!is.na(Median_earnings_after_10yrs)) %>% group_by(School_size) %>% 
      ggplot(aes(x = Median_earnings_after_10yrs, fill = School_size)) + 
      geom_density(alpha = 0.3) +
      theme_bw() + 
      theme(text = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_fill_manual(values = fill_cols()) +
      ggtitle("Distirbution of Median Earnings 10yrs after Graduation") +
      xlab("Earnings ($)") +
      ylab("Density") +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(fill = guide_legend(title = "School Size"))
  })
  

  Mean_entry_age_plot <- reactive({
    data_filtered() %>% filter(!is.na(Mean_entry_age)) %>% group_by(School_size) %>%
      ggplot(aes(x = Mean_entry_age, fill = School_size)) + geom_density(alpha = 0.3) +
      theme_bw() + 
      theme(text = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_fill_manual(values = fill_cols()) +
      theme(legend.position = "none") +
      ggtitle("Distribution of Entrance Age") +
      xlab("Age") +
      ylab("Density") +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(fill = guide_legend(title = "School Size"))
  })
  
  
  perc_fed_loans <- reactive({
    data_filtered() %>% filter(!is.na(Percent_students_with_loans)) %>% group_by(School_size) %>%
      ggplot(aes(x = Percent_students_with_loans, fill=School_size)) + geom_density(alpha = 0.3) +
      theme_bw() + 
      theme(text = element_text(size=14),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_fill_manual(values = fill_cols()) +
      theme(legend.position = "none") +
      ggtitle("Distribution of Students Receiving Financial Aid (%)") +
      xlab("Financial aid (%)") +
      ylab("Density") +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(fill = guide_legend(title = "School Size"))
  })
  
  
  med_fam_earn <- reactive({
    data_filtered() %>% filter(!is.na(Median_family_income)) %>% group_by(School_size) %>%
      ggplot(aes(x = Median_family_income, fill=School_size)) + geom_density(alpha = 0.3) +
      theme_bw() + 
      theme(text = element_text(size=14),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_fill_manual(values = fill_cols()) +
      theme(legend.position = "none") +
      ggtitle("Distribution of Median Family Earnings") +
      xlab("Family earnings ($)") +
      ylab("Density") +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      guides(fill = guide_legend(title = "School Size"))
    
  })
  
 
  output$row_1_T <- renderPlot({
    if (count_schools() == 0) {
      
      validate(
        need(count_schools() != 0, "There are no schools returned for your filtering criteria.
                                    Please increase your filtering criteria.")
      )      
    }
    if (count_schools() != 1) {
      grid.arrange(school_plot() + theme(legend.position = "none"),
                   median_10yr_earn() + theme(legend.position = "none"),
                   get_legend(school_plot() + guides(fill = guide_legend(title = "School Size"))),
                   ncol = 3, nrow = 1,
                   widths = c(5, 5, 1))
    }
    else {
      grid.arrange(school_plot() + theme(legend.position = "none"),
                   widths = c(7,6))
    }
  })
  
 
  output$row_2_T <- renderPlot({
    if (count_schools() == 0) {
      
      validate(
        need(count_schools() != 0, "")
      )
    }
    if (count_schools() == 1) {
      
      validate(
        need(count_schools() != 1, "Additional graphs cannot be displayed when there is only one school selected. 
                                    Please increase your filtering criteria")
      )
    }
    else {
      grid.arrange(
        Percent_Female_students_dis_plot() + theme(legend.position = "none"),
        Mean_entry_age_plot() + theme(legend.position = "none"),
        get_legend(school_plot() + guides(fill = guide_legend(title = "School Size"))),
        ncol = 3, nrow = 1,
        widths = c(5, 5, 1))
    }
    
  })
  
 
  output$row_3_T <- renderPlot({
    if (count_schools() == 0) {
      
      validate(
        need(count_schools() != 0, "")
      )      
    }
    if (count_schools() != 1) {
      grid.arrange(
        perc_fed_loans() + theme(legend.position = "none"),
        med_fam_earn() + theme(legend.position="none"),
        get_legend(school_plot() + guides(fill = guide_legend(title = "School Size"))),
        ncol = 3, nrow = 1,
        widths = c(5, 5, 1))
    }
  })
  

  output$small_T <- renderTable({
    data_small <- data_filtered() %>% filter(School_size == 'Small') %>%
      select(Institution_name, Median_earnings_after_10yrs, 
             Percent_Female_students, Mean_entry_age,
             Percent_students_with_loans, Median_family_income) %>%
      arrange(Institution_name) %>%
      rename("Institution name" = Institution_name,
             "Median earnings after 10yrs" = Median_earnings_after_10yrs,
             "Percent female students" = Percent_Female_students,
             "Mean entry age" = Mean_entry_age,
             "Percent students with loans" = Percent_students_with_loans,
             "Median family income" = Median_family_income
      )
   
    validate(
      need(nrow(data_filtered() %>% filter(School_size == 'Small')) > 0, 
           "There are no small schools returned for your filtering criteria.
                                 Please increase your filtering criteria.")
    )
    
    validate(
      need(nrow(data_small) < 150, "There are too many values to display. 
           Please increase your filtering criteria to return fewer than 150 small sized schools")
    ) 
    data_small
  })
  
  
  output$medium_T <- renderTable({
    data_medium <- data_filtered() %>% filter(School_size == 'Medium') %>%
      select(Institution_name, Median_earnings_after_10yrs, 
             Percent_Female_students, Mean_entry_age,
             Percent_students_with_loans, Median_family_income) %>%
      arrange(Institution_name) %>%
      rename("Institution name" = Institution_name,
             "Median earnings after 10yrs" = Median_earnings_after_10yrs,
             "Percent female students" = Percent_Female_students,
             "Mean entry age" = Mean_entry_age,
             "Percent students with loans" = Percent_students_with_loans,
             "Median family income" = Median_family_income
      )
    validate(
      need(nrow(data_filtered() %>% filter(School_size == 'Medium')) > 0, 
           "There are no medium schools returned for your filtering criteria.
                                 Please increase your filtering criteria.")
    )
    validate(
      need(nrow(data_medium) < 150, "There are too many values to display. 
           Please increase your filtering criteria to return fewer than 150 medium sized schools")
    ) 
    data_medium
    
  })
  

  output$large_T <- renderTable({
    data_large <- data_filtered() %>% filter(School_size == 'Large') %>%
      select(Institution_name, Median_earnings_after_10yrs, 
             Percent_Female_students, Mean_entry_age,
             Percent_students_with_loans, Median_family_income) %>%
      arrange(Institution_name) %>%
      rename("Institution name" = Institution_name,
             "Median earnings after 10yrs" = Median_earnings_after_10yrs,
             "Percent female students" = Percent_Female_students,
             "Mean entry age" = Mean_entry_age,
             "Percent students with loans" = Percent_students_with_loans,
             "Median family income" = Median_family_income
      )
    validate(
      need(nrow(data_filtered() %>% filter(School_size == 'Large')) > 0, 
           "There are no large schools returned for your filtering criteria.
                                    Please increase your filtering criteria.")
    )
    validate(
      need(nrow(data_large) < 150, "There are too many values to display. 
           Please increase your filtering criteria to return fewer than 150 large sized schools")
    ) 
    data_large
  })
}


shinyApp(ui = ui, server = server)