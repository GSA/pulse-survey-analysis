library(tidyverse)
library(shiny)
library(hash)
library(data.table)
library(marginaleffects)
library(ggplot2)
library(modelbased)
library(emmeans)
library(devtools)
set.seed(47)
setwd("D:/nmiller/")
install("pulse")

### Calculate means and regression-adjusted means ----
covariates <- pulse::get_covariates()

models = list(engagement2_q1_full_wt, engagement2_q2_full_wt, engagement2_q3_full_wt, engagement2_q4_full_wt,
              equity2_q1_full_wt, equity2_q2_full_wt, equity2_q3_full_wt,
              reentry2_q1_full_wt, reentry2_q2A_full_wt, reentry2_q2B_full_wt, reentry2_q3_full_wt)

# loop over questions and covariates to produce means
# and store in an output data frame

groups <- pulse::get_groups()

question_codes_2 <- c("ENGAGEMENT2_Q1", "ENGAGEMENT2_Q2", "ENGAGEMENT2_Q3",
                      "ENGAGEMENT2_Q4", "EQUITY2_Q1", "EQUITY2_Q2", "EQUITY2_Q3",
                      "REENTRY2_Q1", "REENTRY2_Q2A", "REENTRY2_Q2B","REENTRY2_Q3")

avg_df_2 <- pulse::get_government_wide_means(analysis_dataset_2, question_codes, groups)

model_df_2 <- pulse::get_government_wide_adjusted_means(models)

#### Create R Shiny visualization ----
covariate_labels <- pulse::get_covariate_labels()

#### Mappings of questions and variables ####
reentry2_q1 = "I trust agency leadership to do what's right to protect employees' health, safety, and wellbeing."
reentry2_q2a = "If I found a job elsewhere with more workplace flexibilities or remote options, I would take it."
reentry2_q2b = "If I found a job elsewhere with more pay or better benefits, I would take it."
reentry2_q3 = "My agency's reentry arrangements are fair in accounting for employees' diverse needs and situations."
engagement2_q1 = "The Federal Government is the best place to work for those who want to make a difference."
engagement2_q2 = "My direct supervisor shows very little interest in the feelings of subordinates."
engagement2_q3 = "My workload is reasonable."
engagement2_q4 = "I feel exhausted in the morning at the thought of another workday."
equity2_q1 = "Agency leadership shows that diversity and inclusion is important through their actions."
equity2_q2 = "I have to hide parts of my identity to be successful at work."
equity2_q3 = "People on my team listen to me, even when my views are dissimilar."

# question-model mapping
q_map_2 = hash::hash()

q_map_2[[reentry2_q1]] = "REENTRY2_Q1"
q_map_2[[reentry2_q2a]] = "REENTRY2_Q2A"
q_map_2[[reentry2_q2b]] = "REENTRY2_Q2B"
q_map_2[[reentry2_q3]] = "REENTRY2_Q3"
q_map_2[[engagement2_q1]] = "ENGAGEMENT2_Q1"
q_map_2[[engagement2_q2]] = "ENGAGEMENT2_Q2"
q_map_2[[engagement2_q3]] = "ENGAGEMENT2_Q3"
q_map_2[[engagement2_q4]] = "ENGAGEMENT2_Q4"
q_map_2[[equity2_q1]] = "EQUITY2_Q1"
q_map_2[[equity2_q2]] = "EQUITY2_Q2"
q_map_2[[equity2_q3]] = "EQUITY2_Q3"

# label-var_name mapping
var_map <- pulse::get_var_map()
# 
# var_map = hash::hash()
# 
# var_map[['Age']] = "age_factor"
# var_map[['Race']] = "race_factor"
# var_map[['Ethnicity']] = "ethnicity_factor"
# var_map[['Gender']] = "GENDER"
# var_map[['Disability Status']] = "disability_factor"
# var_map[['Education']] = "education_factor"
# var_map[['Length of Service']] = "length_of_service_factor"
# var_map[['Veteran Status']] = "veteran_status_factor"
# var_map[['Agency']] = "AGENCY"
# var_map[['Telework Eligibility']] = "telework_eligibility_factor"
# var_map[['Supervisor Status']] = "supervisor_status_factor"
# var_map[['Job Series']] = 'job_series_factor'
# var_map[['Location']] = 'location_factor'
# var_map[['Citizenship']] = 'US_CITIZENSHIP_IND'

#### GENERATE SHINY APP ####

# Format Shiny App
ui_2 <- navbarPage("Pulse 2 Descriptive Analysis",
                 tabPanel("Avg Response by Question - Employee Characteristics",
                          fluidPage(
                            # App title ----
                            titlePanel("Employee Voice Initiative"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("Question",
                                            label = "Select survey question",
                                            list('Engagement' = c(engagement2_q1, engagement2_q2, engagement2_q4),
                                                 'Reentry' = c(reentry2_q1, reentry2_q2a, reentry2_q2b, reentry2_q3),
                                                 'Equity and Inclusion' = c(equity2_q1, equity2_q2, equity2_q3),
                                                 'FEVS Question' = c(engagement2_q3, ""))),
                                selectInput("Variable",
                                            label = "Select Variable of Interest",
                                            #I pass in the vector of covariate labels from above
                                            covariate_labels),
                                br(),
                                br(),
                                br(),
                                "This survey is an interagency collaboration",
                                br(),
                                br(),
                                div(style="display:inline-block;",img(src = "omb_logo.png", height=100, width=100,style="left;"),
                                    img(src="opm_logo.png", height=100, width=100, style="right;")),
                                br(),
                                br(),
                                div(style="display:inline-block;",img(src = "gsa_logo.png", height=100, width=100,style="left;"),
                                    img(src="oes_logo.png", height=100, width=100, style="right;"))
                              ),
                              mainPanel(
                                h4("Adjusted Average Response for the Selected Question, by Selected Characteristic"),
                                # Output: Bar Plot ----
                                plotOutput(outputId = "barPlot"),
                                br(),
                                br(),
                                br(),
                                h6(tags$a(href="https://peoplelab.berkeley.edu/", "Designed and analyzed with support from The People Lab"),align="right"),
                                br(),
                                img(src='peoplelab-logo.png', height=50, width=150,align = "right")
                              )
                            )
                          )
                 )
)

# Define server logic required to draw plots ----
server_2 <- function(input, output) {
  
  output$barPlot <- renderPlot({
    
    # I pass in the selected question and the selected variable to filter the df
    selectedQ = q_map_2[[input$Question]]
    selectedVar = var_map[[input$Variable]]
    df_plot = model_df_2 %>% filter(model==selectedQ & variable==selectedVar)
    
    # I use the filtered data frame to generate the plot for the selected Q and Variable
    ggplot(df_plot, 
           aes(x=reorder(variable_levels, -emmean), y=emmean, 
               fill=factor(ifelse(variable_levels=='USG-wide', 'Highlight', "skyblue")))) + 
      geom_bar(stat='identity') + 
      #geom_line() +
      geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0.4, color='orange', alpha = 0.9, size = 1.3) +
      xlab("Selected Employee Characteristic") + ylab("Average Level of Agreement") +
      coord_flip() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14),
            axis.text.x  = element_text(size=16),
            axis.text.y = element_text(size=12),
            legend.position = "none") +
      scale_fill_manual(name= 'variable_levels', values = c('Dark Blue','Light Blue'))
    
  })
  
}

shinyApp(ui = ui_2, server = server_2)

## Write out EMMs and plain means for outside analysis
write.csv(model_df_2, "D:/nmiller/pulse_2_analysis/pulse_2_emms.csv")
write.csv(avg_df_2, "D:/nmiller/pulse_2_analysis/pulse_2_means.csv")
