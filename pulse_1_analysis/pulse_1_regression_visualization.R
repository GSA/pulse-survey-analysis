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

models = list(engagement1_q1_full_wt, engagement1_q2_full_wt, engagement1_q3_full_wt,
              equity1_q1_full_wt, equity1_q2_full_wt, equity1_q3_full_wt, equity1_q4_full_wt,
              reentry1_q1_full_wt, reentry1_q2_full_wt, reentry1_q3_full_wt)

# loop over questions and covariates to produce means
# and store in an output data frame

groups <- pulse::get_groups()

question_codes <- c("ENGAGEMENT1_Q1", "ENGAGEMENT1_Q2", "ENGAGEMENT1_Q3",
                    "EQUITY1_Q1", "EQUITY1_Q2", "EQUITY1_Q3", "EQUITY1_Q4",
                    "REENTRY1_Q1", "REENTRY1_Q2", "REENTRY1_Q3")

avg_df <- pulse::get_government_wide_means(analysis_dataset, question_codes, groups)

model_df <- pulse::get_government_wide_adjusted_means(models)

#### Create R Shiny visualization ----
covariate_labels <- pulse::get_covariate_labels()

#### Mappings of questions and variables ####
reentry1 = "I trust agency leadership to do whats right to protect employees' health, safety, and wellbeing."
reentry2 = "I trust my supervisor will help me navigate the reentry transition."
reentry3 = "If I found a job elsewhere with more workplace flexibilities or remote options, I would take it."
engagement1 = "I have the support I need to do my job well."
engagement2 = "There is someone at work I can talk to about my day-to-day problems if I need to."
engagement3 = "I feel exhausted in the morning at the thought of another day at work."
equity1 = "Agency leadership shows that diversity and inclusion are important through their actions."
equity2 = "If I found a job elsewhere with the same pay and benefits as this one, I would take it."
equity3 = "My workload is reasonable."
equity4 = "I feel that maybe I don't belong in my agency."


# question-model mapping
q_map = hash::hash()

q_map[[reentry1]] = "REENTRY1_Q1"
q_map[[reentry2]] = "REENTRY1_Q2"
q_map[[reentry3]] = "REENTRY1_Q3"
q_map[[engagement1]] = "ENGAGEMENT1_Q1"
q_map[[engagement2]] = "ENGAGEMENT1_Q2"
q_map[[engagement3]] = "ENGAGEMENT1_Q3"
q_map[[equity1]] = "EQUITY1_Q1"
q_map[[equity2]] = "EQUITY1_Q2"
q_map[[equity3]] = "EQUITY1_Q3"
q_map[[equity4]] = "EQUITY1_Q4"

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
ui <- navbarPage("Pulse 1 Descriptive Analysis",
                 tabPanel("Avg Response by Question - Employee Characteristics",
                          fluidPage(
                            # App title ----
                            titlePanel("Employee Voice Initiative"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("Question",
                                            label = "Select survey question",
                                            list('Engagement' = c(engagement1, engagement2, engagement3),
                                                 'Reentry' = c(reentry1, reentry2, reentry3),
                                                 'Equity and Inclusion' = c(equity1, equity2, equity4),
                                                 'FEVS Question' = c(equity3, ""))),
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
server <- function(input, output) {
  
  output$barPlot <- renderPlot({
    
    # I pass in the selected question and the selected variable to filter the df
    selectedQ = q_map[[input$Question]]
    selectedVar = var_map[[input$Variable]]
    df_plot = model_df %>% filter(model==selectedQ & variable==selectedVar)
    
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

#shinyApp(ui = ui, server = server)

## Write out EMMs and plain means for outside analysis
write.csv(model_df, "pulse_1_emms.csv")
write.csv(avg_df, "pulse_1_means.csv")
