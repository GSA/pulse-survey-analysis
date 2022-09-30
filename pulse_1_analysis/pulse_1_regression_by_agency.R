library(dplyr)
library(broom)
library(stargazer)
library(Hmisc)
library(tidyr)
library(gtools)

question_codes <- c("ENGAGEMENT1_Q1", "ENGAGEMENT1_Q2", "ENGAGEMENT1_Q3",
                    "EQUITY1_Q1", "EQUITY1_Q2", "EQUITY1_Q3", "EQUITY1_Q4",
                    "REENTRY1_Q1", "REENTRY1_Q2", "REENTRY1_Q3")

# weighting_impact <- rbind(
#   analysis_dataset %>% 
#     summarize(across(all_of(question_codes), mean, na.rm = T)),
#   analysis_dataset %>%
#     summarize(across(all_of(question_codes), ~weighted.mean(., w = wt, na.rm = T))),
#   analysis_dataset %>%
#     summarize(across(all_of(question_codes), sd, na.rm = T))

analysis_dataset %>% group_by(AGENCY) %>% summarise(across(all_of(question_codes),
                                                          list(mean = ~ mean(., na.rm = T),
                                                               wt.mean = ~ weighted.mean(., w = wt, na.rm = T),
                                                               diff = ~ abs(mean(., na.rm = T) - weighted.mean(., w = wt, na.rm = T))))) %>%
  View()

groups <- c(quo(age_factor), quo(race_factor), quo(ethnicity_factor),
            quo(GENDER), quo(disability_factor), quo(education_factor),
            quo(length_of_service_factor), quo(veteran_status_factor), quo(AGENCY),
            quo(telework_eligibility_factor), quo(supervisor_status_factor),
            quo(job_series_factor), quo(location_factor), quo(US_CITIZENSHIP_IND))

analysis_dataset %>% group_by(AGENCY, race_factor) %>% summarise(n = n(), n_resp = sum(RESPONSE)) %>% View()

analysis_dataset <- analysis_dataset %>%
  mutate(race_factor_combined = if_else(
    race_factor %in% c('Hawaiian Pacific Islander', 'American Indian', 'Two or more races'), 
    'Pacific Islander, American Indian, or more than one race',
    as.character(race_factor)))

analysis_dataset %>% group_by(AGENCY, race_factor_combined) %>% summarise(n = n(), n_resp = sum(RESPONSE)) %>% View()

analysis_dataset %>% group_by(AGENCY, education_factor) %>% summarise(n = n(), n_resp = sum(RESPONSE)) %>% View()

analysis_dataset <- analysis_dataset %>%
  mutate(education_factor_combined = if_else(
    education_factor %in% c('Less than High School', 'Trade or Technical Certificate', 
                            'Some college (no degree)', 'High School Diploma_GED or equivalent'), 
    'No college degree',
    as.character(education_factor)))

analysis_dataset %>% group_by(AGENCY, education_factor_combined) %>% summarise(n = n(), n_resp = sum(RESPONSE)) %>% View()

analysis_dataset %>% group_by(AGENCY, length_of_service_factor) %>% summarise(n = n(), n_resp = sum(RESPONSE)) %>% View()
# smallest group has 26 responses

analysis_dataset %>% group_by(AGENCY, age_factor) %>% summarise(n = n(), n_resp = sum(RESPONSE)) %>% View()
# Ages 25 and under have <15 respondents at ED, NSF, and USAID. NRC has <15 respondents 26-29

analysis_dataset <- analysis_dataset %>%
  mutate(age_factor_combined = if_else(
    age_factor %in% c('25 and under', '26-29 years old'), 
    'Under 30',
    as.character(age_factor)))
analysis_dataset %>% group_by(AGENCY, age_factor_combined) %>% summarise(n = n(), n_resp = sum(RESPONSE)) %>% View()

analysis_dataset <- analysis_dataset %>%
  mutate(component_no_na = if_else(is.na(COMPONENT), "Missing/NA", as.character(COMPONENT)))

# analysis_dataset %>% group_by(AGENCY, job_series_factor) %>% summarise(n = n(), n_resp = sum(RESPONSE)) %>% View()

# ...List out variables used for models ----

# education_factor or education_factor_combined depends on group size
# age_factor or age_factor combined depends on group size
# race_factor or race_factor_combined depends on group size
# COMPONENT only included if there's more than 1 value
basic_model_vars_a <- "total_annual_pay_imputed_5000 + ethnicity_factor + 
GENDER + disability_factor + length_of_service_factor + veteran_status_factor"

# Removed US_CITIZENSHIP_IND, which was in government-wide model,
# due to small sample size for multiple agencies
# inclusion of telework_eligibility_factor depends on whether both Y and N are present
additional_model_vars_a <- " + supervisor_status_factor + job_series_factor + location_factor"

interaction_effects <- " + race_factor:GENDER  + ethnicity_factor:GENDER + age_factor:GENDER"

# Function to create agency-specific weights ----

get_predictors <- function(x){
  min_ed_size <- x %>% group_by(education_factor) %>% 
    summarise(n_response = sum(RESPONSE)) %>%
    pull(n_response) %>%
    min()
  ed_var <- if_else(min_ed_size >= 1, "+ education_factor", "+ education_factor_combined")
  
  min_age_size <- x %>% group_by(age_factor) %>% 
    summarise(n_response = sum(RESPONSE)) %>%
    pull(n_response) %>%
    min()
  age_var <- if_else(min_age_size >= 1, "+ age_factor", "+ age_factor_combined")
  
  min_race_size <- x %>% group_by(race_factor) %>% 
    summarise(n_response = sum(RESPONSE)) %>%
    pull(n_response) %>%
    min()
  race_var <- if_else(min_race_size >= 1, "+ race_factor", "+ race_factor_combined")
  
  # If only 1 value for COMPONENT, then using it as a predictor gives an error
  component_var <- if_else(length(unique(x$COMPONENT)) > 1,
                           "+ component_no_na", "")
  
  # If only 1 value for telework_eligibility_factor, then using it as a predictor gives an error
  telework_var <- if_else(length(unique(x$telework_eligibility_factor)) > 1,
                          "+ telework_eligibility_factor", "")
  
  output <- gsub("[\r\n]", "", paste0(
    basic_model_vars_a, additional_model_vars_a, 
    component_var, telework_var, ed_var, age_var, race_var,
    race_var, ":GENDER"))
  return(output)
}

# Note: this function should be run after x has been filtered to a single agency
get_agency_weights <- function(x){
  
  if(length(unique(x$AGENCY)) > 1){
    warning("Multiple values for AGENCY in the input data frame")
  }
  
  predictor_string <- get_predictors(x)
  
  response_full_logistic <- glm(as.formula(paste0("RESPONSE ~ ", predictor_string)),
  data = x,
  family=binomial(link="logit"))
  
  # Set outliers (bottom 5% of fitted values) equal to 5th percentile
  # This prevents weights (the inverse of fitted values) from being too high
  fitted_values <- response_full_logistic$fitted.values
  fitted_values[fitted_values < quantile(fitted_values, .05)[[1]]] <- quantile(fitted_values, .05)[[1]]
  
  # Set weight as inverse of fitted value
  wt <- 1 / fitted_values
  wt_v <- c(stack(wt)$values)
  
  # Scale weights to be between 0 and 1
  wt_v <- wt_v / max(wt_v)
  
  return(wt_v)
}


# Function to get agency weighted and unweighted averages
get_agency_weighted_averages <- function(x, agency){
  agency_df <- x %>% filter(AGENCY == agency)
  weight_vector <- get_agency_weights(agency_df)
  agency_df$weights_col <- weight_vector
  
  agency_avg <- agency_df %>%
    summarise(across(all_of(question_codes),
                     list(mean = ~ mean(., na.rm = T),
                          wt.mean = ~ weighted.mean(., w = weights_col, na.rm = T),
                          diff = ~ abs(mean(., na.rm = T) - weighted.mean(., w = weights_col, na.rm = T)))))
  return(agency_avg)
}

agency_averages_df <- data.frame()

for(a in unique(analysis_dataset$AGENCY)){
  print(paste0("Calculating weighted and unweighted averages for ", a))
  agency_avg <- get_agency_weighted_averages(analysis_dataset, a) %>%
    mutate(agency = a) %>%
    relocate(agency)
  
  agency_averages_df <- rbind(agency_averages_df, agency_avg)
  
  rm(agency_avg)
}

#View(agency_averages_df)

# Function to run agency weighted regressions
run_agency_weighted_regression <- function(x, agency, column){
  agency_df <- x %>% filter(AGENCY == agency)
  agency_df$weights_col <- get_agency_weights(agency_df)
  
  no_na_response <- agency_df[!is.na(agency_df[,column]),]
  
  no_na_response <- droplevels(no_na_response)
  
  predictor_string <- get_predictors(no_na_response)
  
  full_model_wt <- lm(formula = paste0(column, " ~ ", predictor_string),
                      data = no_na_response,
                      weights = weights_col) 
  return(full_model_wt)
}

# agency_covariates = c('total_annual_pay_imputed_5000','age_factor','race_factor','ethnicity_factor',
#                       'GENDER','disability_factor','education_factor','length_of_service_factor',
#                       'veteran_status_factor','component_no_na','telework_eligibility_factor',
#                       'supervisor_status_factor','job_series_factor','location_factor')
# Does not currently include interaction effects
agency_covariates = c('age_factor','race_factor','ethnicity_factor', 'GENDER',
                      'disability_factor','education_factor','length_of_service_factor',
                      'veteran_status_factor', 'supervisor_status_factor',
                      'job_series_factor','location_factor')

#create empty df to store regression-adjusted means
agency_emmeans_df = data.frame()

# loop over models and covariates to produce regression-adjusted means
# and store in output data frame (input to shiny app)
for (a in unique(analysis_dataset$AGENCY))
{
  print(paste0("Working on agency ", a))
  agency_df0 <- analysis_dataset %>%
    filter(AGENCY == a)
  agency_df0$weights_col <- get_agency_weights(agency_df0)
  
  for (q in question_codes){
    model <- run_agency_weighted_regression(x = agency_df0, agency = a, column = q)
    
    model_name = as.character(model$terms[[2]])  #store outcome variable (question)
    
    for (x in agency_covariates){
      print(paste0("Calculating regression-adjusted mean for variable ", x))
            df_new = data.frame(emmeans(model, 
                                specs = x,
                                non.nuisance = x,
                                weights='proportional'))
      colnames(df_new)[1] = 'variable_levels'             #standardize term column
      df_new <- df_new %>% mutate(model = model_name,
                     agency = a,
                     variable = x)
      
      agency_emmeans_df = smartbind(agency_emmeans_df, df_new)
    }
  }
}

agency_emmeans_df <- agency_emmeans_df %>% select(-asymp.LCL, -asymp.UCL)

# Get agency weighted and unweighted means by demographic
agency_groups <- c(quo(age_factor), quo(race_factor), quo(ethnicity_factor),
            quo(GENDER), quo(disability_factor), quo(education_factor),
            quo(length_of_service_factor), quo(veteran_status_factor), quo(component_no_na),
            quo(telework_eligibility_factor), quo(supervisor_status_factor),
            quo(job_series_factor), quo(location_factor), quo(US_CITIZENSHIP_IND))
question_codes <- c("ENGAGEMENT1_Q1", "ENGAGEMENT1_Q2", "ENGAGEMENT1_Q3",
                    "EQUITY1_Q1", "EQUITY1_Q2", "EQUITY1_Q3", "EQUITY1_Q4",
                    "REENTRY1_Q1", "REENTRY1_Q2", "REENTRY1_Q3")

agency_avg_df <- data.frame()
for (a in unique(analysis_dataset$AGENCY)){

  print(paste0("Now working on agency ", a))
  agency_df0 <- analysis_dataset %>%
    filter(AGENCY == a)
  agency_df0$weights_col <- get_agency_weights(agency_df0)
  
  for (i in seq_along(agency_groups)){
  
  varName <- as.character(rlang::quo_get_expr(agency_groups[[i]]))
  
  df0 <- agency_df0 %>%
    group_by(!!agency_groups[[i]]) %>%
    summarise(across(all_of(question_codes), 
                     .fns = list(mean = ~mean(., na.rm = TRUE),
                                 weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                                 n.responses = ~round(sum(!is.na(.)), digits = 0),
                                 n.population = ~n(),
                                 sd = ~sqrt(var(., na.rm = TRUE)),
                                 weighted.sd = ~sqrt(wtd.var(., weights = wt, na.rm = TRUE))
                     ))) %>%
    mutate(variable = varName) %>%
    rename(variable_levels = 1) %>%
    pivot_longer(-c(variable, variable_levels),
                 names_to = c("track", "q", "metric"),
                 names_sep = "_") %>%
    mutate(model = paste0(track, "_", q),
           agency = a) %>%
    select(-c(track, q)) 
  agency_avg_df <- rbind(agency_avg_df, df0)
  rm(df0)
  }
}

agency_avg_df_n <- agency_avg_df %>%
  filter(metric == 'n.responses') %>%
  select(-metric) %>%
  rename(n.responses = value)

agency_avg_df <- agency_avg_df %>%
  left_join(x = ., y = agency_avg_df_n, by = c("agency", "variable_levels", "variable", "model")) %>%
  filter(metric != 'n.responses') %>%
  relocate(agency) %>%
  relocate(n.responses, .after = last_col())

is.na(agency_avg_df$value) <- agency_avg_df$n.responses < 10
is.na(agency_avg_df$n.responses) <- agency_avg_df$n.responses < 10

agency_emmeans_df <- agency_emmeans_df %>%
  left_join(agency_avg_df_n, by = c("agency", "variable_levels", "variable", "model")) %>%
  relocate(agency, variable_levels, variable)

is.na(agency_emmeans_df$emmean) <- agency_emmeans_df$n.responses < 10
is.na(agency_emmeans_df$SE) <- agency_emmeans_df$n.responses < 10
is.na(agency_emmeans_df$lower.CL) <- agency_emmeans_df$n.responses < 10
is.na(agency_emmeans_df$upper.CL) <- agency_emmeans_df$n.responses < 10
is.na(agency_emmeans_df$n.responses) <- agency_emmeans_df$n.responses < 10

agency_avg_df %>%
  left_join(stack(q_map), by = c("model" = "values")) %>%
  rename(question_text = ind) %>% arrange(agency, model, metric, variable) %>%
  write.csv("D:/nmiller/pulse_1_analysis/agency_pulse_1_means.csv", row.names = F)

agency_emmeans_df %>%
  left_join(stack(q_map), by = c("model" = "values")) %>%
  rename(question_text = ind) %>% arrange(agency, model, variable, variable_levels) %>%
  write.csv("D:/nmiller/pulse_1_analysis/agency_pulse_1_emms.csv", row.names = F)

agency_covariate_labels = c('Age','Race','Ethnicity','Gender','Disability Status','Education',
                     'Length of Service','Veteran Status', 'Component', 'Telework Eligibility',
                     'Supervisor Status','Job Series','Location','Citizenship')

# ui_a <- navbarPage("Pulse 1 Descriptive Analysis by Agency",
#                  tabPanel("Avg Response by Question - Employee Characteristics",
#                           fluidPage(
#                             # App title ----
#                             titlePanel("Employee Voice Initiative"),
#                             
#                             sidebarLayout(
#                               sidebarPanel(
#                                 selectInput("Agency",
#                                             label = "Select agency",
#                                             unique(agency_emmeans_df$agency)),
#                                 selectInput("Question",
#                                             label = "Select survey question",
#                                             list('Engagement' = c(engagement1, engagement2, engagement3),
#                                                  'Reentry' = c(reentry1, reentry2, reentry3),
#                                                  'Equity and Inclusion' = c(equity1, equity2, equity4),
#                                                  'FEVS Question' = c(equity3, ""))),
#                                 selectInput("Variable",
#                                             label = "Select variable of interest",
#                                             #I pass in the vector of covariate labels from above
#                                             agency_covariate_labels),
#                                                                 br(),
#                                 br(),
#                                 br(),
#                                 "This survey is an interagency collaboration",
#                                 br(),
#                                 br(),
#                                 div(style="display:inline-block;",img(src = "omb_logo.png", height=100, width=100,style="left;"),
#                                     img(src="opm_logo.png", height=100, width=100, style="right;")),
#                                 br(),
#                                 br(),
#                                 div(style="display:inline-block;",img(src = "gsa_logo.png", height=100, width=100,style="left;"),
#                                     img(src="oes_logo.png", height=100, width=100, style="right;"))
#                               ),
#                               mainPanel(
#                                 h4("Adjusted Average Response for the Selected Question, by Selected Characteristic"),
#                                 # Output: Bar Plot ----
#                                 plotOutput(outputId = "barPlot"),
#                                 br(),
#                                 br(),
#                                 br(),
#                                 h6(tags$a(href="https://peoplelab.berkeley.edu/", "Designed and analyzed with support from The People Lab"),align="right"),
#                                 br(),
#                                 img(src='peoplelab-logo.png', height=50, width=150,align = "right")
#                               )
#                             )
#                           )
#                  )
# )

# Define server logic required to draw plots ----
# server_a <- function(input, output) {
#   
#   output$barPlot <- renderPlot({
#     
#     # I pass in the selected question and the selected variable to filter the df
#     selectedQ = q_map[[input$Question]]
#     selectedVar = var_map[[input$Variable]]
#     df_plot = agency_emmeans_df %>% filter(model==selectedQ & variable==selectedVar & agency == input$Agency)
#     
#     # I use the filtered data frame to generate the plot for the selected Q and Variable
#     ggplot(df_plot, 
#            aes(x=reorder(variable_levels, -emmean), y=emmean, 
#                fill=factor(ifelse(variable_levels=='USG-wide', 'Highlight', "skyblue")),
#                label = n.responses)) +
#       geom_text(hjust = -0.5, vjust = -0.5) +
#       geom_bar(stat='identity') + 
#       #geom_line() +
#       geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0.4, color='orange', alpha = 0.9, size = 1.3) +
#       xlab("Selected Employee Characteristic") + ylab("Average Level of Agreement") +
#       coord_flip() +
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#             panel.background = element_blank(), axis.line = element_line(colour = "black"),
#             axis.title.x = element_text(size=14),
#             axis.title.y = element_text(size=14),
#             axis.text.x  = element_text(size=16),
#             axis.text.y = element_text(size=12),
#             legend.position = "none") +
#       scale_fill_manual(name= 'variable_levels', values = c('Dark Blue','Light Blue'))
#     
#   })
#   
# }
# 
# shinyApp(ui = ui_a, server = server_a)

# agency_avg_df %>% filter(metric == "weighted.mean", variable == "age_factor", model == "ENGAGEMENT1_Q1", variable_levels %in% c("60 or older", "50-59 years old", "26-29 years old", "30-39 years old")) %>% arrange(agency, desc(value)) %>% View()
agency_avg_df %>% filter(agency == "General Services Administration") %>% 
  left_join(stack(q_map), by = c("model" = "values")) %>%
  rename(question_text = ind) %>% arrange(agency, model, metric, variable) %>%
  write.csv("GSA_demographic_breakouts_pulse_1.csv", row.names = F)

  