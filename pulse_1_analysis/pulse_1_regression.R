# PRELIMINARY STEPS ----

library(dplyr)
library(stargazer)
library(Hmisc)
library(lmtest)
library(sandwich)

# ...Divide salary by 5000 to aid interpretation ----
analysis_dataset <- analysis_dataset %>%
  mutate(total_annual_pay_imputed_5000 = total_annual_pay_imputed / 5000)

# ...List out variables used for models ----
basic_model_vars <- "total_annual_pay_imputed_5000 + age_factor + race_factor + 
ethnicity_factor + GENDER + disability_factor + education_factor + 
length_of_service_factor + veteran_status_factor + AGENCY"

additional_model_vars <- "+ telework_eligibility_factor + supervisor_status_factor + 
job_series_factor + location_factor + US_CITIZENSHIP_IND"

interaction_effects <- " + race_factor:GENDER  + ethnicity_factor:GENDER + age_factor:GENDER"

# gsub("[\r\n]", "", paste0(basic_model_vars, additional_model_vars))

# ...Relevel variables to make interpretation of coefficients easier ----
analysis_dataset <- pulse::relevel_factors(analysis_dataset)

# analysis_dataset$age_factor <- relevel(analysis_dataset$age_factor, "50-59 years old")
# analysis_dataset$race_factor <- relevel(analysis_dataset$race_factor, "White")
# analysis_dataset$ethnicity_factor <- relevel(analysis_dataset$ethnicity_factor, "Not Hispanic")
# analysis_dataset$GENDER <- droplevels(analysis_dataset$GENDER) # Remove unused level '*'
# analysis_dataset$GENDER <- relevel(analysis_dataset$GENDER, "M")
# analysis_dataset$disability_factor <- relevel(analysis_dataset$disability_factor, "No disability identified")
# analysis_dataset$education_factor <- relevel(analysis_dataset$education_factor, "Bachelor's Degree")
# analysis_dataset$length_of_service_factor <- relevel(analysis_dataset$length_of_service_factor, "More than 20 years")
# analysis_dataset$veteran_status_factor <- relevel(analysis_dataset$veteran_status_factor, "Not a veteran")
# analysis_dataset$AGENCY <- relevel(analysis_dataset$AGENCY, "Department of Defense")
# 
# analysis_dataset$telework_eligibility_factor <- relevel(analysis_dataset$telework_eligibility_factor, "N")
# analysis_dataset$supervisor_status_factor <- relevel(analysis_dataset$supervisor_status_factor, "Not supervisor, manager, or team leader")
# analysis_dataset$job_series_factor <- relevel(analysis_dataset$job_series_factor, "Non-mission support job series")
# analysis_dataset$location_factor <- relevel(analysis_dataset$location_factor, "Other duty station")
# analysis_dataset$US_CITIZENSHIP_IND <- relevel(as.factor(analysis_dataset$US_CITIZENSHIP_IND), "Y")

# WRITE FUNCTION TO RUN MODELS AND PRINT COEFFICIENTS ----
run_full_model_wt <- function(x, column, weight_vector, question_text){
  x$weights_col <- weight_vector
  no_na_response <- x[!is.na(x[,column]),]
  
  basic_model <- lm(formula = paste0(column, " ~ ", (gsub("[\r\n]", "", basic_model_vars))),
                    data = no_na_response)
  full_model <- lm(formula = paste0(column, " ~ ", gsub("[\r\n]", "", paste0(basic_model_vars, additional_model_vars, interaction_effects))),
                   data = no_na_response)
  basic_model_wt <- lm(formula = paste0(column, " ~ ", (gsub("[\r\n]", "", basic_model_vars))),
                       data = no_na_response,
                       weights = weights_col)
  full_model_wt <- lm(formula = paste0(column, " ~ ", gsub("[\r\n]", "", paste0(basic_model_vars, additional_model_vars, interaction_effects))),
                      data = no_na_response,
                      weights = weights_col)
  # The gsub for "[\r\n]" removes the newline markers, \n
  
  sink(file = paste0(column, '_all_models.txt'))
  stargazer(basic_model, full_model, basic_model_wt, full_model_wt,
            type = "text",
            column.labels = c("Unweighted basic model", "Unweighted full model",
                              "Weighted basic model", "Weighted full model"),
            title = question_text)
  sink()

  return(full_model_wt)
}

# WRITE FUNCTION TO RUN LOGISTIC MODEL ABOUT EACH QUESTION ----
run_logistic_model_q <- function(x, column, 
                                 survey_track = c("reentry", "engagement", "equity"),
                                 question_text){
  
  sent_survey <- analysis_dataset %>%
    filter(SURVEY_TRACK == survey_track)
  
  sent_survey$q_response <- if_else(is.na(sent_survey[,column]), 0, 1)

  q_response_logit <- glm(formula = paste0("q_response ~ ", gsub("[\r\n]", "", paste0(basic_model_vars, additional_model_vars, interaction_effects))),
                      data = sent_survey,
                      family=binomial(link="logit"))
  sink(file = paste0(column, '_response_model.txt'))
  stargazer(q_response_logit,
            type = "text",
            title = paste0("Response to q: ", question_text))
  sink()
  return(q_response_logit)
}

# analysis_dataset %>%
#   filter(!is.na(REENTRY1_Q1), COMPONENT == 'Transportation Security Administration') %>%
#   group_by(GENDER, length_of_service_factor) %>%
#   summarise(n = n(),
#             a = mean(REENTRY1_Q1),
#             pct_agree = sum(REENTRY1_Q1 %in% c(4,5)) / n(),
#             pct_disagree = sum(REENTRY1_Q1 %in% c(1,2)) / n)


# WEIGHTING ----
# ...Run unweighted logistic regression about response ----
response_full_logistic <- glm(as.formula(paste0("RESPONSE ~ ", (gsub("[\r\n]", "", paste0(
  basic_model_vars, additional_model_vars, "+ race_factor:GENDER"))))),
                              data = analysis_dataset,
                              family=binomial(link="logit"))

# ...Create weights as inverse of likelihood of response ----
summary(response_full_logistic$fitted.values)
length(response_full_logistic$fitted.values) # Same as length of analysis dataset

wt <- 1 / response_full_logistic$fitted.values
wt_v <- c(stack(wt)$values)
rm(wt)

quantile(wt_v, c(0.01, .9, .95, .995))

# Scale model to range of 0-1, after replacing outliers (above 95th percentile,
# which is currently 22.2); maximum weight without replacing outliers is 126
wt_v[wt_v > quantile(wt_v, .95)[[1]]] <- quantile(wt_v, .95)[[1]]
wt_v <- (wt_v / max(wt_v))

#  ...Compare population distribution and weighted response distribution of covariates ----
analysis_dataset$wt <- wt_v
rm(wt_v)

# age_dist <- analysis_dataset %>% group_by(age_factor) %>%
#   summarise(n_sent = n(),
#             n_response = sum(RESPONSE),
#             sum_weighted_responses = sum(wt[RESPONSE == 1])) %>%
#   mutate(pct_sent = n_sent / sum(n_sent),
#          pct_response = n_response / sum(n_response),
#          pct_weighted_responses = sum_weighted_responses / sum(sum_weighted_responses)) %>%
#   select(age_factor, pct_sent, pct_response, pct_weighted_responses) %>%
#   arrange(desc(pct_sent))

groups <- c(quo(age_factor), quo(race_factor), quo(ethnicity_factor),
            quo(GENDER), quo(disability_factor), quo(education_factor),
            quo(length_of_service_factor), quo(veteran_status_factor), quo(AGENCY),
            quo(telework_eligibility_factor), quo(supervisor_status_factor),
            quo(job_series_factor), quo(location_factor), quo(US_CITIZENSHIP_IND))

for (i in seq_along(groups)){
  analysis_dataset %>%
    group_by(!!groups[[i]]) %>%
    summarise(n_sent = n(),
              n_response = sum(RESPONSE),
              sum_weighted_responses = sum(wt[RESPONSE == 1])) %>%
    mutate(pct_sent = n_sent / sum(n_sent),
           pct_response = n_response / sum(n_response),
           pct_weighted_responses = sum_weighted_responses / sum(sum_weighted_responses)) %>%
    select(!!groups[[i]], pct_sent, pct_response, pct_weighted_responses) %>%
    arrange(desc(pct_sent)) %>%
    print()
}

agency_weights <- analysis_dataset %>%
  group_by(AGENCY) %>%
  summarise(n_sent = n(),
            n_response = sum(RESPONSE),
            sum_weighted_responses = sum(wt[RESPONSE == 1])) %>%
  mutate(pct_sent = n_sent / sum(n_sent),
         pct_response = n_response / sum(n_response),
         pct_weighted_responses = sum_weighted_responses / sum(sum_weighted_responses)) %>%
  select(AGENCY, pct_sent, pct_response, pct_weighted_responses) %>%
  arrange(desc(pct_sent))

for (i in seq_along(groups)){
  analysis_dataset %>%
    group_by(!!groups[[i]]) %>%
    summarise(n_sent = n(),
              n_responses = sum(!is.na(ENGAGEMENT1_Q1)),
              average = mean(ENGAGEMENT1_Q1, na.rm = TRUE),
              wt_average = weighted.mean(ENGAGEMENT1_Q1, w = wt, na.rm = T)) %>%
    mutate(variable = groups[[i]]) %>%
    print()
}
# ...Check need for interaction effects in weighting ----
# We looked at group_by(age_factor, GENDER); group_by(race_factor, GENDER);
# group_by(ethnicity_factor, GENDER)

analysis_dataset %>%
  group_by(race_factor, GENDER) %>%
  summarise(n_sent = n(),
            n_response = sum(RESPONSE),
            sum_weighted_responses = sum(wt[RESPONSE == 1]),
           .groups = "drop") %>%
  mutate(pct_sent = n_sent / sum(n_sent),
         pct_response = n_response / sum(n_response),
         pct_weighted_responses = sum_weighted_responses / sum(sum_weighted_responses)) %>%
  select(race_factor, GENDER, pct_sent, pct_response, pct_weighted_responses) %>%
  arrange(desc(pct_sent))

# When checking weights created without interaction effects, Black and
# African American  men were underrepresented. Therefore, interaction between 
# race and gender are now included in the model used to construct weights

# ...Look at impact of weights on government-wide responses ----
question_codes <- c("ENGAGEMENT1_Q1", "ENGAGEMENT1_Q2", "ENGAGEMENT1_Q3",
                    "EQUITY1_Q1", "EQUITY1_Q2", "EQUITY1_Q3", "EQUITY1_Q4",
                    "REENTRY1_Q1", "REENTRY1_Q2", "REENTRY1_Q3")

weighting_impact <- rbind(
  analysis_dataset %>% 
    summarise(across(all_of(question_codes), mean, na.rm = T)),
  analysis_dataset %>%
    summarise(across(all_of(question_codes), ~weighted.mean(., w = wt, na.rm = T))),
  analysis_dataset %>%
    summarise(across(all_of(question_codes), sd, na.rm = T))
)
  row.names(weighting_impact) <- c("Unweighted government-wide average",
                    "Weighted government-wide average",
                    "Standard deviation") 
  write.csv(weighting_impact, file = "weighting_impact.csv")


# WEIGHTED GOVERNMENT-WIDE REGRESSIONS ----

# ...Weighted and unweighted logistic models of response ----

# response_basic_logistic <- glm(as.formula(gsub("[\r\n]", "", paste0("RESPONSE ~ ", basic_model_vars))),
#                         data = analysis_dataset,
#                         family = binomial(link="logit"))
# 
# response_basic_logistic_wt <- glm(as.formula(gsub("[\r\n]", "", paste0("RESPONSE ~ ", basic_model_vars))),
#                                   data = analysis_dataset,
#                                   family = binomial(link="logit"),
#                                   weights = wt)
  
response_full_logistic_wt <- glm(as.formula(gsub("[\r\n]", "", paste0("RESPONSE ~ ", basic_model_vars, additional_model_vars, interaction_effects))),
                                  data = analysis_dataset,
                                  family = binomial(link="logit"),
                                  weights = wt)

# sink(file = 'RESPONSE_all_models.txt')
# stargazer(response_basic_logistic, response_full_logistic, 
#           response_basic_logistic_wt, response_full_logistic_wt, type = "text",
#           column.labels = c("Unweighted basic model", "Unweighted full model (used to construct weights)",
#                             "Weighted basic model", "Weighted full model"),
#           title = "Response - completed survey or not, logistic")
# sink()

# ...Weighted Engagement1_Q1: I have the support I need to do my job well. ----
# I have the support I need to do my job well.

engagement1_q1_full_wt <- run_full_model_wt(analysis_dataset, "ENGAGEMENT1_Q1",
                                   weight_vector = analysis_dataset$wt,
                                   question_text = "I have the support I need to do my job well.")

# engagement1_q1_response_logit <- run_logistic_model_q(analysis_dataset, "ENGAGEMENT1_Q1", 
#                                 survey_track = "engagement",
#                                 question_text = "I have the support I need to do my job well.")

# ...Weighted Engagement1_Q2: There is someone I can talk to about my day-to-day problems if I need to. ----
# There is someone I can talk to about my day-to-day problems if I need to.

engagement1_q2_full_wt <- run_full_model_wt(analysis_dataset, "ENGAGEMENT1_Q2",
                                            weight_vector = analysis_dataset$wt,
                                            question_text = "There is someone I can talk to about my day-to-day problems if I need to.")
# engagement1_q2_response_logit <- run_logistic_model_q(analysis_dataset, "ENGAGEMENT1_Q2", 
#                                                       survey_track = "engagement",
#                                                       question_text = "There is someone I can talk to about my day-to-day problems if I need to.")

# ...Weighted Engagement1_Q3: I feel exhausted in the morning at the thought of another day at work. ----
# I feel exhausted in the morning at the thought of another day at work.

engagement1_q3_full_wt <- run_full_model_wt(analysis_dataset, "ENGAGEMENT1_Q3",
                                            weight_vector = analysis_dataset$wt,
                                            question_text = "I feel exhausted in the morning at the thought of another day at work.")
# engagement1_q3_response_logit <- run_logistic_model_q(analysis_dataset, "ENGAGEMENT1_Q3", 
#                                                       survey_track = "engagement",
#                                                       question_text = "I feel exhausted in the morning at the thought of another day at work.")

# ...Weighted Equity1_Q1: Agency leadership shows that diversity and inclusion are important through their actions. ----
# Agency leadership shows that diversity and inclusion are important through their actions. 

equity1_q1_full_wt <- run_full_model_wt(analysis_dataset, "EQUITY1_Q1",
                                            weight_vector = analysis_dataset$wt,
                                            question_text = "Agency leadership shows that diversity and inclusion are important through their actions.")

# equity1_q1_response_logit <- run_logistic_model_q(analysis_dataset, "EQUITY1_Q1", 
#                                                   survey_track = "equity",
#                                                   question_text = "Agency leadership shows that diversity and inclusion are important through their actions.")

# ...Weighted Equity1_Q2: If I found a job elsewhere with the same pay and benefits as this one I would take it. ----
# If I found a job elsewhere with the same pay and benefits as this one I would take it. 

equity1_q2_full_wt <- run_full_model_wt(analysis_dataset, "EQUITY1_Q2",
                                        weight_vector = analysis_dataset$wt,
                                        question_text = "If I found a job elsewhere with the same pay and benefits as this one I would take it.")

# equity1_q2_response_logit <- run_logistic_model_q(analysis_dataset, "EQUITY1_Q2", 
#                                                   survey_track = "equity",
#                                                   question_text = "If I found a job elsewhere with the same pay and benefits as this one I would take it.")


# ...Weighted Equity1_Q3: My workload is reasonable. ----
# My workload is reasonable.

equity1_q3_full_wt <- run_full_model_wt(analysis_dataset, "EQUITY1_Q3",
                                        weight_vector = analysis_dataset$wt,
                                        question_text = "My workload is reasonable.")

# equity1_q3_response_logit <- run_logistic_model_q(analysis_dataset, "EQUITY1_Q3", 
#                                                   survey_track = "equity",
#                                                   question_text = "My workload is reasonable.")


# ...Weighted Equity1_Q4: I feel that maybe I don't belong in my agency. ----
# I feel that maybe I don't belong in my agency.
equity1_q4_full_wt <- run_full_model_wt(analysis_dataset, "EQUITY1_Q4",
                                        weight_vector = analysis_dataset$wt,
                                        question_text = "I feel that maybe I don't belong in my agency.")

# equity1_q4_response_logit <- run_logistic_model_q(analysis_dataset, "EQUITY1_Q4", 
#                                                   survey_track = "equity",
#                                                   question_text = "I feel that maybe I don't belong in my agency.")

# ...Weighted Reentry1_Q1: I trust Agency leadership to do what's right to protect employees' health safety and wellbeing. ----
# I trust Agency leadership to do what's right to protect employees' health safety and wellbeing.

reentry1_q1_full_wt <- run_full_model_wt(analysis_dataset, "REENTRY1_Q1",
                                        weight_vector = analysis_dataset$wt,
                                        question_text = "I trust Agency leadership to do what's right to protect employees' health, safety and wellbeing.")

# reentry1_q1_response_logit <- run_logistic_model_q(analysis_dataset, "REENTRY1_Q1", 
#                                                   survey_track = "reentry",
#                                                   question_text = "I trust Agency leadership to do what's right to protect employees' health, safety and wellbeing.")


# ...Weighted Reentry1_Q2: I trust my supervisor will help me navigate the reentry transition. ----
# I trust my supervisor will help me navigate the reentry transition.

reentry1_q2_full_wt <- run_full_model_wt(analysis_dataset, "REENTRY1_Q2",
                                         weight_vector = analysis_dataset$wt,
                                         question_text = "I trust my supervisor will help me navigate the reentry transition.")
# reentry1_q2_response_logit <- run_logistic_model_q(analysis_dataset, "REENTRY1_Q2", 
#                                                    survey_track = "reentry",
#                                                    question_text = "I trust my supervisor will help me navigate the reentry transition.")

# ...Weighted Reentry1_Q3: If I found a job elsewhere with more workplace flexibilities or remote options I would take it.----
# If I found a job elsewhere with more workplace flexibilities or remote options I would take it.

reentry1_q3_full_wt <- run_full_model_wt(analysis_dataset, "REENTRY1_Q3",
                                         weight_vector = analysis_dataset$wt,
                                         question_text = "If I found a job elsewhere with more workplace flexibilities or remote options I would take it.")
# reentry1_q3_response_logit <- run_logistic_model_q(analysis_dataset, "REENTRY1_Q3", 
#                                                    survey_track = "reentry",
#                                                    question_text = "If I found a job elsewhere with more workplace flexibilities or remote options I would take it.")

# Print out all response-level logits together ----
# 
# sink(file = 'all_question_response_model.txt')
# stargazer(engagement1_q1_response_logit, 
#           engagement1_q2_response_logit,
#           engagement1_q3_response_logit,
#           equity1_q1_response_logit, 
#           equity1_q2_response_logit,
#           equity1_q3_response_logit,
#           equity1_q4_response_logit,
#           reentry1_q1_response_logit, 
#           reentry1_q2_response_logit,
#           reentry1_q3_response_logit,
#           type = "text",
#           column.labels = c("engagement1q1", "engagement1q2", "engagement1q3",
#                             "equity1q1", "equity1q2", "equity1q3",  "equity1q4",
#                             "reentry1q1", "reentry1q2", "reentry1q3"),
#           title = "Question-level response logistic regression")
# sink()