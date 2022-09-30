# PRELIMINARY STEPS ----

library(dplyr)
library(broom)
library(stargazer)
library(Hmisc)
library(lmtest)
library(sandwich)

# ...Drop agencies with low match rate ----
analysis_dataset_2 <- analysis_dataset_2 %>%
  mutate(AGENCY = droplevels(AGENCY))

# ...Divide salary by 5000 to aid interpretation ----
analysis_dataset_2 <- analysis_dataset_2 %>%
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
analysis_dataset_2 <- pulse::relevel_factors(analysis_dataset_2)
# 
# analysis_dataset_2$age_factor <- relevel(analysis_dataset_2$age_factor, "50-59 years old")
# analysis_dataset_2$race_factor <- relevel(analysis_dataset_2$race_factor, "White")
# analysis_dataset_2$ethnicity_factor <- relevel(analysis_dataset_2$ethnicity_factor, "Not Hispanic")
# analysis_dataset_2$GENDER <- droplevels(analysis_dataset_2$GENDER) # Remove unused level '*'
# analysis_dataset_2$GENDER <- relevel(analysis_dataset_2$GENDER, "M")
# analysis_dataset_2$disability_factor <- relevel(analysis_dataset_2$disability_factor, "No disability identified")
# analysis_dataset_2$education_factor <- relevel(analysis_dataset_2$education_factor, "Bachelor's Degree")
# analysis_dataset_2$length_of_service_factor <- relevel(analysis_dataset_2$length_of_service_factor, "More than 20 years")
# analysis_dataset_2$veteran_status_factor <- relevel(analysis_dataset_2$veteran_status_factor, "Not a veteran")
# analysis_dataset_2$AGENCY <- relevel(analysis_dataset_2$AGENCY, "Department of Defense")
# 
# analysis_dataset_2$telework_eligibility_factor <- relevel(analysis_dataset_2$telework_eligibility_factor, "N")
# analysis_dataset_2$supervisor_status_factor <- relevel(analysis_dataset_2$supervisor_status_factor, "Not supervisor, manager, or team leader")
# analysis_dataset_2$job_series_factor <- relevel(analysis_dataset_2$job_series_factor, "Non-mission support job series")
# analysis_dataset_2$location_factor <- relevel(analysis_dataset_2$location_factor, "Other duty station")
# analysis_dataset_2$US_CITIZENSHIP_IND <- relevel(as.factor(analysis_dataset_2$US_CITIZENSHIP_IND), "Y")

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
  # 
  # sink(file = paste0(column, '_full_model_summary_sg_wt.txt'))
  # print(paste0(column, " ~ ", basic_model_vars, additional_model_vars))
  # stargazer(full_model_wt, type = 'text',
  #           column.labels = column)
  # sink()
  
  return(full_model_wt)
}

# WRITE FUNCTION TO RUN LOGISTIC MODEL ABOUT EACH QUESTION ----
run_logistic_model_q <- function(x, column, 
                                 survey_track = c("reentry", "engagement", "equity"),
                                 question_text){
  
  sent_survey <- analysis_dataset_2 %>%
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

# analysis_dataset_2 %>%
#   filter(!is.na(REENTRY1_Q1), COMPONENT == 'Transportation Security Administration') %>%
#   group_by(GENDER, length_of_service_factor) %>%
#   summarise(n = n(),
#             a = mean(REENTRY1_Q1),
#             pct_agree = sum(REENTRY1_Q1 %in% c(4,5)) / n(),
#             pct_disagree = sum(REENTRY1_Q1 %in% c(1,2)) / n)


# WEIGHTING ----
# ...Run unweighted logistic regression about response ----
response_full_logistic_2 <- glm(as.formula(paste0("RESPONSE ~ ", (gsub("[\r\n]", "", paste0(
  basic_model_vars, additional_model_vars, "+ race_factor:GENDER"))))),
  data = analysis_dataset_2,
  family=binomial(link="logit"))

# ...Create weights as inverse of likelihood of response ----
summary(response_full_logistic_2$fitted.values)
length(response_full_logistic_2$fitted.values) # Same as length of analysis dataset

wt_2 <- 1 / response_full_logistic_2$fitted.values
wt_2_v <- c(stack(wt_2)$values)
rm(wt_2)

quantile(wt_2_v, c(0.01, .9, .95, .995))

# Scale model to range of 0-1, after replacing outliers (above 95th percentile,
# which is currently 22.2); maximum weight without replacing outliers is 126
wt_2_v[wt_2_v > quantile(wt_2_v, .95)[[1]]] <- quantile(wt_2_v, .95)[[1]]
wt_2_v <- (wt_2_v / max(wt_2_v))

#  ...Compare population distribution and weighted response distribution of covariates ----
analysis_dataset_2$wt <- wt_2_v
rm(wt_2_v)

# age_dist <- analysis_dataset_2 %>% group_by(age_factor) %>%
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
  analysis_dataset_2 %>%
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

agency_weights_2 <- analysis_dataset_2 %>%
  group_by(AGENCY) %>%
  summarise(n_sent = n(),
            n_response = sum(RESPONSE),
            sum_weighted_responses = sum(wt[RESPONSE == 1])) %>%
  mutate(pct_sent = n_sent / sum(n_sent),
         pct_response = n_response / sum(n_response),
         pct_weighted_responses = sum_weighted_responses / sum(sum_weighted_responses)) %>%
  select(AGENCY, pct_sent, pct_response, pct_weighted_responses) %>%
  arrange(desc(pct_sent))

# ...Check need for interaction effects in weighting ----
# We looked at group_by(age_factor, GENDER); group_by(race_factor, GENDER);
# group_by(ethnicity_factor, GENDER)

analysis_dataset_2 %>%
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
question_codes_2 <- c("ENGAGEMENT2_Q1", "ENGAGEMENT2_Q2", "ENGAGEMENT2_Q3",
                    "ENGAGEMENT2_Q4", "EQUITY2_Q1", "EQUITY2_Q2", "EQUITY2_Q3",
                    "REENTRY2_Q1", "REENTRY2_Q2A", "REENTRY2_Q2B","REENTRY2_Q3")

weighting_impact_2 <- rbind(
  analysis_dataset_2 %>% 
    summarise(across(all_of(question_codes_2), mean, na.rm = T)),
  analysis_dataset_2 %>%
    summarise(across(all_of(question_codes_2), ~weighted.mean(., w = wt, na.rm = T))),
  analysis_dataset_2 %>%
    summarise(across(all_of(question_codes_2), sd, na.rm = T))
)
row.names(weighting_impact_2) <- c("Unweighted government-wide average",
                                 "Weighted government-wide average",
                                 "Standard deviation") 
write.csv(weighting_impact_2, file = "weighting_impact_2.csv")


# WEIGHTED GOVERNMENT-WIDE REGRESSIONS ----

# ...Weighted and unweighted logistic models of response ----

# response_basic_logistic <- glm(as.formula(gsub("[\r\n]", "", paste0("RESPONSE ~ ", basic_model_vars))),
#                         data = analysis_dataset_2,
#                         family = binomial(link="logit"))
# 
# response_basic_logistic_wt <- glm(as.formula(gsub("[\r\n]", "", paste0("RESPONSE ~ ", basic_model_vars))),
#                                   data = analysis_dataset_2,
#                                   family = binomial(link="logit"),
#                                   weights = wt)

response_full_logistic_wt <- glm(as.formula(gsub("[\r\n]", "", paste0("RESPONSE ~ ", basic_model_vars, additional_model_vars, interaction_effects))),
                                 data = analysis_dataset_2,
                                 family = binomial(link="logit"),
                                 weights = wt)

sink(file = 'RESPONSE_all_models_2.txt')
stargazer(response_full_logistic_2, response_full_logistic_wt, type = "text",
          column.labels = c("Unweighted full model (used to construct weights)",
                            "Weighted full model"),
          title = "Response - completed survey or not, logistic")
sink()

# ...Weighted Engagement2_Q1: The Federal Government is the best place to work for those who want to make a difference.  ----
#The Federal Government is the best place to work for those who want to make a difference. 

engagement2_q1_full_wt <- run_full_model_wt(analysis_dataset_2, "ENGAGEMENT2_Q1",
                                            weight_vector = analysis_dataset_2$wt,
                                            question_text = "The Federal Government is the best place to work for those who want to make a difference.")

# ...Weighted Engagement2_Q2: My direct supervisor shows very little interest in the feelings of subordinates. ----
# My direct supervisor shows very little interest in the feelings of subordinates.

engagement2_q2_full_wt <- run_full_model_wt(analysis_dataset_2, "ENGAGEMENT2_Q2",
                                            weight_vector = analysis_dataset_2$wt,
                                            question_text = "My direct supervisor shows very little interest in the feelings of subordinates.")

# ...Weighted Engagement2_Q3: My workload is reasonable. ----
# My workload is reasonable.

engagement2_q3_full_wt <- run_full_model_wt(analysis_dataset_2, "ENGAGEMENT2_Q3",
                                            weight_vector = analysis_dataset_2$wt,
                                            question_text = "My workload is reasonable.")

# ...Weighted Engagement2_Q4: I feel exhausted in the morning at the thought of another workday. ----
# I feel exhausted in the morning at the thought of another workday.

engagement2_q4_full_wt <- run_full_model_wt(analysis_dataset_2, "ENGAGEMENT2_Q4",
                                            weight_vector = analysis_dataset_2$wt,
                                            question_text = "I feel exhausted in the morning at the thought of another workday.")

# ...Weighted Equity2_Q1: Agency leadership shows that diversity and inclusion are important through their actions. ----
# Agency leadership shows that diversity and inclusion are important through their actions. 

equity2_q1_full_wt <- run_full_model_wt(analysis_dataset_2, "EQUITY2_Q1",
                                        weight_vector = analysis_dataset_2$wt,
                                        question_text = "Agency leadership shows that diversity and inclusion are important through their actions.")

# ...Weighted Equity2_Q2: I have to hide parts of my identity to be successful at work. ----
# I have to hide parts of my identity to be successful at work.

equity2_q2_full_wt <- run_full_model_wt(analysis_dataset_2, "EQUITY2_Q2",
                                        weight_vector = analysis_dataset_2$wt,
                                        question_text = "I have to hide parts of my identity to be successful at work.")

# ...Weighted Equity2_Q3: People on my team listen to me, even when my views are dissimilar. ----
# People on my team listen to me, even when my views are dissimilar.

equity2_q3_full_wt <- run_full_model_wt(analysis_dataset_2, "EQUITY2_Q3",
                                        weight_vector = analysis_dataset_2$wt,
                                        question_text = "People on my team listen to me, even when my views are dissimilar.")

# ...Weighted Reentry2_Q1: I trust agency leadership to do what's right to protect employees' health, safety, and wellbeing. ----
# I trust agency leadership to do what's right to protect employees' health, safety, and wellbeing.

reentry2_q1_full_wt <- run_full_model_wt(analysis_dataset_2, "REENTRY2_Q1",
                                         weight_vector = analysis_dataset_2$wt,
                                         question_text = "I trust agency leadership to do what's right to protect employees' health, safety, and wellbeing.")

# ...Weighted Reentry2_Q2A: If I found a job elsewhere with more workplace flexibilities or remote options, I would take it. ----
# If I found a job elsewhere with more workplace flexibilities or remote options, I would take it.

reentry2_q2A_full_wt <- run_full_model_wt(analysis_dataset_2, "REENTRY2_Q2A",
                                         weight_vector = analysis_dataset_2$wt,
                                         question_text = "If I found a job elsewhere with more workplace flexibilities or remote options, I would take it.")

# ...Weighted Reentry2_Q2B: If I found a job elsewhere with more pay or better benefits, I would take it. ----
# If I found a job elsewhere with more pay or better benefits, I would take it.

reentry2_q2B_full_wt <- run_full_model_wt(analysis_dataset_2, "REENTRY2_Q2B",
                                          weight_vector = analysis_dataset_2$wt,
                                          question_text = "If I found a job elsewhere with more pay or better benefits, I would take it.")


# ...Weighted Reentry2_Q3: My agency's reentry arrangements are fair in accounting for employees' diverse needs and situations. ----
# My agency's reentry arrangements are fair in accounting for employees' diverse needs and situations.

reentry2_q3_full_wt <- run_full_model_wt(analysis_dataset_2, "REENTRY2_Q3",
                                         weight_vector = analysis_dataset_2$wt,
                                         question_text = "If I found a job elsewhere with more workplace flexibilities or remote options I would take it.")

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