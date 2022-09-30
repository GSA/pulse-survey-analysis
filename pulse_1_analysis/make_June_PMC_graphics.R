library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)

swr = function(string, nwrap=50) {
  `Encoding<-`(string, 'latin1')
  paste(strwrap(string, width=nwrap), collapse="\n")
  
}
swr = Vectorize(swr)

pulse_questions <- read_csv("pulse_questions.csv") %>%
  mutate(question_code = toupper(question_code))

# Validate age discrepencies by other variables ----
weighted_mean_by_age_and_length <- analysis_dataset %>%
  group_by(age_factor, length_of_service_factor) %>%
  summarise(across(all_of(question_codes), 
                   .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                               n.responses = ~round(sum(!is.na(.)), digits = 0)
                               ))) %>%
  pivot_longer(-c(age_factor, length_of_service_factor),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(q_code = paste0(track, "_", q),
         age_factor = factor(age_factor, levels = c("25 and under",
                                                    "26-29 years old",
                                                    "30-39 years old",
                                                    "40-49 years old",
                                                    "50-59 years old",
                                                    "60 or older")),
         length_of_service_factor = factor(length_of_service_factor,
                                           levels = c("Less than 1 year",
                                                      "1 to 3 years",
                                                      "4 to 5 years",
                                                      "6 to 10 years",
                                                      "11 to 14 years",
                                                      "15 to 20 years",
                                                      "More than 20 years"))) %>%
  filter(n.responses >= 50)

weighted_mean_by_age_and_length %>%
  filter(q_code == "REENTRY1_Q3") %>%
  ungroup() %>%
  ggplot(., aes(x = age_factor,
                y = weighted.mean,
                color = length_of_service_factor,
                group = length_of_service_factor)) +
  geom_line() +
  geom_point() +
  xlab("Age") + ylab("Weighted average response")  +
  labs(color = "Length of service")

weighted_mean_by_age_and_race <- analysis_dataset %>%
  group_by(age_factor, race_factor) %>%
  summarise(across(all_of(question_codes), 
                   .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                               n.responses = ~round(sum(!is.na(.)), digits = 0)
                   ))) %>%
  pivot_longer(-c(age_factor, race_factor),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(q_code = paste0(track, "_", q),
         age_factor = factor(age_factor, levels = c("25 and under",
                                                    "26-29 years old",
                                                    "30-39 years old",
                                                    "40-49 years old",
                                                    "50-59 years old",
                                                    "60 or older"))) %>%
  filter(n.responses >= 50)

weighted_mean_by_age_and_race %>%
  filter(q_code == "REENTRY1_Q2") %>%
  ungroup() %>%
  ggplot(., aes(x = age_factor,
                y = weighted.mean,
                color = race_factor,
                group = race_factor)) +
  geom_line() +
  geom_point() +
  xlab("Age") + ylab("Weighted average response")  +
  labs(color = "Race")


weighted_mean_by_age_and_gender <- analysis_dataset %>%
  group_by(age_factor, GENDER) %>%
  summarise(across(all_of(question_codes), 
                   .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                               n.responses = ~round(sum(!is.na(.)), digits = 0)
                   ))) %>%
  pivot_longer(-c(age_factor, GENDER),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(q_code = paste0(track, "_", q),
         age_factor = factor(age_factor, levels = c("25 and under",
                                                    "26-29 years old",
                                                    "30-39 years old",
                                                    "40-49 years old",
                                                    "50-59 years old",
                                                    "60 or older"))) %>%
  filter(n.responses >= 50)

weighted_mean_by_age_and_gender %>%
  filter(q_code == "ENGAGEMENT1_Q3") %>%
  ungroup() %>%
  ggplot(., aes(x = age_factor,
                y = weighted.mean,
                color = GENDER,
                group = GENDER)) +
  geom_line() +
  geom_point() +
  xlab("Age") + ylab("Weighted average response")  +
  labs(color = "Gender")


weighted_mean_by_age_and_ethnicity <- analysis_dataset %>%
  group_by(age_factor, ethnicity_factor) %>%
  summarise(across(all_of(question_codes), 
                   .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                               n.responses = ~round(sum(!is.na(.)), digits = 0)
                   ))) %>%
  pivot_longer(-c(age_factor, ethnicity_factor),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(q_code = paste0(track, "_", q),
         age_factor = factor(age_factor, levels = c("25 and under",
                                                    "26-29 years old",
                                                    "30-39 years old",
                                                    "40-49 years old",
                                                    "50-59 years old",
                                                    "60 or older"))) %>%
  filter(n.responses >= 50)

weighted_mean_by_age_and_ethnicity %>%
  filter(q_code == "EQUITY1_Q2") %>%
  ungroup() %>%
  ggplot(., aes(x = age_factor,
                y = weighted.mean,
                color = ethnicity_factor,
                group = ethnicity_factor)) +
  geom_line() +
  geom_point() +
  xlab("Age") + ylab("Weighted average response")  +
  labs(color = "Ethnicity")

weighted_mean_by_age_and_agency <- analysis_dataset %>%
  group_by(age_factor, AGENCY) %>%
  summarise(across(all_of(question_codes), 
                   .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                               n.responses = ~round(sum(!is.na(.)), digits = 0)
                   ))) %>%
  pivot_longer(-c(age_factor, AGENCY),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(q_code = paste0(track, "_", q),
         age_factor = factor(age_factor, levels = c("25 and under",
                                                    "26-29 years old",
                                                    "30-39 years old",
                                                    "40-49 years old",
                                                    "50-59 years old",
                                                    "60 or older"))) %>%
  filter(n.responses >= 50)

weighted_mean_by_age_and_agency %>%
  filter(q_code == "EQUITY1_Q4") %>%
  ungroup() %>%
  ggplot(., aes(x = age_factor,
                y = weighted.mean,
                color = AGENCY,
                group = AGENCY)) +
  geom_line() +
  geom_point() +
  xlab("Age") + ylab("Weighted average response")  +
  labs(color = "Agency")


most_common_grades <- analysis_dataset %>%
  group_by(GRADE_SCALE_NUM) %>%
  summarise(num = n()) %>%
  slice_max(order_by = num, n = 10)

weighted_mean_by_age_and_grade <- analysis_dataset %>%
  group_by(age_factor, GRADE_SCALE_NUM) %>%
  summarise(across(all_of(question_codes), 
                   .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                               n.responses = ~round(sum(!is.na(.)), digits = 0)
                   ))) %>%
  pivot_longer(-c(age_factor, GRADE_SCALE_NUM),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(q_code = paste0(track, "_", q),
         age_factor = factor(age_factor, levels = c("25 and under",
                                                    "26-29 years old",
                                                    "30-39 years old",
                                                    "40-49 years old",
                                                    "50-59 years old",
                                                    "60 or older"))) %>%
  filter(n.responses >= 50)

weighted_mean_by_age_and_grade %>%
  filter(q_code == "REENTRY1_Q1",
         GRADE_SCALE_NUM %in% most_common_grades$GRADE_SCALE_NUM) %>%
  ungroup() %>%
  ggplot(., aes(x = age_factor,
                y = weighted.mean,
                color = GRADE_SCALE_NUM,
                group = GRADE_SCALE_NUM)) +
  geom_line() +
  geom_point() +
  xlab("Age") + ylab("Weighted average response")  +
  labs(color = "GRADE_SCALE_NUM")

# Make age graphics ----

agree_disagree_by_age <- analysis_dataset %>%
  mutate(age_factor_combined = if_else(age_factor %in% c("26-29 years old", "30-39 years old"),
                                        "26-39 years old",
                                        if_else(age_factor %in% c("50-59 years old", "60 or older"),
                                                "50 and older", "Other"))) %>%
  group_by(age_factor_combined) %>%
  summarise(across(all_of(question_codes), 
                   .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                               n.responses = ~round(sum(!is.na(.)), digits = 0),
                               weighted.n = ~sum(if_else(!is.na(.), wt, 0)),
                              weighted.sum.agree = ~sum(if_else(. %in% c(4, 5), wt, 0)),
                              weighted.sum.disagree = ~sum(if_else(. %in% c(1, 2), wt, 0))
                   ))) %>%
  pivot_longer(-c(age_factor_combined),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(q_code = paste0(track, "_", q),
         weighted.pct.agree = weighted.sum.agree / weighted.n,
         weighted.pct.disagree = weighted.sum.disagree / weighted.n) %>%
  filter(n.responses >= 50) %>%
  left_join(., pulse_questions, by = c("q_code" = "question_code"))

agree_disagree_by_age_2 <- analysis_dataset_2 %>%
  mutate(age_factor_combined = if_else(age_factor %in% c("26-29 years old", "30-39 years old"),
                                       "26-39 years old",
                                       if_else(age_factor %in% c("50-59 years old", "60 or older"),
                                               "50 and older", "Other"))) %>%
  group_by(age_factor_combined) %>%
  summarise(across(all_of(question_codes_2), 
                   .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                               n.responses = ~round(sum(!is.na(.)), digits = 0),
                               weighted.n = ~sum(if_else(!is.na(.), wt, 0)),
                               weighted.sum.agree = ~sum(if_else(. %in% c(4, 5), wt, 0)),
                               weighted.sum.disagree = ~sum(if_else(. %in% c(1, 2), wt, 0))
                   ))) %>%
  pivot_longer(-c(age_factor_combined),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(q_code = paste0(track, "_", q),
         weighted.pct.agree = weighted.sum.agree / weighted.n,
         weighted.pct.disagree = weighted.sum.disagree / weighted.n) %>%
  filter(n.responses >= 50) %>%
  left_join(., pulse_questions, by = c("q_code" = "question_code"))

# filter(analysis_dataset_3, 
#        !is.na(ENGAGEMENT3_Q1), !is.na(ENGAGEMENT3_Q3), AGENCY_SHORT == "GSA") %>% 
#   mutate(age_factor_combined = if_else(age_factor %in% c("26-29 years old", "30-39 years old"),
#                                        "26-39 years old",
#                                        if_else(age_factor %in% c("50-59 years old", "60 or older"),
#                                                "50 and older", "Other"))) %>%
#   filter(age_factor_combined != "Other") %>%
#   ggplot(., aes(ENGAGEMENT3_Q1, ENGAGEMENT3_Q2)) + 
#   geom_point() + geom_jitter(aes(color = age_factor_combined))
  
agree_disagree_by_age_3 <- analysis_dataset_3 %>%
  mutate(age_factor_combined = if_else(age_factor %in% c("26-29 years old", "30-39 years old"),
                                       "26-39 years old",
                                       if_else(age_factor %in% c("50-59 years old", "60 or older"),
                                               "50 and older", "Other"))) %>%
  group_by(age_factor_combined) %>%
  summarise(across(all_of(question_codes_3), 
                   .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                               n.responses = ~round(sum(!is.na(.)), digits = 0),
                               weighted.n = ~sum(if_else(!is.na(.), wt, 0)),
                               weighted.sum.agree = ~sum(if_else(. %in% c(4, 5), wt, 0)),
                               weighted.sum.disagree = ~sum(if_else(. %in% c(1, 2), wt, 0))
                   ))) %>%
  pivot_longer(-c(age_factor_combined),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(q_code = paste0(track, "_", q),
         weighted.pct.agree = weighted.sum.agree / weighted.n,
         weighted.pct.disagree = weighted.sum.disagree / weighted.n) %>%
  filter(n.responses >= 50) %>%
  left_join(., pulse_questions, by = c("q_code" = "question_code"))



# agree_disagree_by_age_2 <- analysis_dataset_2 %>%
#   group_by(age_factor) %>%
#   summarise(across(all_of(question_codes), 
#                    .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
#                                n.responses = ~round(sum(!is.na(.)), digits = 0),
#                                weighted.n = ~sum(if_else(!is.na(.), wt, 0)),
#                                weighted.sum.agree = ~sum(if_else(. %in% c(4, 5), wt, 0)),
#                                weighted.sum.disagree = ~sum(if_else(. %in% c(1, 2), wt, 0))
#                    ))) %>%
#   pivot_longer(-c(age_factor),
#                names_to = c("track", "q", "metric"),
#                names_sep = "_") %>%
#   pivot_wider(names_from = metric, values_from = value) %>%
#   mutate(q_code = paste0(track, "_", q),
#          age_factor = factor(age_factor, levels = c("25 and under",
#                                                     "26-29 years old",
#                                                     "30-39 years old",
#                                                     "40-49 years old",
#                                                     "50-59 years old",
#                                                     "60 or older")),
#          weighted.pct.agree = weighted.sum.agree / weighted.n,
#          weighted.pct.disagree = weighted.sum.disagree / weighted.n) %>%
#   filter(n.responses >= 50) %>%
#   left_join(., pulse_questions, by = c("q_code" = "question_code"))


agree_disagree_by_age %>%
  filter(! age_factor_combined %in% c("Other")) %>%
  filter(q_code %in% c("EQUITY1_Q1", "ENGAGEMENT1_Q1", "REENTRY1_Q1",  "REENTRY1_Q2")) %>%
  mutate(date_text = if_else(pulse_num == 1, "October 2021", if_else(pulse_num == 2,
                                                                     "January - February 2022", 
                                                                     "March - April 2022"))) %>%
  mutate(question_label = gsub('<92>', "'", swr(paste0(question_text, " (", date_text, ")")))) %>%
  ggplot(aes(x = question_label, y = weighted.pct.disagree, color = age_factor)) +
  geom_point(size = 2, position = position_dodge(width = 0.0)) +
  labs(y = "Weighted percent disagreement", x = NULL, color = "Age",
       title = "Younger employees were more likely \n to disagree with positive statements...",
       caption = "Percentage of employees who selected 'disagree' or 
  'strongly disagree', weighted to account for non-response bias.") +
  theme(aspect.ratio = 0.6) +
  coord_flip()

agree_disagree_by_age %>%
  filter(! age_factor_combined %in% c("Other")) %>%
  filter(q_code %in% c("ENGAGEMENT1_Q3", "EQUITY1_Q4", "REENTRY1_Q3")) %>%
  mutate(date_text = if_else(pulse_num == 1, "October 2021", if_else(pulse_num == 2,
                                                                "January - February 2022", 
                                                                "March - April 2022"))) %>%
  mutate(question_label = gsub('<92>', "'", swr(paste0(question_text), 30))) %>%
  ggplot(aes(x = question_label, y = weighted.pct.agree, color = age_factor_combined)) +
  geom_point(size = 2, position = position_dodge(width = 0.0)) +
  labs(y = "Percent agreement", x = NULL, color = "Age",
       title = "Younger employees express lower satisfaction",
       caption = "October 2021. Weighted to account for non-response bias.") +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  theme(aspect.ratio = 0.6) +
  coord_flip()


agree_disagree_by_age_3 %>%
  filter(! age_factor_combined %in% c("Other")) %>%
  filter(q_code %in% c("ENGAGEMENT3_Q1", "ENGAGEMENT3_Q3",
                       "EQUITY3_Q2")) %>%
  mutate(date_text = if_else(pulse_num == 1, "October 2021", if_else(pulse_num == 2,
                                                                     "January - February 2022", 
                                                                     "March - April 2022"))) %>%
  mutate(question_label = gsub('<92>', "'", swr(paste0(question_text), 30))) %>%
  ggplot(aes(x = q_code, y = weighted.pct.agree, color = age_factor_combined)) +
  geom_point(size = 2, position = position_dodge(width = 0.0)) +
  labs(y = "Percent agreement", x = NULL, color = "Age",
       title = "Younger employees express lower satisfaction",
       caption = "March-April 2022. Weighted to account for non-response bias.") +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  theme(aspect.ratio = 0.6) +
  coord_flip()

# Validate racial discrepancies by other variables ----
weighted_mean_by_race_and_gender <- analysis_dataset %>%
  group_by(race_factor, GENDER) %>%
  summarise(across(all_of(question_codes), 
                   .fns = list(weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                               n.responses = ~round(sum(!is.na(.)), digits = 0)
                   ))) %>%
  pivot_longer(-c(race_factor, GENDER),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(q_code = paste0(track, "_", q)) %>%
  filter(n.responses >= 50)

weighted_mean_by_race_and_gender %>%
  ungroup() %>%
  ggplot(., aes(x = GENDER,
                y = weighted.mean,
                color = race_factor,
                group = race_factor)) +
  geom_line() +
  geom_point() +
  xlab("Gender") + ylab("Weighted average response")  +
  labs(color = "Race") +
  facet_wrap(~ q_code)
