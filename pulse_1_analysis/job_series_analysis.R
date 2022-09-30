library(dplyr)
library(tidyr)
library(ggplot2)
library(lemon)
library(tidytext)

swr = function(string, nwrap=50) {
  `Encoding<-`(string, 'latin1')
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

sum_not_na <- function(x){
  sum(!is.na(x))
}

q_list <- list(reentry1_q1 = "I trust agency leadership to do what's right to protect employees' health, safety, and wellbeing."
,reentry1_q2 = "I trust my supervisor will help me navigate the reentry transition."
,reentry1_q3 = "If I found a job elsewhere with more workplace flexibilities or remote options, I would take it."
,engagement1_q1 = "I have the support I need to do my job well."
,engagement1_q2 = "There is someone at work I can talk to about my day-to-day problems if I need to."
,engagement1_q3 = "I feel exhausted in the morning at the thought of another day at work."
,equity1_q1 = "Agency leadership shows that diversity and inclusion are important through their actions."
,equity1_q2 = "If I found a job elsewhere with the same pay and benefits as this one, I would take it."
,equity1_q3 = "My workload is reasonable."
,equity1_q4 = "I feel that maybe I don't belong in my agency."
)

acquisition_analysis <- analysis_dataset %>%
  mutate(acquisition = if_else(job_series_factor == 'Acquisition', "Acquisition", "All others"))

make_facet_chart <- function(question_code, question_text){
  df <- acquisition_analysis %>% select(agency = AGENCY, acquisition, wt)
  df$response <- acquisition_analysis[[question_code]]
  df <- df %>% group_by(agency, acquisition) %>%
    summarize(mean = mean(response, na.rm = T),
              wt_mean = weighted.mean(response, w = wt, na.rm = T),
              n = sum(!is.na(response))) %>%
    filter(n >= 10) %>%
    mutate(label = paste0(round(mean, 1), "\n (n = ", formatC(n, format = "d", big.mark = ","), ")"),
           agency_break = swr(agency, nwrap = 35))
  
  date <- if_else(grepl(pattern = "1_Q", x = question_code, ignore.case = T), 
                  true = "October 2021",
                  false = if_else(grepl(pattern = "2_Q", x = question_code, ignore.case = T),
                                  true = "January 2022",
                                  false = "March-April 2022"))
  
  p <- df %>%
    ggplot(aes(x = acquisition, y = mean, fill = acquisition)) +
    geom_bar(stat = "identity") + 
    geom_text(aes(label = label), vjust = 1.2, color = "white",
              show.legend = F) + 
    facet_wrap(~agency_break) +
    coord_cartesian(ylim = c(1, 5)) +
    theme(axis.title = element_blank(), 
          legend.title = element_blank(),
          legend.position = "none") +
    ggtitle(paste0(question_text, " \n", date)) +
    labs(caption = "1 is 'strongly disagree', 5 is 'strongly agree'. \nNo data shown if there are fewer than 10 responses.")
    return(p)
}

engagement1_q1_acq_p <- make_facet_chart("ENGAGEMENT1_Q1", "I have the support I need to do my job well.")
engagement1_q1_acq_p
engagement1_q2_acq_p <- make_facet_chart("ENGAGEMENT1_Q2", "There is someone at work I can talk to about my day-to-day problems if I need to.")
engagement1_q3_acq_p <- make_facet_chart("ENGAGEMENT1_Q3", "I feel exhausted in the morning at the thought of another day at work.")
equity1_q1_acq_p <- make_facet_chart("EQUITY1_Q1", "Agency leadership shows that diversity and inclusion are important through their actions.")
equity1_q2_acq_p <- make_facet_chart("EQUITY1_Q2", "If I found a job elsewhere with the same pay and benefits as this one, I would take it.")
equity1_q3_acq_p <- make_facet_chart("EQUITY1_Q3", "My workload is reasonable.")
equity1_q4_acq_p <- make_facet_chart("EQUITY1_Q4", "I feel that maybe I don't belong in my agency.")
reentry1_q1_acq_p <- make_facet_chart("REENTRY1_Q1", "I trust agency leadership to do whats right to protect employees' health, safety, and wellbeing.")
reentry1_q2_acq_p <- make_facet_chart("REENTRY1_Q2", "I trust my supervisor will help me navigate the reentry transition.")
reentry1_q3_acq_p <- make_facet_chart("REENTRY1_Q3", "If I found a job elsewhere with more workplace flexibilities or remote options, I would take it.")

gov_wide_acquisition_analysis <- acquisition_analysis %>%
  group_by(acquisition) %>%
  summarise(across(all_of(question_codes), weighted.mean, na.rm = T, w = wt)) %>%
  pivot_longer(cols = all_of(question_codes)) %>%
  mutate(name = tolower(name)) %>%
  mutate(name = recode(name, !!!q_list)) %>%
  mutate(name_breaks = swr(name))
gov_wide_acquisition_analysis_n <- acquisition_analysis %>%
  group_by(acquisition) %>%
  summarise(across(all_of(question_codes), sum_not_na)) %>%
  pivot_longer(cols = all_of(question_codes)) %>%
  mutate(name = tolower(name)) %>%
  mutate(name = recode(name, !!!q_list)) %>%
  mutate(name_breaks = swr(name)) %>%
  rename(n = value)
gov_wide_acquisition_analysis <- gov_wide_acquisition_analysis %>%
  left_join(gov_wide_acquisition_analysis_n) %>%
  mutate(label = paste0(round(value, 1), "\n (n = ", formatC(n, format = "d", big.mark = ","), ")"))

gov_wide_acq_p <- ggplot(gov_wide_acquisition_analysis, aes(acquisition, y = value, fill = acquisition)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Government-wide results \nOctober 2021") + 
  geom_text(aes(label = label), vjust = 1.5, color = "white", show.legend = F) +
  facet_wrap(~ name_breaks) +
  coord_cartesian(ylim = c(1, 5)) + 
  theme(axis.title = element_blank(), legend.title = element_blank()) +
  labs(caption = "1 is 'strongly disagree', 5 is 'strongly agree'.") +
  theme(legend.direction = "horizontal")
gov_wide_acq_p_rl <- reposition_legend(gov_wide_acq_p, 'bottom',
                  panel = c('panel-1-3', 'panel-4-3'))
gov_wide_acq_p_rl

plots = list(engagement1_q1_acq_p
,engagement1_q2_acq_p 
,engagement1_q3_acq_p 
,equity1_q1_acq_p
,equity1_q2_acq_p
,equity1_q3_acq_p 
,equity1_q4_acq_p 
,reentry1_q1_acq_p 
,reentry1_q2_acq_p 
,reentry1_q3_acq_p 
)
# 
# for(p in plots){
#   ggsave(filename = paste0(deparse(substitute(p)), ".jpg"), plot = p, device = "jpeg")
#   }

pdf("all_acquisition_visualizations.pdf", width = 11, height = 11)
invisible(lapply(reposition_legend(gov_wide_acq_p, 'bottom',
                                   panel = c('panel-1-3', 'panel-4-3')),
                 print))
invisible(lapply(plots, print))
dev.off()

### Create visualizations for HR/other pulse 1 responses ----
hr_job_series_df <- rbind(analysis_dataset %>%
  summarise(across(all_of(question_codes), ~weighted.mean(., w = wt, na.rm = T))) %>%
  pivot_longer(cols = all_of(question_codes)) %>%
    mutate(job_series_factor = 'All') %>%
    rename(question_code = name),

analysis_dataset %>%
  group_by(job_series_factor) %>%
  summarise(across(all_of(question_codes), 
                   ~weighted.mean(., w = wt, na.rm = TRUE))) %>%
  pivot_longer(cols = all_of(question_codes),
               names_to = "question_code") %>%
  filter(job_series_factor != 'Non-mission support job series')) %>%
  mutate(question_code = tolower(question_code)) %>%
  left_join(pulse_questions) %>%
  mutate(q_text_break = gsub('<92>', "'", swr(question_text, 40)),
         job_series_short = case_when(job_series_factor == 'Human capital' ~ 'HC',
                                      job_series_factor == 'Financial management' ~ 'FM',
                                      job_series_factor == 'Acquisition' ~ 'Acq',
                                      TRUE ~ as.character(job_series_factor)),
         comparison = if_else((job_series_factor == 'Human capital' & question_code %in% c('engagement1_q3',
                                                                                          'equity1_q3'))
                              | question_code == 'reentry1_q3',
                              'Less satisfied',
                              'More satisfied'))

Encoding(hr_job_series_df$question_text) <- 'latin1'
Encoding(hr_job_series_df$q_text_break) <- 'latin1'

ggplot(hr_job_series_df %>% filter(job_series_factor != "All"),
       aes(x = reorder_within(x = job_series_short, within = q_text_break, by = value), 
           y = value,
           fill = comparison)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap( ~ q_text_break, scales = "free_x") +
  geom_hline(data = hr_job_series_df %>% filter(job_series_factor == "All"), aes(yintercept = value)) +
  coord_cartesian(ylim = c(1, 5)) +
  ylab("Weighted average") + 
  theme(axis.ticks = element_blank()) + 
  labs(caption = "October 2021 pulse survey.\n 1 is 'strongly disagree', 5 is 'strongly agree'.",
       x = "Function",
       fill = "Comparison \n to overall \n average") +
  ggtitle("Human capital workforce burnout")
  
