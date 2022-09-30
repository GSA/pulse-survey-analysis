library(dplyr)

quits <- OracleSqlQuery("SELECT u.agency, u.agency_subelement, 
dynamics_category, 
organizational_component, 
personnel_act_eff_dt, 
personnel_act_eff_dt_yyyymm,
sdm_dynamics_process_dt,
t.*,
p1.RESPONSE
,p1.ENGAGEMENT1_Q1
,p1.ENGAGEMENT1_Q2
,p1.ENGAGEMENT1_Q3
,p1.EQUITY1_Q1
,p1.EQUITY1_Q2
,p1.EQUITY1_Q3
,p1.EQUITY1_Q4
,p1.REENTRY1_Q1
,p1.REENTRY1_Q2
,p1.REENTRY1_Q3
FROM SDMDB.UMV_DYNAMICS u
LEFT JOIN DMENNEALY.TOKEN_IDS t
ON u.unique_person_fk = t.unique_person_fk
LEFT JOIN DMENNEALY.PULSE_EHRI p1
ON u.unique_person_fk = p1.UNIQUE_PERSON_PK
WHERE DYNAMICS_CATEGORY = '3B'
AND personnel_act_eff_dt_yyyymm IN ('202111', '202112', '202201')
AND u.AGENCY IN ('VA', 'AR', 'NV', 'HS', 'AF',
'DJ', 'DD', 'TR', 'AG', 'HE', 'IN', 'TD', 'CM',
'DN', 'DL', 'ST', 'HU', 'ED', 'SZ', 'NN', 'EP', 'GS', 
'SB', 'NU', 'AM', 'OM', 'NF')")

# table(november_21_quits$ENGAGEMENT1_Q3)
# I feel exhausted in the morning...
# 
# 1   2   3   4   5 
# 24  14  32  40 109 
# 52% strongly agree, compared to 17.3% gov-wide
# 19% somewhat agree, compared to 25% gov-wide
# 71% of respondents who quit in November agree vs. 42% overall
# Only 18% of resignations disagree, vs. 38% gov-wide

quits %>%
  filter(!is.na(ENGAGEMENT1_Q3)) %>%
  group_by(AGENCY, AGENCY_SUBELEMENT) %>% 
  summarise(n = n(), 
            strong_agree = sum(ENGAGEMENT1_Q3 == 5),
            agree = sum(ENGAGEMENT1_Q3 == 4)) %>% arrange(desc(n)) %>%
  mutate(pct_strong_agree = strong_agree / n,
         pct_agree = (agree + strong_agree) / n)

november_21_quits_summary <- november_21_quits %>% summarise(across(all_of(question_codes), 
                 .fns = list(strong.disagree.pct = ~sum(. == 1, na.rm = T)/sum(!is.na(.)),
                             somewhat.disagree.pct = ~sum(. == 2, na.rm = T)/sum(!is.na(.)),
                             neutral.pct = ~sum(. == 3, na.rm = T)/sum(!is.na(.)),
                             somewhat.agree.pct = ~sum(. == 4, na.rm = T)/sum(!is.na(.)),
                             strongly.agree.pct = ~sum(. == 5, na.rm = T)/sum(!is.na(.)),
                             n.responses = ~sum(!is.na(.))
                 ))) %>%
  pivot_longer(everything(),
               names_to = c("track", "q", "metric"),
               names_sep = "_") %>%
  mutate(question_code = paste0(track, "_", q)) %>%
  select(-c(track, q))

november_21_quits_summary <- november_21_quits_summary %>%
  filter(value > 1) %>%
  rename(n.responses = value) %>%
  select(-metric) %>%
  right_join(november_21_quits_summary) %>%
  filter(value <= 1) %>%
  relocate(n.responses, .after = last_col()) %>%
  left_join(stack(q_map), by = c("question_code" = "values")) %>%
  rename(question_text = ind)



# 52% of Veterans Health Admin who quit strongly agreed, compared to 22% of all VHA (n = 54)
# 78% of Veterans Health Admin who quit agreed, compared to 22% of all VHA (n = 54)

# 70% (60%) of SBA respondents who quit agreed (strongly agreed), compared to 29% (11%) at SBA as a whole

# EQUITY1_Q2 "If I found a job elsewhere with the same pay and benefits as this one, I would take it."
november_21_quits %>%
  filter(!is.na(EQUITY1_Q2)) %>%
  group_by(AGENCY, AGENCY_SUBELEMENT) %>% 
  summarise(n = n(), 
            strong_agree = sum(EQUITY1_Q2 == 5),
            agree = sum(EQUITY1_Q2 == 4)) %>% arrange(desc(n)) %>%
  mutate(pct_strong_agree = strong_agree / n,
         pct_agree = (agree + strong_agree) / n)

# Among respondents at VHA who quit in November 2021, 73% strongly agreed, 86% agreed. Compared to 
# 29% strongly agreed across all VHA responses, 39% agreed (n = 71)
# AG11 == forest service, 82% agree/64% strongly agree among people who quit, vs. 48% agree/27% strongly agree 


# REENTRY1_Q3 "If I found a job elsewhere with more workplace flexibilities or remote options I would take it."
november_21_quits %>%
  filter(!is.na(REENTRY1_Q3)) %>%
  group_by(AGENCY, AGENCY_SUBELEMENT) %>% 
  summarise(n = n(), 
            strong_agree = sum(REENTRY1_Q3 == 5),
            agree = sum(REENTRY1_Q3 == 4)) %>% arrange(desc(n)) %>%
  mutate(pct_strong_agree = strong_agree / n,
         pct_agree = (agree + strong_agree) / n)

# Among respondents at VHA who quit in November 2021, 73% strongly agreed, 86% agreed. Compared to 
# 29% strongly agreed across all VHA responses, 39% agreed (n = 71)
# AG11 == forest service, 82% agree/64% strongly agree among people who quit, vs. 48% agree/27% strongly agree 