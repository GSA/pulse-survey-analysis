
equity1_q1_no_na <- add_column(analysis_dataset, wt_v) %>%
  filter(!is.na(EQUITY1_Q1))

equity1_q1_full_model <- lm(formula = paste0("EQUITY1_Q1 ~ ", gsub("[\r\n]", "", paste0(basic_model_vars, additional_model_vars, interaction_effects))),
                 data = equity1_q1_no_na)

head(coeftest(equity1_q1_full_model, vcov. = vcovHC(equity1_q1_full_model, type = 'HC1')))

head(coeftest(equity1_q1_full_wt, vcov. = vcovHC(equity1_q1_full_wt, type = 'HC1')))

stargazer(equity1_q1_full_model, equity1_q1_full_wt, type = "text")

####
engagement1_q1_no_na <- add_column(analysis_dataset, wt_v) %>%
  filter(!is.na(ENGAGEMENT1_Q1))

engagement1_q1_full_model <- lm(formula = paste0("ENGAGEMENT1_Q1 ~ ", gsub("[\r\n]", "", paste0(basic_model_vars, additional_model_vars, interaction_effects))),
                            data = engagement1_q1_no_na)

head(coeftest(engagement1_q1_full_model, vcov. = vcovHC(engagement1_q1_full_model, type = 'HC1')))

head(coeftest(engagement1_q1_full_wt, vcov. = vcovHC(engagement1_q1_full_wt, type = 'HC1')))

stargazer(engagement1_q1_full_model, engagement1_q1_full_wt, type = "text", ci = T)

