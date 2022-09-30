library(randomGLM)

colnames(engagement1_q1_no_na)
set.seed(989)

engagement1_q1_no_na_m <- engagement1_q1_no_na %>% 
  dplyr::select(ENGAGEMENT1_Q1,
                                 total_annual_pay_imputed_5000,
                                 age_factor,
                                 race_factor,
                                 ethnicity_factor,
                                 GENDER,
                                 disability_factor,
                                 education_factor,
                                 length_of_service_factor,
                                 veteran_status_factor,
                                 AGENCY,
                                 telework_eligibility_factor,
                                 supervisor_status_factor,
                                 job_series_factor,
                                 location_factor,
                                 US_CITIZENSHIP_IND)

split1 <- sample(seq_len(nrow(engagement1_q1_no_na_m)), size = floor(0.7 * nrow(engagement1_q1_no_na_m)))

train <- engagement1_q1_no_na_m[split1, ]
test <-  engagement1_q1_no_na_m[-split1, ]

RGLM <- randomGLM(x = dplyr::select(train, -ENGAGEMENT1_Q1), 
                  y = train$ENGAGEMENT1_Q1,
                  classify = FALSE,
                  keepModels = TRUE,
                  nThreads = 1)
