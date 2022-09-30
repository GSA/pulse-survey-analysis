library(dplyr)
library(tidyr)
library(ggplot2)
set.seed(47)

# Define function for mode, used for imputation later
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

dt_pulse2_ehri = OracleSqlQuery("SELECT
    --row_id,
    pulse_token,
    agency_id,
    agency,
    agency_short,
    component,
    sub_component,
    sentemail,
    sent_survey_track,
    sent,
    engagement1_q1,
    engagement1_q2,
    engagement1_q3,
    equity1_q1,
    equity1_q2,
    equity1_q3,
    equity1_q4,
    reentry1_q1,
    reentry1_q2,
    reentry1_q3,
    --strata,
    --treat,
    --messenger,
    --personid,
    recipientemail,
    --agencylong,
    --agencyshort,
    --startdate,
    --enddate,
    --status,
    --finished,
    --survey_track,
    response,
    --sentence,
    ro_br_fl_6,
    reentry2_q1,
    reentry2_q2a,
    reentry2_q2b,
    reentry2_q3,
    equity2_q1,
    equity2_q2,
    equity2_q3,
    engagement2_q1,
    engagement2_q2,
    engagement2_q3,
    engagement2_q4,
    --pmc_name,
    --pmc_title,
    --url,
    --ipaddress,
    --locationlongitude,
    --locationlatitude,
    --locationaccuracy,
    --externaldata,
    ehri_token,
    --first_name,
    --middle_name,
    --last_name,
    work_email,
    --correspondence_city,
    --correspondence_state_cd,
    --correspondence_state_name,
    age_years,
    gender,
    race_short_name,
    race_long_name,
    ethnicity_indicator,
    --am_indian_ak_native_ind,
    --asian_ind,
    --black_african_am_ind,
    --hawaiian_pacific_islander_ind,
    --white_ind,
    --ethnicity_and_race_cd,
    --race_bridge_long_name,
    disability_short_name,
    --disability_long_name,
    veterans_status_long_name,
    us_citizenship_ind,
    --citizenship_country,
    --citizenship_country_long_name,
    education_level,
    education_level_short_name,
    department_cd,
    agency_cd,
    subelement_cd,
    department_long_name,
    agency_long_name,
    subelement_long_name,
    component_long_name,
    job_category_cd,
    job_group_cd,
    job_series,
    --job_category_name,
    --job_group_name,
    --job_long_name,
    supervisor_long_name,
    work_schedule_long_name,
    --tenure_long_name,
    tenure_short_name,
    length_of_service_years,
    length_of_service_months,
    tlwrk_elig_ind,
    pay_plan_cd,
    --pay_plan_long_name,
    grade_level,
    total_annual_pay,
    --rating_cd,
    --rating_long_name,
    --duty_station_city,
    --duty_station_county_long_name,
    --duty_station_country_long_name,
    duty_station_state_cd
    --duty_station_state_name,
    --locatlity_pay_long_name,
    --cbsa_long_name,
    --csa_long_name,
    --duty_station_territory_long_name,
    --msa_long_name,
    --cmsa_long_name,
    --leo_geo_long_name,
    --duty_station_area_long_name,
    --duty_station_town_long_name,
    --duty_station_region_long_name
FROM
    dmennealy.pulse2_ehri
WHERE ehri_token IS NOT NULL;")

analysis_dataset_2 <- dt_pulse2_ehri

rm(dt_pulse2_ehri)

# Recode "RESPONSE" as 1 or 0
# Right now no response is NA

analysis_dataset_2$RESPONSE[is.na(analysis_dataset_2$RESPONSE)] <- 0

# Resolve duplicates ----

# Remove duplicates
# Keep based on 1) response, 
# 2) longer length of service,
# 3) higher total annual pay
# If these three variables are all equal,
# The first row will be kept.

nrow(analysis_dataset_2)
analysis_dataset_2 <- analysis_dataset_2 %>%
  group_by(EHRI_TOKEN) %>%
  arrange(desc(RESPONSE), desc(LENGTH_OF_SERVICE_MONTHS), desc(TOTAL_ANNUAL_PAY)) %>%
  slice_head() %>%
  ungroup()
nrow(analysis_dataset_2)

# Explore and transform variables ----

# Transform variables into factors
to_factors <- c('AGENCY', 'COMPONENT', 'SUB_COMPONENT',
                'SENT_SURVEY_TRACK',
                'GENDER', 'RACE_SHORT_NAME',
                'RACE_LONG_NAME', 'RACE_BRIDGE_LONG_NAME',
                'DISABILITY_SHORT_NAME', 'VETERANS_STATUS_LONG_NAME',
                'EDUCATION_LEVEL_SHORT_NAME',
                'JOB_CATEGORY_CD', 'JOB_GROUP_CD', 'JOB_SERIES',
                'SUPERVISOR_LONG_NAME', 'WORK_SCHEDULE_LONG_NAME',
                'TENURE_SHORT_NAME', 'TLWRK_ELIG_IND',
                'PAY_PLAN_CD', 'GRADE_LEVEL')

analysis_dataset_2 <- mutate_at(analysis_dataset_2,
                              .vars = to_factors,
                              as.factor)

### ...create education factor ----
analysis_dataset_2 <- pulse::create_education_factor(analysis_dataset_2)

table(analysis_dataset_2$education_factor, useNA = "ifany")

analysis_dataset_2 <- pulse::impute_education_factor(x = analysis_dataset_2)

### ...transform race variables ------
analysis_dataset_2 <- pulse::create_race_factor(analysis_dataset_2)

table(analysis_dataset_2$race_factor, useNA = "ifany")

stopifnot(sum(is.na(analysis_dataset_2$race_factor)) == 0)

### ...transform gender variable ------
table(analysis_dataset_2$GENDER, useNA = "ifany")

analysis_dataset_2 <- pulse::impute_gender(analysis_dataset_2)

analysis_dataset_2$GENDER <- droplevels(analysis_dataset_2$GENDER)

table(analysis_dataset_2$GENDER, useNA = "ifany")

### ...create ethnicity factor
analysis_dataset_2 <- pulse::create_ethnicity_factor(analysis_dataset_2)

table(analysis_dataset_2$ethnicity_factor, useNA = "ifany")

stopifnot(sum(is.na(analysis_dataset_2$ethnicity_factor)) == 0)

### ...create age buckets -------
analysis_dataset_2 <- pulse::create_age_factor(analysis_dataset_2)

table(analysis_dataset_2$age_factor, useNA = "ifany")

# There are three NAs for age factor. Impute by mode of agency.
analysis_dataset_2 <- pulse::impute_age_factor(analysis_dataset_2)

table(analysis_dataset_2$age_factor, useNA = "ifany")
stopifnot(sum(is.na(analysis_dataset_2$age_factor)) == 0)

### ...create length of service buckets ----
analysis_dataset_2 <- pulse::create_length_of_service_factor(analysis_dataset_2)

table(analysis_dataset_2$length_of_service_factor, useNA = "ifany")

# There are two NAs for length of service factor. Impute by mode of agency.
analysis_dataset_2 <- pulse::impute_length_of_service_factor(analysis_dataset_2)

table(analysis_dataset_2$length_of_service_factor, useNA = "ifany")

stopifnot(sum(is.na(analysis_dataset_2$length_of_service_factor)) == 0)

### ...create veteran status factor variable ----
analysis_dataset_2 <- pulse::create_veteran_status_factor(analysis_dataset_2)

table(analysis_dataset_2$veteran_status_factor, useNA = "ifany")

# A handful of rows may show up with veteran status factor NA. Drop that row.
if(sum(is.na(analysis_dataset_2$veteran_status_factor)) < 5){
  analysis_dataset_2 <- filter(analysis_dataset_2, !is.na(veteran_status_factor))
}

stopifnot(sum(is.na(analysis_dataset_2$veteran_status_factor)) == 0)

### ...create disability factor variable ----
analysis_dataset_2 <- pulse::create_disability_factor(analysis_dataset_2)

table(analysis_dataset_2$disability_factor, useNA = "ifany")

stopifnot(sum(is.na(analysis_dataset_2$disability_factor)) == 0)

### ...create supervisor status factor variable -----
analysis_dataset_2 <- pulse::create_supervisor_status_factor(analysis_dataset_2)

table(analysis_dataset_2$supervisor_status_factor, useNA = "ifany")

# There are 60 NAs for supervisory status
# Impute by mode of component and grade code

analysis_dataset_2 <- pulse::impute_supervisor_status_factor(analysis_dataset_2)

table(analysis_dataset_2$supervisor_status_factor, useNA = "ifany")
stopifnot(sum(is.na(analysis_dataset_2$supervisor_status_factor)) == 0)

### ...create work schedule factor variable ----
analysis_dataset_2 <- pulse::create_work_schedule_factor(analysis_dataset_2)

table(analysis_dataset_2$work_schedule_factor, useNA = "ifany")

stopifnot(sum(is.na(analysis_dataset_2$work_schedule_factor)) == 0)

### ...create job series factor variable ----
analysis_dataset_2 <- pulse::create_job_series_factor(analysis_dataset_2)

table(analysis_dataset_2$job_series_factor, useNA = "ifany")

### ...create location factor variable ----
analysis_dataset_2 <- pulse::create_location_factor(analysis_dataset_2)

table(analysis_dataset_2$location_factor, useNA = "ifany")

### ...examine whether TOTAL_ANNUAL_PAY is continuous ----
max(analysis_dataset_2$TOTAL_ANNUAL_PAY, na.rm = T)

ggplot(analysis_dataset_2, aes(x = TOTAL_ANNUAL_PAY)) + 
  geom_histogram()
br = seq(0, 500000, by = 10000)
ranges = paste(head(br,-1), br[-1], sep = " - ")
freq = hist(analysis_dataset_2$TOTAL_ANNUAL_PAY, breaks = br,
            include.lowest = TRUE)
sink(file = 'salary_freq_counts.txt')
data.frame(range = ranges, frequency = freq$counts)
sink()

### ...impute missing TOTAL_ANNUAL_PAY ---- 
analysis_dataset_2 <- pulse::impute_total_annual_pay(analysis_dataset_2)

sum(is.na(analysis_dataset_2$total_annual_pay_imputed))

# ...impute telework eligibility factor by component and grade code ----
analysis_dataset_2 <- pulse::impute_telework_eligibility_factor(analysis_dataset_2)

table(analysis_dataset_2$telework_eligibility_factor, useNA = "ifany")

# Write summary stats to text files ----

sum_na <- function(x){sum(is.na(x))}

sink(file = 'summary-stats_2.txt')
lapply(analysis_dataset_2, sum_na)

for (n in 1:length(colnames(analysis_dataset_2))){
  cn <- colnames(analysis_dataset_2[, n])
  if (cn %in% c("EMAIL", "RESPONSE", "UNIQUE_PERSON_PK",
                "WORK_EMAIL")){
    next
  }
  if (startsWith(cn, "ENGAGEMENT")){
    next
  }
  if (startsWith(cn, "EQUITY")){
    next
  }
  if (startsWith(cn, "REENTRY")){
    next
  }
  
  if (startsWith(cn, "TOTAL_ANNUAL_PAY")){
    print("Summary of 'TOTAL_ANNUAL_PAY' column")
    print(summary(analysis_dataset_2[, n]))
    next
  }
  
  if (startsWith(cn, "total_annual_imputed_pay")){
    print("Summary of 'total_annual_imputed_pay' column")
    print(summary(analysis_dataset_2[, n]))
    next
  }
  
  print(cn)
  print(str(analysis_dataset_2[, n]))
  print(sort(table(analysis_dataset_2[, n], useNA = "ifany"), decreasing = T))
}

sink()


# # Remove columns that are not needed ----
# 
# # We'll keep columns that were used to impute derived variables,
# # so that we can check back and see which records were imputed.
# # We'll also keep job series, job category, and job group in case
# # we want to slice the data by job duties later on.

analysis_dataset_2 <- analysis_dataset_2 %>%
  select(-c("SENT", "SENTEMAIL", "WORK_EMAIL", "RECIPIENTEMAIL", "RACE_SHORT_NAME",
            "RACE_LONG_NAME", "ETHNICITY_INDICATOR",
            "DISABILITY_SHORT_NAME",
            "VETERANS_STATUS_LONG_NAME","EDUCATION_LEVEL_SHORT_NAME",
            "DEPARTMENT_LONG_NAME", "AGENCY_LONG_NAME",
            "TENURE_SHORT_NAME"))

sink("final_colnames_2.txt")
colnames(analysis_dataset_2)
sink()


# oracleDsn <- Sys.getenv('OracleDsnPrd')
# oracleUser <- Sys.getenv("oracleuser")
# oraclePwd <- Sys.getenv("oraclepwd")
# con <- odbcConnect(oracleDsn, uid=oracleUser, pwd=oraclePwd, believeNRows = FALSE) 
# sqlSave(con, analysis_dataset_2, tablename = "analysis_dataset_2")
# rm(oraclePwd)
# 
# tst_q <- OracleSqlQuery('SELECT * FROM nsmiller."analysis_dataset_2" WHERE rownum <10')
