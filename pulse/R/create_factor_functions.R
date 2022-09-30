#' Function to Transform Education Factor
#'
#' This function transform the raw EDUCATION_LEVEL variable into education_factor
#' @param x Name of analysis dataset
#' @keywords education_factor
#' @export
#' @examples
#' create_education_factor()

create_education_factor <- function(x)
  {x <- mutate(x,
           education_factor = factor(case_when(EDUCATION_LEVEL %in% c("01", "02", "03") ~ 
                                 "Less than High School",
                               EDUCATION_LEVEL %in% c("04", "05") ~
                                 "High School Diploma_GED or equivalent",
                               EDUCATION_LEVEL %in% c("06") ~ 
                                 "Trade or Technical Certificate",
                               EDUCATION_LEVEL %in% c("07", "08", "09", "11", "12") ~
                                 "Some college (no degree)",
                               EDUCATION_LEVEL %in% c("10") ~
                                 "Associate's Degree",
                               EDUCATION_LEVEL %in% c("13", "14", "19", "20") ~
                                 "Bachelor's Degree",
                               EDUCATION_LEVEL %in% c("17", "18") ~
                                 "Master's Degree",
                               EDUCATION_LEVEL %in% c("15", "16", "21", "22") ~
                                 "Doctoral_Professional Degree",
                               TRUE ~ NA_character_
                             )))
}

#' Function to Transform Race Factor
#'
#' This function transforms the RACE_LONG_NAME variable into race_factor
#' @param x Name of analysis dataset
#' @keywords race_factor
#' @export
#' @examples
#' create_race_factor()

create_race_factor <- function(x)
  {x <- mutate(x,
             race_factor = factor(case_when(
               RACE_LONG_NAME == "BLACK AFRICAN AMERICAN" ~ "Black African American",
               RACE_LONG_NAME == "WHITE" ~ "White",
               RACE_LONG_NAME == "ASIAN" ~ "Asian",
               RACE_LONG_NAME == "AMERICAN INDIAN" ~ "American Indian",
               RACE_LONG_NAME %in% c("HAWAIIAN PACIFIC ISLANDER", "PAC ISL") ~ "Hawaiian Pacific Islander",
               RACE_LONG_NAME %in% c("No Data Reported", "NOT A RACE", "Invalid") ~ "No race reported",
               grepl(",", RACE_LONG_NAME) ~ "Two or more races"
               )))
}

#' Function to Transform Ethnicity Factor
#'
#' This function transforms the RACE_SHORT_NAME variable into ethnicity_factor
#' @param x Name of analysis dataset
#' @keywords ethnicity_factor
#' @export
#' @examples
#' create_ethnicity_factor()

create_ethnicity_factor <- function(x){
  x <- mutate(x,
            ethnicity_factor = factor(case_when(
              RACE_SHORT_NAME == "HISPANIC" ~ "Hispanic",
              RACE_SHORT_NAME == "NO ETHNICITY" ~ "Not Hispanic",
              RACE_SHORT_NAME %in% c("No Data Reported", "Invalid") ~
                                 "No ethnicity reported"
              )))
}

#' Function to Transform Age Factor
#'
#' This function transforms the AGE_YEARS variable into age_factor
#' @param x Name of analysis dataset
#' @keywords age_factor
#' @export
#' @examples
#' create_age_factor()

create_age_factor <- function(x){
  x <- mutate(x,
            age_factor = factor(case_when(
              AGE_YEARS <= 25 ~ "25 and under",
              AGE_YEARS %in% 26:29 ~ "26-29 years old",
              AGE_YEARS %in% 30:39 ~ "30-39 years old",
              AGE_YEARS %in% 40:49 ~ "40-49 years old",
              AGE_YEARS %in% 50:59 ~ "50-59 years old",
              AGE_YEARS >= 60 ~ "60 or older"
              )))
}

#' Function to Transform Length of Service Factor
#'
#' This function transforms the LENGTH_OF_SERVICE_YEARS variable into length_of_service_factor
#' @param x Name of analysis dataset
#' @keywords length_of_service_factor
#' @export
#' @examples
#' create_length_of_service_factor()

create_length_of_service_factor <- function(x){
x <- mutate(x,
            length_of_service_factor = factor(case_when(
              LENGTH_OF_SERVICE_YEARS < 1 ~ "Less than 1 year",
              LENGTH_OF_SERVICE_YEARS %in% 1:3 ~ "1 to 3 years",
              LENGTH_OF_SERVICE_YEARS %in% 4:5 ~ "4 to 5 years",
              LENGTH_OF_SERVICE_YEARS %in% 6:10 ~ "6 to 10 years",
              LENGTH_OF_SERVICE_YEARS %in% 11:14 ~ "11 to 14 years",
              LENGTH_OF_SERVICE_YEARS %in% 15:20 ~ "15 to 20 years",
              LENGTH_OF_SERVICE_YEARS > 20 ~ "More than 20 years"
            )))
}


#' Function to Transform Length of Service Factor
#'
#' This function transforms the VETERANS_STATUS_LONG_NAME variable into veteran_status_factor
#' @param x Name of analysis dataset
#' @keywords veteran_status_factor
#' @export
#' @examples
#' create_veteran_status_factor()

create_veteran_status_factor <- function(x){
  x <- mutate(x,
              veteran_status_factor = factor(case_when(
              VETERANS_STATUS_LONG_NAME %in% c("NOT A VETERAN",
                                               "NOT A VETERAN (ASSUMPTION BASED ON VETERANS PREFERENCE)") ~ 
                "Not a veteran",
              VETERANS_STATUS_LONG_NAME %in% c("POST-VIETNAM-ERA VETERAN", "PRE-VIETNAM-ERA VETERAN",
                                               "VETERAN, ERA UNKNOWN (BASED ON VETERANS PREFERENCE)",
                                               "VIETNAM-ERA VETERAN") ~ "Veteran",
              VETERANS_STATUS_LONG_NAME == "EXEMPT FROM REPORTING" ~ "Exempt from reporting veteran status"
            )))
}

#' This function transforms the DISABILITY_SHORT_NAME variable into disability_factor
#' @param x Name of analysis dataset
#' @keywords disability_factor
#' @export
#' @examples
#' create_disability_factor()

create_disability_factor <- function(x){
  x <- mutate(x,
            disability_factor = factor(case_when(DISABILITY_SHORT_NAME %in% c("I DO NOT HAVE A DISABILITY OR SERIOUS", "NO HANDICAP", "Invalid") 
                               ~ "No disability identified",
                               DISABILITY_SHORT_NAME == "I DO NOT WISH TO IDENTIFY MY DISABILITY" ~
                                 "Does not wish to identify disability",
                               !is.na(DISABILITY_SHORT_NAME) & ! DISABILITY_SHORT_NAME %in%
                                 c("I DO NOT HAVE A DISABILITY OR SERIOUS", "NO HANDICAP", 
                                   "I DO NOT WISH TO IDENTIFY MY DISABILITY") ~
                                 "Disability identified"
            )))
}

#' This function transforms the SUPERVISOR_LONG_NAME variable into supervisor_status_factor
#' @param x Name of analysis dataset
#' @keywords supervisor_status_factor
#' @export
#' @examples
#' create_supervisor_status_factor()

create_supervisor_status_factor <- function(x){
x <- mutate(x,
            supervisor_status_factor = factor(case_when(
              SUPERVISOR_LONG_NAME == "ALL OTHER POSITIONS" ~
                "Not supervisor, manager, or team leader",
              SUPERVISOR_LONG_NAME %in%
                c("LEADER", "MANAGEMENT OFFICIAL (CSRA)",
                  "SUPERVISOR (CSRA)", "SUPERVISOR OR MANAGER",
                  "TEAM LEADER") ~ 
                "Supervisor, manager, or team leader",
              SUPERVISOR_LONG_NAME == "Invalid" ~
                NA_character_
            )))
}

#' This function transforms the WORK_SCHEDULE_LONG_NAME variable into work_schedule_factor
#' @param x Name of analysis dataset
#' @keywords work_schedule_factor
#' @export
#' @examples
#' create_work_schedule_factor()

create_work_schedule_factor <- function(x){
x <- mutate(x,
            work_schedule_factor = factor(case_when(
              WORK_SCHEDULE_LONG_NAME == "FULL-TIME" ~ "Full-time work schedule",
              WORK_SCHEDULE_LONG_NAME %in%
                c("A SCHEDULE REQUIRED WHEN AN EMPLOYEE HAS ELECTED PHASED EMPLOYMENT/PHASED RETIREMENT STATUS AND HAS A LESS THA",
                  "BAYLOR PLAN", "FULL-TIME SEASONAL",
                  "INTERMITTENT", "INTERMITTENT SEASONAL",
                  "Invalid", "PART-TIME", "PART-TIME JOB SHARER",
                  "PART-TIME SEASONAL") ~ "Work schedule other than full time"
              )))
}

#' This function transforms the JOB_SERIES variable into job_series_factor
#' @param x Name of analysis dataset
#' @keywords job_series_factor
#' @export
#' @examples
#' create_job_series_factor()

create_job_series_factor <- function(x){
  x <- mutate(x,
            job_series_factor = factor(case_when(
              JOB_SERIES %in% c('1102', '1105', '1106') ~ "Acquisition",
              JOB_SERIES %in% c('2210') ~ "IT",
              JOB_SERIES %in% c('0510', '0501') ~ "Financial management",
              JOB_SERIES %in% c('0201') ~ "Human capital",
              TRUE ~ "Non-mission support job series"
            )))
}

#' This function transforms the DUTY_STATION_STATE_CD variable into location_factor
#' @param x Name of analysis dataset
#' @keywords job_series_factor
#' @export
#' @examples
#' create_location_factor()

create_location_factor <- function(x){
x <- mutate(x,
            location_factor = factor(case_when(
              DUTY_STATION_STATE_CD %in% c('DC', 'VA', 'MD') ~ "DC, VA, MD duty station",
              TRUE ~ "Other duty station"
            )))
}
