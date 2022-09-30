#' Function to Get Mode from Vector
#'
#' This function returns the mode from a vector
#' @param v Vector to get mode from
#' @keywords mode
#' @export
#' @examples
#' getmode()

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Function to Impute Education Factor
#'
#' This function imputes NA values for education_factor
#' @param x Name of analysis dataset
#' @keywords education
#' @export
#' @examples
#' impute_education_factor()

impute_education_factor <- function(x){
  
  if(sum(is.na(x$education_factor)) == 0){
    return(x)
    stop()
  }
  
  # Note which rows had education_factor imputed
  x <- x %>% 
    mutate(education_factor_imputed = is.na(education_factor))
  
  # Attempt to impute by agency, component, and grade level
  x <- x %>% group_by(AGENCY, COMPONENT, GRADE_LEVEL) %>%
    mutate(education_factor = if_else(is.na(education_factor),
                                      getmode(education_factor),
                                      education_factor)) %>%
    ungroup()
  
  if(sum(is.na(x$education_factor)) == 0){
    return(x)
    stop()
  }
  
  # Impute by mode within agency and grade level
  x <- x %>% group_by(AGENCY, GRADE_LEVEL) %>%
    mutate(education_factor = if_else(is.na(education_factor),
                                      getmode(education_factor),
                                      education_factor)) %>%
    ungroup()
  
  if(sum(is.na(x$education_factor)) == 0){
    return(x)
    stop()
  }
  
  # Impute by mode within agency and job group
  x <- x %>% group_by(AGENCY, JOB_GROUP_CD) %>%
    mutate(education_factor = if_else(is.na(education_factor),
                                      getmode(education_factor),
                                      education_factor)) %>%
    ungroup()
  
  if(sum(is.na(x$education_factor)) != 0){
    warning("There are still NA values for education_factor")
  }
  
  return(x)
}

#' Function to Impute GENDER
#'
#' This function imputes NA and '*' values for GENDER
#' @param x Name of analysis dataset
#' @keywords gender
#' @export
#' @examples
#' impute_gender()

impute_gender <- function(x){
  
  if(sum(is.na(x$GENDER) + sum(x$GENDER == '*')) == 0){
    return(x)
    stop()
  }
  
  # Note which rows had GENDER imputed
  x <- x %>% 
    mutate(gender_imputed = is.na(GENDER) | GENDER == '*')
  
  # Impute gender by most common within agency and component
  x <- x %>% group_by(AGENCY, COMPONENT) %>%
    mutate(GENDER = if_else(GENDER == "*",
                            getmode(GENDER),
                            GENDER)) %>%
    ungroup()
  
  if(sum(is.na(x$GENDER)) + sum(x$GENDER == '*') != 0){
    warning("There are still NA or '*' values for GENDER")
  }
  
  return(x)
}

#' Function to Impute age_factor
#'
#' This function imputes NA values for age_factor
#' @param x Name of analysis dataset
#' @keywords age_factor
#' @export
#' @examples
#' impute_age_factor()

impute_age_factor <- function(x){
  
  if(sum(is.na(x$age_factor)) == 0){
    return(x)
    stop()
  }
  
  # Note which rows had age_factor imputed
  x <- x %>% 
    mutate(age_factor_imputed = is.na(age_factor))
  
  # Impute age_factor by most common within agency
  x <- x %>% group_by(AGENCY) %>%
    mutate(age_factor = if_else(is.na(age_factor),
                                getmode(age_factor),
                                age_factor)) %>%
    ungroup()
  
  if(sum(is.na(x$age_factor)) != 0){
    warning("There are still NA values for age_factor")
  }
  
  return(x)
}

#' Function to Impute length_of_service_factor
#'
#' This function imputes NA values for length_of_service_factor
#' @param x Name of analysis dataset
#' @keywords length_of_service_factor
#' @export
#' @examples
#' impute_length_of_service_factor()

impute_length_of_service_factor <- function(x){
  
  if(sum(is.na(x$length_of_service_factor)) == 0){
    return(x)
    stop()
  }
  
  # Note which rows had length_of_service_factor imputed
  x <- x %>% 
    mutate(length_of_service_factor_imputed = is.na(length_of_service_factor))
  
  # Impute length_of_service_factor by most common within agency
  x <- x %>% group_by(AGENCY) %>%
    mutate(length_of_service_factor = if_else(is.na(length_of_service_factor),
                                getmode(length_of_service_factor),
                                length_of_service_factor)) %>%
    ungroup()
  
  if(sum(is.na(x$length_of_service_factor)) != 0){
    warning("There are still NA values for length_of_service_factor")
  }
  
  return(x)
}


#' Function to Impute supervisor_status_factor
#'
#' This function imputes NA values for supervisor_status_factor
#' @param x Name of analysis dataset
#' @keywords supervisor_status_factor
#' @export
#' @examples
#' impute_supervisor_status_factor()

impute_supervisor_status_factor <- function(x){
  
  if(sum(is.na(x$supervisor_status_factor)) == 0){
    return(x)
    stop()
  }
  
  # Note which rows had supervisor_status_factor imputed
  x <- x %>% 
    mutate(supervisor_status_factor_imputed = is.na(supervisor_status_factor))
  
  # Impute length_of_service_factor by most common within agency
  x <- x %>% group_by(AGENCY, COMPONENT, GRADE_LEVEL) %>%
    mutate(supervisor_status_factor = if_else(is.na(supervisor_status_factor),
                                              getmode(supervisor_status_factor),
                                              supervisor_status_factor)) %>%
    ungroup()
  
  if(sum(is.na(x$supervisor_status_factor)) != 0){
    warning("There are still NA values for supervisor_status_factor")
  }
  
  return(x)
}

#' Function to Impute TOTAL_ANNUAL_PAY
#'
#' This function imputes NA values for TOTAL_ANNUAL_PAY
#' @param x Name of analysis dataset
#' @keywords TOTAL_ANNUAL_PAY
#' @export
#' @examples
#' impute_total_annual_pay()

impute_total_annual_pay <- function(x){
  
  if(sum(is.na(x$TOTAL_ANNUAL_PAY)) == 0){
    return(x)
    stop()
  }
  
  # Since we are not writing over TOTAL_ANNUAL_PAY,
  # You can see which rows are imputed by looking for
  # NAs in TOTAL_ANNUAL_PAY
  
  # First try to impute TOTAL_ANNUAL_PAY as median by component, grade level
  
  x <- x %>% 
    group_by(AGENCY, COMPONENT, GRADE_LEVEL) %>%
    mutate(total_annual_pay_imputed = if_else(is.na(TOTAL_ANNUAL_PAY),
                                              median(TOTAL_ANNUAL_PAY, na.rm = TRUE),
                                              TOTAL_ANNUAL_PAY)) %>%
    ungroup()
  
  if(sum(is.na(x$total_annual_pay_imputed)) == 0){
    return(x)
    stop()
  }
  
  # Next impute as median by
  # component, supervisory status, and education
  x <- x %>%
    group_by(AGENCY, COMPONENT, supervisor_status_factor, education_factor) %>%
    mutate(total_annual_pay_imputed = if_else(is.na(TOTAL_ANNUAL_PAY),
                                              median(TOTAL_ANNUAL_PAY, na.rm = TRUE),
                                              TOTAL_ANNUAL_PAY)) %>%
    ungroup()
  
  
  if(sum(is.na(x$total_annual_pay_imputed)) != 0){
    warning("There are still NA values for total_annual_pay_imputed")
  }
  
  return(x)
}

#' Function to Impute telework_eligibility_factor
#'
#' This function imputes NA values for telework_eligibility_factor
#' @param x Name of analysis dataset
#' @keywords telework_eligibility_factor
#' @export
#' @examples
#' impute_telework_eligibility_factor()

impute_telework_eligibility_factor <- function(x){
  
  if("telework_eligibility_factor" %in% colnames(x)){
    if(sum(is.na(x$telework_eligibility_factor)) == 0){
      return(x)
      stop()
    }
  }
  # Impute telework_eligibility_factor as mode by agency, component, and grade level
  x <- x %>%
    group_by(AGENCY, COMPONENT, GRADE_LEVEL) %>%
    mutate(telework_eligibility_factor = if_else(! TLWRK_ELIG_IND %in% c("Y", "N") | is.na(TLWRK_ELIG_IND),
                                                 getmode(TLWRK_ELIG_IND),
                                                 TLWRK_ELIG_IND)) %>%
    # There was 1 row going to " " with getmode, assigning to N as most common
    mutate(telework_eligibility_factor = if_else(telework_eligibility_factor %in% c("Y", "N"),
                                                 telework_eligibility_factor,
                                                 as.factor("N"))) %>%
    ungroup()
  
  # Removing unused levels, which were blank or '*' in TLWRK_ELIG_IND
  x$telework_eligibility_factor <- droplevels(x$telework_eligibility_factor)
  
  if(sum(is.na(x$telework_eligibility_factor)) != 0){
    warning("There are still NA values for telework_eligibility_factor")
  }
  
  return(x)
}
