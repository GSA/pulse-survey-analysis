#' Function to Relevel Factor
#'
#' This function relevels factor variables to set the base level, and removes unused level
#' @param x Name of analysis dataset
#' @keywords relevel
#' @export
#' @examples
#' relevel_factors()

relevel_factors <- function(x){
  x$age_factor <- relevel(x$age_factor, "50-59 years old")
  x$race_factor <- relevel(x$race_factor, "White")
  x$ethnicity_factor <- relevel(x$ethnicity_factor, "Not Hispanic")
  x$GENDER <- droplevels(x$GENDER) # Remove unused level '*'
  x$GENDER <- relevel(x$GENDER, "M")
  x$disability_factor <- relevel(x$disability_factor, "No disability identified")
  x$education_factor <- relevel(x$education_factor, "Bachelor's Degree")
  x$length_of_service_factor <- relevel(x$length_of_service_factor, "More than 20 years")
  x$veteran_status_factor <- relevel(x$veteran_status_factor, "Not a veteran")
  x$AGENCY <- relevel(x$AGENCY, "Department of Defense")
  
  x$telework_eligibility_factor <- relevel(x$telework_eligibility_factor, "N")
  x$supervisor_status_factor <- relevel(x$supervisor_status_factor, "Not supervisor, manager, or team leader")
  x$job_series_factor <- relevel(x$job_series_factor, "Non-mission support job series")
  x$location_factor <- relevel(x$location_factor, "Other duty station")
  x$US_CITIZENSHIP_IND <- relevel(as.factor(x$US_CITIZENSHIP_IND), "Y")
  
  return(x)
}

#' Function to Run Models and Print Coefficients
#'
#' This function runs basic and expended models, weighted and unweighted
#' @param x Name of analysis dataset
#' @keywords model
#' @export
#' @examples
#' run_full_model_wt()
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