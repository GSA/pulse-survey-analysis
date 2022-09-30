#' Function to Get Covariates
#'
#' This function returns a vector with the covariates that are reported on
#' @keywords covariates
#' @export
#' @examples
#' get_covariates()

get_covariates <- function(){ c('total_annual_pay_imputed_5000','age_factor','race_factor','ethnicity_factor',
               'GENDER','disability_factor','education_factor','length_of_service_factor',
               'veteran_status_factor','AGENCY','telework_eligibility_factor',
               'supervisor_status_factor','job_series_factor','location_factor',
               'US_CITIZENSHIP_IND') }
# Does not include interaction effects

#' Function to Get Covariates
#'
#' This function returns a vector with the covariate groups that are reported on
#' @keywords groups
#' @export
#' @examples
#' get_groups()
get_groups <- function(){ c(quo(age_factor), quo(race_factor), quo(ethnicity_factor),
            quo(GENDER), quo(disability_factor), quo(education_factor),
            quo(length_of_service_factor), quo(veteran_status_factor), quo(AGENCY),
            quo(telework_eligibility_factor), quo(supervisor_status_factor),
            quo(job_series_factor), quo(location_factor), quo(US_CITIZENSHIP_IND))}


#' Function to Get Government-wide Means
#'
#' This function outputs a data frame with government-wide means, weighted means, SD, weighted SD, and number of responses
#' @param x dataset with responses and weights to get means from
#' @param question_codes vector with column names to get statistics for
#' @param groups vector with quosures of covariate column names
#' @keywords mean, weighted mean, standard deviation
#' @export
#' @examples
#' get_government_wide_means(analysis_dataset, question_codes, groups)
 
get_government_wide_means <- function(x, question_codes, groups){
  output_df <- data.frame()
  for (i in seq_along(groups)){
    varName <- as.character(rlang::quo_get_expr(groups[[i]]))
    df0 <- x %>%
      group_by(!!groups[[i]]) %>%
      summarise(across(all_of(question_codes), 
                     .fns = list(mean = ~mean(., na.rm = TRUE),
                                 weighted.mean = ~weighted.mean(., w = wt, na.rm = TRUE),
                                 n.responses = ~sum(!is.na(.)),
                                 sd = ~sqrt(var(., na.rm = TRUE)),
                                 weighted.sd = ~sqrt(wtd.var(., weights = wt, na.rm = TRUE))
                     ))) %>%
      mutate(variable = varName) %>%
      rename(variable_levels = 1) %>%
      pivot_longer(-c(variable, variable_levels),
                   names_to = c("track", "q", "metric"),
                   names_sep = "_") %>%
      mutate(model = paste0(track, "_", q)) %>%
      select(-c(track, q))
    
    output_df <- rbind(output_df, df0)
    rm(df0)
  }
}

#' Function to Get Government-wide Regression Adjusted Means
#'
#' This function outputs a data frame with government-wide regression adjusted means
#' @param models List of models to get the regression-adjusted means from 
#' @keywords regression adjusted means, estimated marginal means
#' @export
#' @examples
#' get_government_wide_adjusted_means()

get_government_wide_adjusted_means <- function(models){
  #create empty df to store regression-adjusted means
  output_df = data.frame(matrix(ncol=6, nrow=0))

  # loop over models and covariates to produce regression-adjusted means
  # and store in output data frame (input to shiny app)

  for (model in models){
    model_name = as.character(model$terms[[2]])  #store outcome variable (question)
    for (x in covariates){
    
      # covariate specific regression adjusted average
      df_new = data.frame(emmeans(model, 
                                  specs = x,
                                  non.nuisance = x,
                                  weights='proportional'))
      colnames(df_new)[1] = 'variable_levels'   #standardize term column
      df_new$model = rep(model_name, nrow(df_new))  #create model label for all levels of covariate
    
    # combine USG avg and covariate specific averages and bind to output df
    df_new$variable = rep(x, nrow(df_new))      #create covariate label for all levels of covariate
    output_df = rbind.data.frame(output_df, df_new)   #bind to output df
    }
  }
  
  return(output_df)
}

#' Function to Get Covariate Labels
#'
#' This function returns a vector with plain-text labels for covariates that are shown
#' in the Shiny dashboard
#' @keywords covariate_labels
#' @export
#' @examples
#' get_covariate_labels()

get_covariate_labels <- function(){
  c('Age','Race','Ethnicity','Gender','Disability Status','Education',
    'Length of Service','Veteran Status', 'Agency', 'Telework Eligibility',
    'Supervisor Status','Job Series','Location','Citizenship') 
  #' I am only including the factor variables, so you'll note that pay is not included
  #' This should include everything that's supposed to be in our full model
}

#' Function to Map Labels with Variable Names
#'
#' This function returns a map between variable names and labels for the Shiny
#' dashboard
#' @keywords var_map
#' @export
#' @examples
#' get_var_map()

get_var_map <- function(){
# label-var_name mapping
var_map = hash::hash()

var_map[['Age']] = "age_factor"
var_map[['Race']] = "race_factor"
var_map[['Ethnicity']] = "ethnicity_factor"
var_map[['Gender']] = "GENDER"
var_map[['Disability Status']] = "disability_factor"
var_map[['Education']] = "education_factor"
var_map[['Length of Service']] = "length_of_service_factor"
var_map[['Veteran Status']] = "veteran_status_factor"
var_map[['Agency']] = "AGENCY"
var_map[['Telework Eligibility']] = "telework_eligibility_factor"
var_map[['Supervisor Status']] = "supervisor_status_factor"
var_map[['Job Series']] = 'job_series_factor'
var_map[['Location']] = 'location_factor'
var_map[['Citizenship']] = 'US_CITIZENSHIP_IND'

return(var_map)
}