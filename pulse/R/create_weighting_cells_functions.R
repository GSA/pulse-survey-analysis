# This functionality has not yet been used. We have only calculated
# weights government-wide (see pulse_1_regression.R, pulse_2_regression.R,
# and pulse_3_regression.R) and for each agency (pulse_1_regression_by_agency.R,
# pulse_2_regression_by_agency.R, and pulse_3_regression_by_agency.R)

# This code creates weighting cells at the component level. It combines smaller
# components into a single weighting cell, based on the parameters set.

# # Create weighting sets  ----
# # See weighting set if we weighted all components with >= 500
# # employees on their own
# new_cell_threshold <- 500
# minimum_cell_size <- 200
# 
# component_cells <- data.frame()
# sub_component_cells <- data.frame()
# 
# cells <- analysis_dataset %>%
#   group_by(AGENCY, COMPONENT) %>%
#   summarize(n = n()) %>%
#   arrange(AGENCY, desc(n))
# 
# agency_list <- unique(cells$AGENCY)
# agency_cells <- data.frame()
# 
# calc_max_cell_1 <- function(){
#   out <- max(component_cells$n[component_cells$cell_number_component == 1 &
#                       component_cells$COMPONENT != 'Other' &
#                        ! is.na(component_cells$COMPONENT)])
#   return(out)
# }
# max_cell_1 <- calc_max_cell_1()
# 
# calc_sum_cell_1 <- function(){
#   out <- sum(component_cells$n[component_cells$cell_number_component == 1])
#   return(out)
# }
# sum_cell_1 <- calc_sum_cell_1()
# 
# for(a in agency_list){
#   print(paste0("now splitting agency ", a, " into cells"))
#   
#   cells <- analysis_dataset %>%
#     group_by(AGENCY, COMPONENT) %>%
#     summarize(n = n()) %>%
#     arrange(AGENCY, desc(n))
#   
#   component_cells <- cells %>%
#     filter(AGENCY == a) %>%
#     mutate(cell_number_component = 1) %>%
#     group_by(cell_number_component) %>%
#     mutate(cell_size = sum(n))
#   
#   max_cell_1 <- calc_max_cell_1()
#   sum_cell_1 <- calc_sum_cell_1()
#   
#   new_cell_number <- 1
#   
# while(max_cell_1 > new_cell_threshold & 
#       sum_cell_1 - max_cell_1  > minimum_cell_size){
#     new_cell_number = new_cell_number + 1
#     component_cells <- component_cells %>%
#       mutate(cell_number_component = ifelse(n == max_cell_1, new_cell_number, cell_number_component)) %>%
#       group_by(cell_number_component) %>%
#       mutate(cell_size = sum(n)) %>%
#       ungroup()
#     print("update component cell number")
#     
#     max_cell_1 <- calc_max_cell_1()
#     sum_cell_1 <- calc_sum_cell_1()
#     print(max_cell_1)
#     print(sum_cell_1)
#   }
#   agency_cells <- rbind(agency_cells, component_cells)
#   
#   ## Create weighting cells for subcomponents ----
#   cells <- analysis_dataset %>%
#     group_by(AGENCY, COMPONENT, SUB_COMPONENT) %>%
#     filter(AGENCY == a) %>%
#     summarize(n = n()) %>%
#     arrange(AGENCY, desc(n))
# 
#   component_list <- unique(cells$COMPONENT)
# 
#   calc_max_cell_1_sub <- function(){
#     out <- max(sub_c_cells$n[sub_c_cells$cell_number_subcomponent == 1 &
#                                sub_c_cells$SUB_COMPONENT != 'Other' &
#                                ! is.na(sub_c_cells$SUB_COMPONENT)])
#     return(out)
#   }
# 
#   calc_sum_cell_1_sub <- function(){
#     out <- sum(sub_c_cells$n[sub_c_cells$cell_number_subcomponent == 1])
#     return(out)
#   }
# 
#   # Create sub-component cells
#   for(c in component_list){
#     print(paste0("now splitting component ", c, " into cells"))
#     cell_number_iter <- 1
#     new_cell_number <- cell_number_iter
# 
#     sub_c_cells <- cells %>%
#       filter(COMPONENT == c) %>%
#       mutate(cell_number_subcomponent = cell_number_iter) %>%
#       group_by(cell_number_subcomponent) %>%
#       mutate(cell_size = sum(n))
# 
#     max_cell_1_sub <- calc_max_cell_1_sub()
#     sum_cell_1_sub <- calc_sum_cell_1_sub()
# 
#     while(max_cell_1_sub > new_cell_threshold &
#           sum_cell_1_sub - max_cell_1_sub  > minimum_cell_size){
#       new_cell_number = new_cell_number + 1
#       sub_c_cells <- sub_c_cells %>%
#         mutate(cell_number_subcomponent = ifelse(n == max_cell_1_sub, new_cell_number, cell_number_subcomponent)) %>%
#         group_by(cell_number_subcomponent) %>%
#         mutate(cell_size = sum(n)) %>%
#         ungroup()
#       print("update subcomponent cell number")
# 
#       max_cell_1_sub <- calc_max_cell_1_sub()
#       sum_cell_1_sub <- calc_sum_cell_1_sub()
#       print(max_cell_1_sub)
#       print(sum_cell_1_sub)
#     }
#     sub_component_cells <- rbind(sub_component_cells, sub_c_cells)
#     }
# }
# 
# head(agency_cells)
# head(sub_component_cells)
# 
# weighting_cells <- analysis_dataset %>%
#   group_by(AGENCY, COMPONENT, SUB_COMPONENT) %>%
#   summarize(sub_component_size = n(),
#             num_responses = sum(RESPONSE)) %>%
#   mutate(sub_component_response_rate = num_responses / sub_component_size) %>%
#   left_join(y = agency_cells, by = c("AGENCY", "COMPONENT")) %>%
#   select(-n, -cell_size) %>%
#   left_join(y = sub_component_cells, by = c("AGENCY", "COMPONENT", "SUB_COMPONENT")) %>%
#   select(-n, -cell_size) %>%
#   ungroup() %>%
#   mutate(cell_number_subcomponent = if_else(cell_number_component == 1,
#                                             1,
#                                             cell_number_subcomponent)) %>%
#   arrange(AGENCY, cell_number_component, cell_number_subcomponent)
# 
# weighting_cell_size <- weighting_cells %>%
#   group_by(AGENCY, cell_number_component, cell_number_subcomponent) %>%
#   summarize(cell_size = sum(sub_component_size),
#             num_responses = sum(num_responses)) %>%
#   mutate(response_rate = num_responses / cell_size) %>%
#   arrange(AGENCY, cell_number_component, cell_number_subcomponent)
# 
# write.csv(x = weighting_cells, file = "weighting_cells.csv", row.names = FALSE)
# write.csv(x = weighting_cell_size, file = "weighting_cell_size.csv", row.names = FALSE)
# 
# 
# # Merge in weighting sets ----
# 
# weighting_cells_for_merge <- weighting_cells %>%
#   select(AGENCY, COMPONENT, SUB_COMPONENT, cell_number_component, cell_number_subcomponent)
# 
# analysis_dataset <- analysis_dataset %>%
#   left_join(y = weighting_cells_for_merge, by = c("AGENCY", "COMPONENT", "SUB_COMPONENT"))
# 
# colnames(analysis_dataset)
# 