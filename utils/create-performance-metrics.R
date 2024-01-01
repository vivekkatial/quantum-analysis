###############################################################################
# Script for algorithm INFORMS ISA
#
# Author: Vivek Katial
# Created At: 2024-01-01 15:27:18.657598
###############################################################################


create_performance_metric <- function(data, massive_number = 1e5) {
  # Ensure that the input is a DataFrame
  if (!is.data.frame(data)) {
    stop("Input data must be a DataFrame.")
  }
  
  # Check for necessary columns: init_type, approximation_ratio, eval_count
  required_columns <- c("init_type", "approximation_ratio", "eval_count")
  if (!all(required_columns %in% names(data))) {
    stop("Data must contain 'init_type', 'approximation_ratio', and 'eval_count' columns.")
  }
  
  # Compute the maximum approximation ratio
  max_approximation_ratio <- max(data$approximation_ratio, na.rm = TRUE)
  
  # Filter and summarize for groups that meet the criterion
  filtered_data <- dplyr::filter(data, approximation_ratio >= .95 * max_approximation_ratio) %>%
    dplyr::group_by(init_type) %>%
    dplyr::summarise(min_fevals_to_reach_good_ar = min(eval_count, na.rm = TRUE))
  
  # Identify the excluded groups and impute values
  all_init_types <- unique(data$init_type)
  excluded_init_types <- setdiff(all_init_types, filtered_data$init_type)
  excluded_data <- data.frame(init_type = excluded_init_types, 
                              min_fevals_to_reach_good_ar = massive_number)
  
  # Combine the results
  final_data <- rbind(filtered_data, excluded_data)
  
  # Convert to a key-value list
  key_value_list <- split(final_data$min_fevals_to_reach_good_ar, final_data$init_type)
  key_value_list <- lapply(key_value_list, function(df) df[1])
  
  return(key_value_list)
}

# Example run
# create_performance_metric(d_run_example)
# d_run_example <- read_csv(here("data/example-run.csv"))