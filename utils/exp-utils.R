###############################################################################
# Util functions for running an experiment
#
# Created Date: Wed Aug 28 16:14:31 2019
# Author: Vivek Katial
############################################################################### 

extract_params <- function(params_file){
  tryCatch(
    read_yaml(params_file),
    error = function(e){
      logerror("Unable to load yaml file '%s' - %s", params_file, e)
    }
  )
}

# Function to source all experiment scripts
source_exp_scripts <- function(params){
  # Loop through all parameters
  for (i in names(params)) {
    # Check if source inside params setting
    if ("source" %in% names(params[[i]])) {
      # Source in scripts
      for (j in params[[i]][["source"]]) {
        loginfo("Sourcing Script: '%s'", j)
        # Load scripts
        tryCatch(
          source(j),
          error = function(e){
            logerror("Unable to source script file '%s' - %s", params[[i]][["source"]], e)
          }
        )
      }
    }
  }
}

source_exp_utils <- function(params){
  for (i in params$experiment$utils){
    # Print message
    loginfo("Sourcing Utility Script: %s", i)
    
    # Load scripts
    tryCatch(
      source(i),
      error = function(e){
        logerror("Unable to source utility script file '%s' - %s", i, e)
      }
    )
  }
}

# Convert integer to bit
convert_int_to_bit = function(int){
  # Integer
  bit <- intToBits(int) %>% 
    # Reverse order
    rev() %>% 
    # Conv to int
    as.numeric() %>% 
    paste0(collapse = "") %>% 
    as.numeric()
  
  if (is.na(bit)) {
    stop(sprintf("Bit converstion for '%s' is invalid, returning 'NA'", int))
  } else {
    bit
  }
  
}

#' Clean hamiltonian function
#' This function takes in a matrix object (Hamiltonian) and enriches decimal and bit_string
#' @param hamiltonian Matrix object (Hamiltonian)
#' @param n_qubits Number of qubits for experiment
.clean_hamiltonian = function(hamiltonian, n_qubits){
  
  hamiltonian %>% 
    as.data.frame() %>% 
    tbl_df() %>% 
    mutate(ind = 0:(n()-1)) %>% 
    mutate(
      bit_str = map_chr(ind, convert_int_to_bit),
      bit_str = str_remove_all(bit_str, "\\..*"),
      bit_str = str_pad(bit_str, side = "left", pad = "0", width = n_qubits)
      )
}
