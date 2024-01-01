###############################################################################
# Utility functions for matilda prep
#
# Author: Vivek Katial
# Created 2023-12-20 12:48:24
###############################################################################

extract_real <- function(x) {
  # Check if the string contains "+0j"
  if (!grepl("\\+.*j", x)) {
    return(as.numeric(x))
  }
  
  # Remove the brackets
  x <- gsub("\\(|\\)", "", x)
  # Remove the imaginary part
  x <- gsub("\\+.*$", "", x)
  
  # Convert to numeric
  x <- as.numeric(x)
  
  # Return the result
  return(x)
}

