###############################################################################
# Plotting types of bad in ISA for QAOA classical opts
#
# Author: Vivek Katial
# Created At: 2024-03-19 08:38:06.839435
###############################################################################

library(tidyverse)
library(here)

d_isa <- read_csv(here("data/isa/qaoa-classical-opts-init/algorithm_raw.csv"))
d_isa_processed <- read_csv(here("data/isa/qaoa-classical-opts-init/algorithm_process.csv"))
d_coords <- read_csv(here("data/isa/qaoa-classical-opts-init/coordinates.csv"))

mark_type_bad <- function(df, kappa = 0.9) {
  # Determine the numeric columns, excluding the first column which might be an identifier
  numeric_columns <- names(select(df, where(is.numeric)))[-1]
  
  df %>%
    rowwise() %>%
    mutate(
      Best_Value = min(c_across(all_of(numeric_columns)), na.rm = TRUE),
      across(all_of(numeric_columns),
             ~ case_when(
               .x == 100000 ~ "Type 2 Bad", # Check for Type 2 Bad first
               .x > Best_Value / kappa ~ "Type 1 Bad", # Then check for Type 1 Bad
               TRUE ~ "Good" # If neither, keep the original value
             ),
             .names = "Marked_{.col}")
    )
}

# Apply the function to the data frame
df_marked <- mark_type_bad(d_isa) %>% 
  select(Row, starts_with("Marked_"))

# Join on marked columns to coordinates
d_coords %>% 
  left_join(df_marked, by = "Row") %>% 
  gather(-Row,-z_1, -z_2, key="Algorithm", value="Type") %>%
  # Plot coordinates
  ggplot(aes(x = z_1, y = z_2, color=Type)) +
  geom_point(alpha=0.4) + 
  facet_wrap(~Algorithm) + 
  theme_Publication()
  
  
  
  
  
  
  
  
  
