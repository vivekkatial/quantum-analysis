###############################################################################
# Script to build analysis
#
# Author: Vivek Katial
# Created 2023-12-19 15:45:25
###############################################################################

library(tidyverse)
library(glue)
library(latex2exp)


Sys.time()

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

RUN_DATETIME <- as.POSIXct("2023-11-23 11:45:00 AEDT", tz = "UTC")
system_size = 8

# Adding stuff for QAOA
d_runs <- read_csv("data/d_QAOA-Parameter-layers-vanilla.csv") %>% 
  filter(
    start_time > RUN_DATETIME,
    status == "FINISHED"
  ) %>% 
  select(Instances = run_id, Source = params.instance_class, contains("params"), contains("metrics")) %>% 
  # Clean up random complex numbers coming through
  rowwise() %>% 
  mutate(across(starts_with("params.") & is.character, extract_real))



# Runs --------------------------------------------------------------------
d_runs %>% 
  count(Source) %>%
  arrange(n) %>%
  ggplot(aes(x = reorder(Source, -n), y = n)) +
  geom_col() +
  theme_light() +  # You can change the theme to your preference
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis ticks
  labs(title = "Source Distribution", x = "Source", y = "Count")  # Set title and axis labels

d_runs %>% 
  count(params.instance_size) %>% 
  mutate(params.instance_size = as.factor(params.instance_size)) %>% 
  rename(system_size = params.instance_size) %>% 
  ggplot(aes(x = system_size, y=n)) + 
  geom_col() +
  theme_light() +  # You can change the theme to your preference
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis ticks
  labs(title = "# of Nodes", x = "System Size", y = "Count")  # Set title and axis labels

# Prepare your data
prepared_data <- d_runs %>%
  filter(
    params.instance_size == 8,
    params.n_layers == 1
  ) %>%
  mutate(
    Source = str_replace_all(Source, "_", " "),
    Source = str_to_title(Source),
    OptimalGamma = abs(metrics.optimal_gamma_1)/pi,
    OptimalBeta = abs(metrics.optimal_beta_1)/pi
  ) %>% 
  select(OptimalGamma, OptimalBeta, Source) %>% 
  gather(param, val, -Source) %>%
  mutate(param = ifelse(param == "OptimalGamma", "gamma_star", "beta_star"))

# Calculate medians
medians <- prepared_data %>%
  group_by(Source, param) %>%
  summarize(median_val = median(val))

# Create the plot
ggplot(prepared_data, aes(x = val, fill = param)) +
  geom_density(alpha=0.4, aes(y = ..density..)) +
  geom_vline(data = medians, aes(xintercept = median_val, color = param), linetype = "dashed", show.legend = FALSE) +
  scale_fill_manual(
    values = c("gamma_star" = "blue", "beta_star" = "red"), 
    labels = c(TeX("$\\gamma^*/\pi$"), TeX("$\\beta^*/\pi$"))
  ) +
  scale_color_manual(
    values = c("gamma_star" = "blue", "beta_star" = "red")
  ) + 
  theme_classic() +
  facet_wrap(~Source, scales = "free_y") +
  labs(x = "",
       y = "Density") +
  theme(strip.text = element_text(face = "bold"), axis.ticks.y = element_blank(), legend.title = element_blank()) + 
  xlim(c(-0.2, 1))

# Assuming your dataframe is named 'd_runs'
optimal_parameters_median = d_runs %>%
  group_by(Source, params.n_layers) %>%
  summarize(
    median_beta_1 = median(metrics.optimal_beta_1, na.rm = TRUE),
    median_gamma_1 = median(metrics.optimal_gamma_1, na.rm = TRUE),
    median_beta_2 = median(metrics.optimal_beta_2, na.rm = TRUE),
    median_gamma_2 = median(metrics.optimal_gamma_2, na.rm = TRUE),
    median_beta_3 = median(metrics.optimal_beta_3, na.rm = TRUE),
    median_gamma_3 = median(metrics.optimal_gamma_3, na.rm = TRUE),
    median_beta_4 = median(metrics.optimal_beta_4, na.rm = TRUE),
    median_gamma_4 = median(metrics.optimal_gamma_4, na.rm = TRUE),
    median_beta_5 = median(metrics.optimal_beta_5, na.rm = TRUE),
    median_gamma_5 = median(metrics.optimal_gamma_5, na.rm = TRUE)
  ) %>% 
  group_by(Source) %>% 
  arrange(params.n_layers)

optimal_parameters_median %>% 
  write_csv("data/optimal-parameters.csv")
