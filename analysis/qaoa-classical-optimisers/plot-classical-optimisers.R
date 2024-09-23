###############################################################################
# Analysis scripts
#
# Author: Vivek Katial
# Created At: 2024-03-04 14:25:16.558246
###############################################################################

library(tidyverse)
library(here)
library(glue)
library(RColorBrewer)

# Source utils
source(here("utils/ggplot_theme_Publication-2.R"))

df <- read_csv("data/d_QAOA-Classical-Optimization_all_runs.csv")

# Algorithms to remove from ISA
algos_to_select <- c("CG", "L_BFGS_B", "SLSQP", "NELDER_MEAD", "POWELL")


# Approximation Ratio -----------------------------------------------------


# Compare all algorithms across all instance Sources
df %>% 
  select(Source = params.instance_class, contains("approx")) %>% 
  # Rename columns wit `metrics.QAOA_` with just their algorithm name
  rename_all(~str_remove(., "metrics.QAOA_")) %>% 
  # Remove approximation ratio in col name
  rename_all(~str_remove(., "_approximation_ratio")) %>%
  gather(-Source, key = "algo", value = "value") %>%
  # remove COBYLA, GradientDescent, TNC
  filter(!(algo %in% c("COBYLA", "GradientDescent", "TNC"))) %>% 
  filter(value > 0.5) %>% 
  # mutate(algo_to_remove = ifelse(algo %in% algos_to_select, "Keep", "Remove")) %>%
  ggplot(aes(x = algo, y = value)) + #, fill=algo_to_remove)) +
  geom_boxplot(fill = "lightblue") +
  # scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "",
       x = "Algorithm",
       y = "Approximation Ratio") + 
  theme_Publication() +
  # Tilt algorithms by 50 degrees
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  # remove legend title
  theme(legend.title = element_blank())

# Compare all algorithms by instance Sources
df %>% 
  select(Source = params.instance_class, contains("approx")) %>% 
  # Clean up Source names into Title Case and remove `_`
  mutate(Source = str_to_title(str_replace_all(Source, "_", " "))) %>%
  # Rename columns wit `metrics.QAOA_` with just their algorithm name
  rename_all(~str_remove(., "metrics.QAOA_")) %>% 
  # Remove approximation ratio in col name
  rename_all(~str_remove(., "_approximation_ratio")) %>%
  gather(-Source, key = "algo", value = "value") %>%
  # mutate(algos_to_remove = ifelse(algo %in% algos_to_select, "Keep", "Remove")) %>%
  filter(value > 0) %>% 
  filter(value > 0.5) %>% 
  
  ggplot(aes(x = algo, y = value)) +
  geom_boxplot(fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  # Color fill 
  scale_fill_brewer(palette = "Set3") +
  # Legend at bottom
  theme(legend.position = "bottom") +
  facet_wrap(~Source) +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  labs(title = "",
       x = "Algorithm",
       y = "Approximation Ratio")





# Function Evaluations to achieve 90% of best algorithm -------------------


# Compare all algorithms across all instance Sources
df %>% 
  select(Source = params.instance_class, contains("algo_")) %>% 
  # Rename columns wit `metrics.QAOA_` with just their algorithm name
  gather(-Source, key = "algo", value = "value") %>%
  # Clean up algorithm name
  mutate(algo = str_remove(algo, "metrics.algo_")) %>%
  # Add latex in label for better formatting `Achieved (1 - \epsilon)`
  mutate(penalty = ifelse(value == 1e5, "Assigned Penalty", "Achieved (1 - \\epsilon)")) %>%
  count(algo, penalty) %>% 
  drop_na() %>%
  # Make percentage within each algo
  group_by(algo) %>%
  filter(penalty == "Assigned Penalty") %>%
  ggplot(aes(x = reorder(algo,n), y = n)) +
  geom_col() + 
  geom_hline(yintercept = nrow(df), linetype = "dashed") +
  # Add label for the dashed line (total runs)
  annotate("text", x = 12, y = nrow(df) + 800, label = glue("Total Runs = {nrow(df)}"), size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_Publication() + 
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  labs(title = "# of runs that are allocated penalty",
       x = "Algorithm",
       y = "# runs") + 
  coord_flip()

# Compare all algorithms across all instance Sources
df %>% 
  select(Source = params.instance_class, contains("algo_")) %>% 
  # Rename columns wit `metrics.QAOA_` with just their algorithm name
  gather(-Source, key = "algo", value = "value") %>%
  # Clean up algorithm name
  mutate(algo = str_remove(algo, "metrics.algo_")) %>%
  # Remove hyphens and title case Source
  mutate(Source = str_to_title(str_replace_all(Source, "_", " "))) %>%
  # Add latex in label for better formatting `Achieved (1 - \epsilon)`
  mutate(penalty = ifelse(value == 1e5, "Assigned Penalty", "Achieved (1 - \\epsilon)")) %>%
  count(Source, algo, penalty) %>% 
  drop_na() %>%
  # Make percentage within each algo
  group_by(Source, algo) %>%
  filter(penalty == "Assigned Penalty") %>%
  ggplot(aes(x = reorder(algo,n), y = n)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~Source) +
  theme_Publication() + 
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  labs(title = "# of runs that are allocated penalty",
       x = "Algorithm",
       y = "# runs") + 
  coord_flip()


# Algorithm Performance when they achieve 90% of best (excluding penalty) ------

# Compare all algorithms across all instance Sources
df %>% 
  select(Source = params.instance_class, contains("algo_")) %>% 
  # Rename columns wit `metrics.QAOA_` with just their algorithm name
  gather(-Source, key = "algo", value = "value") %>%
  filter(value != 1e5) %>% 
  mutate(algo = str_remove(algo, "metrics.algo_")) %>%
  mutate(algo_to_remove = ifelse(algo %in% algos_to_select, "Keep", "Remove")) %>%
  ggplot(aes(x = algo, y = value, fill=algo_to_remove)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Function Evaluations to achieve 90% of best algorithm",
       x = "Algorithm",
       y = "Function Evaluations") +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))



# Compare all algorithms across by instance Sources
df %>% 
  select(Source = params.instance_class, contains("algo_")) %>% 
  # Rename columns wit `metrics.QAOA_` with just their algorithm name
  gather(-Source, key = "algo", value = "value") %>%
  filter(value != 1e5) %>% 
  mutate(algo = str_remove(algo, "metrics.algo_")) %>%
  mutate(Source = str_to_title(str_replace_all(Source, "_", " "))) %>%
  ggplot(aes(x = algo, y = value, fill = Source)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(title = "Function Evaluations to achieve 90% of best algorithm",
       x = "Algorithm",
       y = "Function Evaluations")


# Algorithm Performance including penalty (log) --------------------------------


# Compare all algorithms across all instance Sources
df %>% 
  select(Source = params.instance_class, contains("algo_")) %>% 
  # Rename columns wit `metrics.QAOA_` with just their algorithm name
  gather(-Source, key = "algo", value = "value") %>%
  mutate(algo = str_remove(algo, "metrics.algo_")) %>%
  mutate(algo_to_remove = ifelse(algo %in% algos_to_select, "Keep", "Remove")) %>%
  ggplot(aes(x = algo, y = log10(value), fill=algo_to_remove)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Function Evaluations to achieve 90% of best algorithm",
       x = "Algorithm",
       y = "log(Function Evaluations)") +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))



# Compare all algorithms across by instance Sources
df %>% 
  select(Source = params.instance_class, contains("algo_")) %>% 
  # Rename columns wit `metrics.QAOA_` with just their algorithm name
  gather(-Source, key = "algo", value = "value") %>%
  filter(!(algo %in% c("COBYLA", "GradientDescent", "TNC"))) %>% 
  mutate(algo = str_remove(algo, "metrics.algo_")) %>%
  mutate(Source = str_to_title(str_replace_all(Source, "_", " "))) %>%
  ggplot(aes(x = algo, y = log10(value), fill = Source)) +
  geom_boxplot(outlier.size = 0.1) +
  scale_fill_brewer(palette = "Set3") +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(title = "Function Evaluations to achieve 90% of best algorithm",
       x = "Algorithm",
       y = "log(Function Evaluations)") + 
  coord_flip()


# Algorithm Performance including penalty --------------------------------

# Compare all algorithms across all instance Sources
df %>% 
  select(Source = params.instance_class, contains("algo_")) %>% 
  # Rename columns wit `metrics.QAOA_` with just their algorithm name
  gather(-Source, key = "algo", value = "value") %>%
  mutate(algo = str_remove(algo, "metrics.algo_")) %>%
  mutate(algo_to_remove = ifelse(algo %in% algos_to_select, "Keep", "Remove")) %>%
  filter(!(algo %in% c("COBYLA", "GradientDescent", "TNC"))) %>% 
  # filter out value > 100k
  # filter(value < 1e5) %>%
  ggplot(aes(x = algo, y = value, fill=algo_to_remove)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Function Evaluations to achieve 90% of best algorithm",
       x = "Algorithm",
       y = "Function Evaluations") +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))



# Compare all algorithms across by instance Sources
df %>% 
  select(Source = params.instance_class, contains("algo_")) %>% 
  # Rename columns wit `metrics.QAOA_` with just their algorithm name
  gather(-Source, key = "algo", value = "value") %>%

  mutate(algo = str_remove(algo, "metrics.algo_")) %>%
  mutate(Source = str_to_title(str_replace_all(Source, "_", " "))) %>%
  ggplot(aes(x = algo, y = value, fill = Source)) +
  geom_boxplot(outlier.size = 0.1) +
  scale_fill_brewer(palette = "Set3") +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(title = "Function Evaluations to achieve 90% of best algorithm",
       x = "Algorithm",
       y = "log(Function Evaluations)") 


d_matilda %>% 
  select(starts_with("feat")) %>% 
  # Plot each column distribution and facet
  gather(var, n) %>%
  ggplot(aes(x = n)) +
  geom_histogram(bins = 30, color = "white") +
  facet_wrap(~var,scales = "free") +
  theme_minimal()
  
  
  
  
