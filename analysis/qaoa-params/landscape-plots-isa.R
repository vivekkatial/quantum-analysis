###############################################################################
# Landscape plots 
#
# Author: Vivek Katial
# Created At: 2024-08-22 22:10:40.998194
###############################################################################

library(tidyverse)
library(jsonlite)
library(latex2exp)  # For LaTeX formatting

data <- fromJSON("data/ml-model-landscape.json")

data <- data %>% 
  filter(!(graph_type %in% c("3-Regular (no triangle)"))) %>% 
  mutate(graph_type = ifelse(graph_type == "Nearly Complete BiPartite", "Nearly Complete Bipartite", graph_type))

# Function to create a landscape plot for a single graph
create_landscape_plot <- function(graph_data, graph_type) {
  # Extract data
  beta <- graph_data$beta[[1]]  # Use the first beta list (they're all the same)
  gamma <- graph_data$gamma[[1]]  # Use the first gamma list (they're all the same)
  obj_vals <- graph_data$obj_vals[[1]]  # Use the first obj_vals matrix
  
  # Create a data frame
  df <- expand.grid(beta = beta, gamma = gamma)
  df$value <- as.vector(obj_vals)
  
  # Create the plot
  ggplot(df, aes(x = beta, y = gamma, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(guide = "none", option = "C") +  # Remove legend
    scale_x_continuous(labels = function(x) paste0(round(x/pi, 2), "π"), 
                       breaks = seq(min(beta), max(beta), length.out = 5)) +
    scale_y_continuous(labels = function(x) paste0(round(x/pi, 2), "π"), 
                       breaks = seq(min(gamma), max(gamma), length.out = 5)) +
    labs(title = paste(graph_type),
         x = TeX("$\\beta / \\pi$"), y = TeX("$\\gamma / \\pi $")) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),  # White background
      plot.background = element_rect(fill = "white", colour = NA),  # White background
      panel.grid = element_blank(),  # Remove grid lines
      axis.text = element_text(size = 14),  # Increase tick label size
      axis.title = element_text(size = 20),  # Increase axis title size
      plot.title = element_text(size = 18, hjust = 0.5),  # Center and increase title size
      axis.ticks = element_line(size = 1),  # Increase tick size
      axis.ticks.length = unit(0.2, "cm")  # Increase tick length
    )
}

# Create and save plots for all graphs in the data
for (i in seq_len(nrow(data))) {
  plot <- create_landscape_plot(data$landscape_data[i,], data$graph_type[i])
  
  # Save the plot
  ggsave(paste0("landscape_plot_", i, ".png"), plot, width = 8, height = 6, dpi = 300)
}

