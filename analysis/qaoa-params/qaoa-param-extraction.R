library(tidyverse)
library(here)
library(scales)
library(gridExtra)

# Read the data
df <- read_csv(here("data/d-chap4-raw-feats.csv")) %>% 
  rename(Instance = Row)

# Function to clean feature names
clean_feature_name <- function(name) {
  name %>%
    str_replace_all("_", " ") %>%
    str_to_title()
}

# Function to create a single box plot
create_box_plot <- function(data, feature) {
  clean_name <- clean_feature_name(feature)
  ggplot(data, aes(y = .data[[feature]])) +
    geom_boxplot(fill = "lightgray", outlier.shape = 19, outlier.size = 1) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 18),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    ) +
    labs(title = clean_name)
}

# Create a list of plots
plot_list <- map(names(df)[-1], ~create_box_plot(df, .x))

# Arrange plots in a grid
grid_arranged_plots <- arrangeGrob(
  grobs = plot_list,
  ncol = 2
)

# Save the plot
ggsave("feature_boxplots.png", grid_arranged_plots, width = 8.3, height = 12, dpi = 300)

# Display the plot (optional, for interactive environments)
grid.arrange(grid_arranged_plots)