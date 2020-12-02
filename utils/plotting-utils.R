###############################################################################
# Plotting utility functions for analysis
#
# Author: Vivek Katial
# Created 2020-05-28 01:52:58
###############################################################################

generate_feature_plot <- function(data, metric, feature){
  
  # Build general feature plot
  p_overall <- data %>% 
    select(-contains("metrics"), metric) %>% 
    ggplot(aes(x= !!as.symbol(feature), y = !!as.symbol(metric), col = params_time_t)) +
    geom_point() + 
    theme_light() + 
    labs(x = "", y = "")
  
  # Build feature plot by qubit
  p_by_qubit <- d_sampled %>% 
    select(-contains("metrics"), metric) %>% 
    ggplot(aes(x= !!as.symbol(feature), y = !!as.symbol(metric), col = params_time_t)) +
    geom_point() + 
    facet_wrap(~params_n_qubits, scales = "free") + 
    theme_light() + 
    labs(x = "", y = "")
  
  # Build grid plot
  p_row <- plot_grid(
    p_overall + theme(legend.position = "none"),
    p_by_qubit + theme(legend.position = "none"),
    labels = c("A", "B"),
    hjust = -1,
    nrow = 1
  )
  
  # Extract legend from plot
  legend <- get_legend(
    # create some space to the left of the legend
    p_overall + theme(legend.box.margin = margin(0, 0, 0, 12))
  )
  
  p_grid <- plot_grid(p_row, legend, rel_widths = c(3, .4))
  
  # Create common x and y labels
  y.grob <- textGrob(metric, 
                     gp=gpar(fontface="bold", fontsize=15), rot=90)
  
  x.grob <- textGrob(feature, 
                     gp=gpar(fontface="bold", fontsize=15))
  
  # Create final grid
  p_final <- grid.arrange(arrangeGrob(p_grid, left = y.grob, bottom = x.grob))
  
  # Output plot
  p_final
}


