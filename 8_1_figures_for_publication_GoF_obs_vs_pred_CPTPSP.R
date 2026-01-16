


library(ggplot2)
library(arrow)

data_scatterPlot_obs_vs_pred <- readRDS("datasets/data_scatterPlot_obs_vs_pred.RDS")




# Function to create the plot with customizable axis ranges
create_log_scatter_plot_test <- function(
    data,  # Dataset
    x_min = NULL,  # Minimum x-value (optional)
    x_max = NULL,  # Maximum x-value (optional)
    y_min = NULL,  # Minimum y-value (optional)
    y_max = NULL,  # Maximum y-value (optional),
    title = "PINN-1cmp"  # Plot title (optional)
) {
  # Color palette for color blindness
  color_palette <- c("#0072B2", "#009E73", "#E69F00")

  # Default values if no explicit limits are provided
  if(is.null(x_min)) x_min <- min(data$c1_est_log)
  if(is.null(x_max)) x_max <- max(data$c1_est_log)
  if(is.null(y_min)) y_min <- min(data$conc_Prediction_log)
  if(is.null(y_max)) y_max <- max(data$conc_Prediction_log)

  # Create plot with publication design
  p <-   ggplot(data = data, aes(x = abs(prediction) , y = observation, color = group, shape = group, alpha = I(alpha))) +
    facet_wrap(~ Approach, ncol = 5) +
    # Diagonal line
    geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.5, linetype = "solid") +
    # different styles "Training","Validation", "Test"
    scale_shape_manual(values = c("Training" = 15, "Validation" = 17, "Test" = 16)) +
    # Parallel dashed lines in log scale with geom_abline()
    geom_abline(
      intercept = log10(3),
      slope = 1,
      color = "gray",
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    geom_abline(
      intercept = log10(1/3),
      slope = 1,
      color = "gray",
      linewidth = 0.5,
      linetype = "dashed"
    ) +

    # Scatter plot with transparency
    geom_point() +

    # Log scale for x and y with manually set limits
    scale_x_log10(
      limits = c(x_min, x_max),
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    scale_y_log10(
      limits = c(y_min, y_max),
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +

    # Color palette for color blindness
    scale_color_manual(values = color_palette) +

    # Axis labels and title
    labs(
      x = expression("Predicted plasma conc. [nM]"),
      y = expression("Observed plasma conc. [nM]"),
      color="Dataset",
      shape="Dataset",
      alpha="Dataset"
    ) +

    # Legend only for data points
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)),
           alpha = "none") +

    # Publication-friendly theme
    theme_pubr() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      axis.title = element_text(face = "italic", size = 10),
      axis.text = element_text(size = 10, face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 6),
      legend.position = "right",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin=grid::unit(c(0,0,0,0), "mm")
    ) +
    coord_fixed(ratio = 1)

  return(p)
}




# Create plot with manually set axis limits and title
p <- create_log_scatter_plot_test(
  data_scatterPlot_obs_vs_pred,
  x_min = 10^(-5),   # Minimum x-value
  x_max = 10e5,    # Maximum x-value
  y_min = 10^(-5),   # Minimum y-value
  y_max = 10e5,     # Maximum y-value
  title = title     # Plot title
)




# Save plot
ggsave( "publication_scatter_plot_Models_together_unifiedAxis.tiff" ,
        plot = p,
        device = "tiff",
        #width = 7,
        #height = 5,
        #units = "in",
        #width = 17,
        #height = 12,
        #units = "cm",
        dpi = 300)#,
#compression = "lzw")
