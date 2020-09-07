##########################################################################
##########################################################################
##########################################################################
# Function - plot_one_group
#          - Write function to plot output from bootstrap resampling for
#            single population estimates across a range of sample sizes
##########################################################################
##########################################################################
##########################################################################

# Define plotting function ---------------------------------

#' Plot output from boot_sample
#'
#' Plot output from boot_sample.
#' @param x Output from boot_sample function. Defaults to 'sims'.
#' @param n_max Numeric. Maximum sample size to extrapolate simulations.
#' @param n_min Numeric. Minimum sample size to extrapolate simulations.
#' Defaults to 3.
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @importFrom dplyr bind_rows
#' @importFrom dplyr between
#' @export
#' @examples
#' sims <- boot_sample(coreid_data,
#'                     groups_col = col,
#'                     groups_which = "Catorhintha schaffneri_APM",
#'                     n_max = 30,
#'                     response = response)
#' plot_one_group(x = sims,
#'                n_min = 3,
#'                n_max = 15)
#'


plot_one_group <- function(x = sims,
                           n_min = 3,
                           n_max){

  # Create dataframe for experimental data
  exp_data <- {{ x }} %>%
    dplyr::filter(dplyr::between(sample_size, {{ n_min }}, {{ n_max }}))

  # Create dataframe for extrapolations from data
  ext_data <- {{ x }} %>%
    dplyr::filter(dplyr::between(sample_size, {{ n_max }}, max(sample_size)))

  # Make a combined dataframe with id included to colour-code ribbon
  both_data <- dplyr::bind_rows(exp_data, ext_data, .id = "id")

  # Plot the width of the 95% CI
  width_plot <- ggplot2::ggplot(data = {{ x }}, aes(x = sample_size,
                                                    y = width_ci)) +
    geom_line(data = both_data, aes(x = sample_size,
                                    y = width_ci,
                                    colour = id),
              alpha = 0.8) +
    scale_colour_manual(values = c("blue", "red"),
                        labels = c("Experimental", "Extrapolation")) +
    geom_ribbon(data = both_data, aes(ymin = sd_width_lower,
                                      ymax = sd_width_upper,
                                      fill = id),
                linetype = 3,
                alpha = 0.2) +
    scale_fill_manual(values = c("blue", "red"),
                      labels = c("Experimental", "Extrapolation")) +
    theme_classic() +
    geom_hline(yintercept = 0,
               linetype = "dashed") +
    labs(x = "Sample size (n)",
         y = "Width of confidence interval (95% CI)",
         fill = "Data",
         subtitle = "(a)") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(colour = "black"),
          axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
          legend.position = "right") +
    guides(colour = FALSE)

  # Plot the width of the 95% CI
  contain_plot <- ggplot2::ggplot(data = {{ x }}, aes(x = sample_size,
                                                      y = prop_ci_contain)) +
    geom_line(data = both_data, aes(x = sample_size,
                                    y = prop_ci_contain,
                                    colour = id),
              alpha = 0.8) +
    scale_colour_manual(values = c("blue", "red"),
                        labels = c("Experimental", "Extrapolation")) +
    theme_classic() +
    geom_hline(yintercept = 0.90,
               linetype = "dashed") +
    labs(x = "Sample size (n)",
         y = "Proportion of 95%'s \ncontaining median CTL",
         colour = "Data",
         subtitle = "(b)") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(colour = "black"),
          axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
          legend.position = "right")

  # Return the plots
  cowplot::plot_grid(width_plot,
                     contain_plot,
                     ncol = 2)


}

##########################################################################
##########################################################################
##########################################################################
