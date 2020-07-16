##########################################################################
##########################################################################
##########################################################################
# Function - plot_two_groups
#          - Write function to plot output from bootstrap resampling for two
#            groups comparisons across a range of sample sizes
##########################################################################
##########################################################################
##########################################################################

# Define plotting function ---------------------------------

#' Plot output from boot_two_groups
#'
#' Plot output from boot_two_groups.
#' @param x Output from boot_two_groups function. Defaults to 'sims'.
#' @param n_max Numeric. Maximum sample size to extrapolate simulations.
#' @param n_min Numeric. Minimum sample size to extrapolate simulations.
#' Defaults to 3.
#' @import ggplot2
#' @import dplyr
#' @import stats
#' @import magrittr
#' @import rlang
#' @export
#' @examples
#' sims <- boot_two_groups(data = coreid_data,
#'                         groups_col = col,
#'                         response = response,
#'                         group1 = "Catorhintha schaffneri_APM",
#'                         group2 = "Catorhintha schaffneri_NPM",
#'                         n_max = 49,
#'                         iter = 99)
#' plots <- plot_two_groups(x = sims,
#'                          n_min = 3,
#'                          n_max = 49)
#'


plot_two_groups <- function(x = sims,
                            n_min = 3,
                            n_max){

  # Create dataframe for experimental data
  exp_data <- {{ x }} %>%
    dplyr::filter(between(sample_size, {{ n_min }}, {{ n_max }}))

  # Create dataframe for extrapolations from data
  ext_data <- {{ x }} %>%
    dplyr::filter(between(sample_size, {{ n_max }}, max(sample_size)))

  # Make a combined datafram with id included to colour-code ribbon
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
         subtitle = "(a)") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(colour = "black"),
          axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
          legend.position = "none")

  # Plot the 95% CI of mean difference

  ci_plot <- ggplot2::ggplot(data = {{ x }}, aes(x = sample_size,
                                        y = mean_diff)) +
    geom_line(data = {{ x }}, aes(x = sample_size,
                                  y = mean_low_ci),
              linetype = "dashed") +
    geom_line(data = {{ x }}, aes(x = sample_size,
                                  y = mean_upp_ci),
              linetype = "dashed") +
    geom_ribbon(data = both_data, aes(ymin = mean_low_ci,
                                      ymax = mean_upp_ci,
                                      fill = id),
                linetype = 3,
                alpha = 0.2) +
    scale_fill_manual(values = c("blue", "red"),
                      labels = c("Experimental", "Extrapolation")) +
    geom_point(data = both_data, aes(x = sample_size,
                                     y = mean_diff,
                                     colour = id),
               alpha = 0.8) +
    scale_colour_manual(values = c("blue", "red"),
                        labels = c("Experimental", "Extrapolation")) +
    theme_classic() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Sample size (n)",
         y = "Mean difference between groups (95% CI)",
         subtitle = "(b)",
         fill = "Data") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(colour = "black"),
          axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
          legend.position = c(0.8, 0.85),
          legend.key = element_rect(linetype = "dashed")) +
    guides(colour = "none")

  # Combine the two plots

  cowplot::plot_grid(width_plot,
                     ci_plot,
                     ncol = 2)

}

##########################################################################
##########################################################################
##########################################################################
