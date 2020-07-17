##########################################################################
##########################################################################
##########################################################################
# Function - boot_sample
#          - Write function to perform bootstrap resampling for one or more
#            groups and calculate summary statistics for CTL values
#            across a range of sample sizes
##########################################################################
##########################################################################
##########################################################################

# Define bootstrap sampling function ---------------------------------

#' Bootstrap sampling to calculate summary statistics of CTL values
#'
#' Calculate mean and CI's of CTL for one or more groups
#' @param data Data frame contain raw data.
#' @param groups_col Factor. Column containing name(s) of population(s) of interest
#' @param n_max Numeric. Maximum sample size to extrapolate simulations.
#' @param n_min Numeric. Minimum sample size to extrapolate simulations. Defaults to 3.
#' @param iter Numeric. Number of bootstrap samples to draw. Defaults to 29.
#' @param response Numeric. Column containing thermal limit data for individual samples
#' @import purrr
#' @import tidyr
#' @import dplyr
#' @import stats
#' @import magrittr
#' @import rlang
#' @return A data frame of CTL summary statistics from bootstrap resamples
#' @examples
#' head(coreid_data)
#' sims <- boot_two_groups(data = coreid_data,
#'                         groups_col = col,
#'                         response = response,
#'                         n_max = 49,
#'                         iter = 99)
#' @export


# Define function to perform bootstrap resampling per species

boot_sample <- function(data,
                        groups_col,
                        n_max,
                        n_min = 3,
                        iter = 29,
                        response) {

  # Perform boostrap sampling
  boot_data <- {{ data }} %>%
    dplyr::group_by( {{ groups_col }} ) %>%
    tidyr::nest() %>%
    tidyr::crossing(sample_size = c({{ n_min }} : {{ n_max }}),
                    iter = seq(1: {{ iter }} )) %>%
    # Added sampling with replacement to code below
    dplyr::mutate(sample_data = purrr::map2(data,
                                            sample_size,
                                            ~dplyr::sample_n(.x, .y,
                                                             # Sample with replacement
                                                             replace = TRUE))) %>%
    dplyr::mutate(calc = purrr::map(sample_data,
                                    ~dplyr::summarize(.,
                                                      mean_val = mean( {{ response }} ),
                                                      sd_val = stats::sd(( {{ response }} ))))) %>%
    dplyr::select({{ groups_col }}, sample_size, iter, calc) %>%
    tidyr::unnest(cols = calc)

  # Estimate population CT value per species
  median_vals <- boot_data %>%
    dplyr::group_by({{ groups_col }}) %>%
    dplyr::filter(sample_size == max(sample_size)) %>%
    dplyr::mutate(median_pop_val = mean_val) %>%
    dplyr::slice(1) %>%
    dplyr::select({{ groups_col }}, median_pop_val)
  median_vals

  # Add median value per species to the bootstrapped dataset
  # Join the two datsets by the {{ groups_col }} column
  boot_proc <- dplyr::full_join(boot_data,
                                median_vals)
  boot_proc

  # Add CI's to raw bootstrap samples
  boot_comb <- boot_proc %>%
    dplyr::group_by({{ groups_col }}) %>%
    # Add standard errors
    dplyr::mutate(std_error = sd_val/sqrt(sample_size)) %>%
    # Now find error
    dplyr::mutate(error = stats::qt(0.975, df = sample_size - 1) * std_error) %>%
    # Calculate lower and upper 95% CI limits
    dplyr::mutate(lower_ci = mean_val - error,
                  upper_ci  = mean_val + error) %>%
    dplyr::ungroup()

  # Return this dataframe
  return(boot_comb)

}


##########################################################################
##########################################################################
##########################################################################
