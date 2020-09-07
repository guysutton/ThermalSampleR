##########################################################################
##########################################################################
##########################################################################
# Function - calc_ci_coverage
#          - Write function to calculate how many bootstrap sample CTL
#            estimates fall within bounds of 95% confidence interval,
#            across a range of sample sizes
##########################################################################
##########################################################################
##########################################################################

# Define function ---------------------------------

#' Calculate coverage of confidence intervals
#'
#' Calculate how many bootstrap sample CTL estimates fall within bounds of 95%
#' confidence interval.
#' @param x Data frame. Output of boot_sample function.
#' @param groups_col Factor. Column containing name(s) of population(s) of interest
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom tidyr nest
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr slice
#' @importFrom dplyr summarise
#' @importFrom dplyr sample_n
#' @importFrom dplyr ungroup
#' @importFrom stats qt
#' @importFrom tidyr unnest
#' @importFrom tidyr crossing
#' @import rlang
#' @return A data frame of CTL summary statistics from bootstrap resamples
#' @examples
#' head(coreid_data)
#' # First draw bootstrap samples
#' sims <- boot_sample(data = coreid_data,
#'                     groups_col = col,
#'                     response = response,
#'                     n_max = 49,
#'                     iter = 99)
#' # Now we can calculate coverage
#' cover <- calc_ci_coverage(x= sims,
#'                           groups_col = col)
#' @export


# Define function to calculate the proportion of bootstrap samples
# that fall within the bounds of a 95% confidence interval

calc_ci_coverage <- function(x,
                             groups_col) {

  # Calculate the proportion of times the median_pop_val falls within
  # the bounds of the confidence interval
  ci_cover <- {{ x }} %>%
  dplyr::group_by({{ groups_col }}, sample_size, iter) %>%
  dplyr::mutate(ci_falls = dplyr::case_when(
    median_pop_val < lower_ci ~ 0,
    median_pop_val > upper_ci ~ 0,
    median_pop_val > lower_ci & median_pop_val < upper_ci ~ 1,
    median_pop_val < upper_ci & median_pop_val > lower_ci ~ 1)) %>%
  dplyr::group_by({{ groups_col }}, sample_size) %>%
  dplyr::mutate(prop_correct = sum(ci_falls)/max(iter)) %>%
  dplyr::slice(1)

  # Return data frame
  return(ci_cover)

}

##########################################################################
##########################################################################
##########################################################################
