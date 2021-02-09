#' Title Compute scalar normalised pairwise distances
#'
#' @param .data data for which mmpd needs to be calculated
#' @param gran_x granularities mapped across x levels
#' @param gran_facet granularities mapped across facetss
#' @param response univarite response variable
#' @param quantile_prob probabilities
#' @param dist_ordered if categories are ordered
#' @return
#'
#' @examples
#' library(tidyverse)
#' library(gravitas)
#' library(parallel)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10017936"))
#' gran_x <- "week_month"
#' gran_facet <- "week_fortnight"
#' v <- compute_pairwise_norm_scalar(sm, gran_x, gran_facet,
#'   response = general_supply_kwh, lambda = 0.90
#' )
#' # month of the year not working in this setup
#' @export compute_pairwise_max
compute_pairwise_norm_scalar <- function(.data,
                                 gran_x = NULL,
                                 gran_facet = NULL,
                                 response = NULL,
                                 quantile_prob =
                                   seq(0.01, 0.99, 0.01),
                                 dist_ordered = TRUE,
                                 lambda = 0.67) {
  dist_data <- compute_pairwise_dist(
    .data,
    gran_x,
    gran_facet,
    {{ response }},
    quantile_prob,
    dist_ordered,
    lambda
  )

  raw <- max(dist_data$trans_value, na.rm = TRUE)

  # fitting a log-linear model and normalising for the number of distances
  (raw - 0.0027*log(nrow(dist_data))) %>% round(digits = 3)

}
