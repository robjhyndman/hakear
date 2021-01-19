#' Title Compute raw pairwise distances
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
#' v <- compute_wpd_norm(sm, gran_x, gran_facet,
#'   response = general_supply_kwh, lambda = 0.8
#' )
#' # month of the year not working in this setup
#' @export compute_wpd_norm
compute_wpd_norm <- function(.data,
                             gran_x = NULL,
                             gran_facet = NULL,
                             response = NULL,
                             quantile_prob =
                               seq(0.01, 0.99, 0.01),
                             dist_ordered = TRUE,
                             lambda = 0.67) {
  dist_data <- compute_wpd_dist(
    .data,
    gran_x,
    gran_facet,
    {{ response }},
    quantile_prob,
    dist_ordered,
    lambda
  )

  len_dist <- nrow(dist_data)
  # normalising by number of distances
  (max(dist_data$trans_value,
    na.rm = TRUE
  ) / log(len_dist,
    base = exp(1)
  )) %>%
    round(digits = 3)
}
