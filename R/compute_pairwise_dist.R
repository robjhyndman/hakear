#' Title computing all within and between facet distances between quantile categories given a data
#'
#' @param .data data for which mmpd needs to be calculated
#' @param gran_x granularities mapped across x levels
#' @param gran_facet granularities mapped across facetss
#' @param response univarite response variable
#' @param quantile_prob probabilities
#' @param dist_ordered if categories are ordered
#' @return the raw weighted pairwise  within-facet and between-facet distances
#'
#' @examples
#' library(tidyverse)
#' library(gravitas)
#' library(parallel)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10017936"))
#' gran_x <- "week_month"
#' gran_facet <- "week_fortnight"
#' v <- compute_pairwise_dist(sm, gran_x, gran_facet,
#'   response = general_supply_kwh
#' )
#' # month of the year not working in this setup
#' @export compute_pairwise_dist
compute_pairwise_dist <- function(.data,
                                  gran_x = NULL,
                                  gran_facet = NULL,
                                  response = NULL,
                                  quantile_prob =
                                    seq(0.01, 0.99, 0.01),
                                  dist_ordered = TRUE,
                                  lambda = 0.67) {
  if (!((gran_x %in% names(.data) &
    (gran_facet %in% names(.data))))) {
    .data <- .data %>%
      gravitas::create_gran(gran_x) %>%
      cgravitas::reate_gran(gran_facet)
  }

  all_dist_data <- suppressMessages(
    .data %>%
      tibble::as_tibble() %>%
      dplyr::select(!!gran_x, !!gran_facet, {{ response }}) %>%
      dplyr::rename("id_facet" = !!gran_facet) %>%
      dplyr::rename("id_x" = !!gran_x) %>%
      dplyr::rename("sim_data" = {{ response }}) %>%
      # mutate(sim_data = scale(sim_data)) %>%
      compute_quantiles(
        quantile_prob =
          quantile_prob
      ) %>%
      distance_all_pairwise(
        quantile_prob =
          quantile_prob,
        dist_ordered = dist_ordered,
        lambda = lambda
      )
  )

  all_dist_data
}
