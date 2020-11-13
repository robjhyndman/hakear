#' Title
#'
#' @param .data
#' @param gran_x
#' @param gran_y
#' @param response
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(gravitas)
#' library(parallel)
#'sm <- smart_meter10 %>%
#'filter(customer_id %in% c("10017936"))
#'gran_x = "week_month"
#'gran_facet = "week_fortnight"
#'v = compute_mmpd(sm, gran_x, gran_facet,
#'response = general_supply_kwh)
#'#month of the year not working in this setup
compute_mmpd <- function(.data,
                        gran_x = NULL,
                        gran_facet = NULL,
                        response = NULL,
                        quantile_prob =
                        seq(0.01, 0.99, 0.01),
                        dist_ordered = TRUE)
{
 .data %>%
    create_gran(gran_x) %>%
    create_gran(gran_facet) %>%
    as_tibble() %>%
    select(!!gran_x, !!gran_facet, {{response}}) %>%
    rename("id_facet" = !!gran_facet) %>%
    rename("id_x" = !!gran_x) %>%
    rename("sim_data" = {{response}}) %>%
        compute_quantiles(
        quantile_prob =
            quantile_prob
    ) %>%
        distance_panel(dist_ordered = dist_ordered) %>%
        tidyr::pivot_longer(cols = -1, names_to = "j") %>%
        select(id_facet, value) %>%
        group_by(id_facet) %>%
        summarise(
            max = max(unlist(value), na.rm = TRUE)) %>%
        summarise(mmpd_wo_norm =  round(median(max, na.rm = TRUE),3)) %>%
        pull(mmpd_wo_norm)
}

