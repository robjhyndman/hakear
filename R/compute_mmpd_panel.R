#' @title compute normalised mmpd for a panel
#'
#' @param quantile_prob
#' @param dist_ordered
#' @param .data
#' @param nperm
#'
#' @return
#' @author Sayani07s
#' @example
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c("10017936"))
#' harmonies <- sm %>%
#'  harmony(
#'    ugran = "month",
#'    filter_in = "wknd_wday",
#'    filter_out = c("hhour", "fortnight")
#'  )
#' panel_data = create_harmony_data(sm,
#' harmonies[3,], general_supply_kwh)
#' compute_mmpd_panel(panel_data,
#'                             quantile_prob =
#'                             seq(0.01, 0.99, 0.01),
#'                             dist_ordered = TRUE,
#'                             nperm = 20)

#' @export compute_mmpd_panel
compute_mmpd_panel <- function(.data,
                               quantile_prob =
                                 seq(0.01, 0.99, 0.01),
                               dist_ordered = TRUE,
                               nperm = 20) {
  dist_data_panel <- .data %>%
    compute_quantiles(
      quantile_prob =
        quantile_prob
    ) %>%
    distance_panel(dist_ordered = dist_ordered)

  #
  # mpd_data <- mpd(.data, dist_data_panel, nperm)
  # mmpd(.data, mpd_data, nperm)

  # new normalisation trick to take perm only once
  mpd2(.data, dist_data_panel, nperm)
}
