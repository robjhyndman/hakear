#' Create data for one harmony pair
#'
#' @param .data a tsibble
#' @param harmony_tbl_row a row from the harmony table
#' @param response univariate response variable
#'
#' @return a tibble with the harmony pair and response variable
#'
#' @examples
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10017936"))
#' harmonies <- sm %>%
#'   harmony(
#'     ugran = "month",
#'     filter_in = "wknd_wday",
#'     filter_out = c("hhour", "fortnight")
#'   )
#' panel_data <- create_harmony_data(
#'   sm,
#'   harmonies[3, ], general_supply_kwh
#' )
#' @export
create_harmony_data <- function(.data, harmony_tbl_row, response) {
  .data %>%
    gravitas::create_gran(harmony_tbl_row$facet_variable) %>%
    gravitas::create_gran(harmony_tbl_row$x_variable) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      id_facet = harmony_tbl_row$facet_variable,
      id_x = harmony_tbl_row$x_variable,
      sim_data = {{ response }}
    )
}
