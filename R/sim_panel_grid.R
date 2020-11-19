#' @title sim panel grid
#' @param range_nx
#' @param range_nfacet
#' @param ntimes
#' @param sim_dist
#' @return
#' @author Sayani Gupta
#' @examples
#' library(distributional)
#' library(purrr)
#' library(tibble)
#' library(dplyr)
#' library(tidyr)
#' sim_null_normal <-
#'   function(nx, nfacet) {
#'     rep(distributional::dist_normal(
#'       5,
#'       10
#'     ), nx * nfacet)
#'   }
#' sim_panel_grid_data <-
#'   sim_panel_grid(
#'     range_nx = 2:3,
#'     range_nfacet = 3:4,
#'     ntimes = 50,
#'     sim_dist = sim_null_normal
#'   ) %>%
#'   unnest(c(data))
#' sim_grid_split <- sim_panel_grid_data %>%
#'   group_by(nx, nfacet) %>%
#'   group_split()
#'   @export sim_panel_grid
sim_panel_grid <- function(range_nx = 2:7, range_nfacet = 2:7, ntimes = 5000, sim_dist = sim_null_dist1) {
  (range_nx) %>%
    map_df(function(i) {
      (range_nfacet) %>%
        map_df(function(j) {
          sim_panel(nx = i, nfacet = j, ntimes = ntimes, sim_dist = sim_dist)
        })
    })
}
