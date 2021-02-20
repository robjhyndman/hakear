#' @title sim panel
#' @param nx number of x categories
#' @param nfacet number of facet categories
#' @param ntimes number of observations to be simulated for each categories
#' @param sim_dist type of distribution to be simulated
#' @return
#' @author Sayani07
#' @examples
#' library(tidyverse)
#' library(distributional)
#' sim_varx_normal <- function(nx, nfacet, mean, sd, w) {
#'   rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w), sd), nfacet)
#' }
#'
#' sim_panel_data <- sim_panel(
#'   nx = 2, nfacet = 3,
#'   ntimes = 5,
#'   sim_dist = sim_varx_normal(2, 3, 0, 1, 10)
#' ) %>% unnest(data)
#'
#' sim_panel_data %>%
#'   ggplot() +
#'   geom_boxplot(aes(x = as.factor(id_x), y = sim_data)) +
#'   facet_wrap(~id_facet)
#' compute_quantiles(sim_panel_data) %>%
#'   unnest(c(sim_data_quantile)) %>%
#'   ggplot() +
#'   geom_boxplot(aes(x = as.factor(id_x), y = sim_data_quantile)) +
#'   facet_wrap(~id_facet)
#'
#'
#' sim_varf_normal <- function(nx, nfacet, mean, sd, w) {
#'   rep(dist_normal((mean + seq(0, nfacet - 1, by = 1) * w), sd), each = nx)
#' }
#' sim_panel_data <- sim_panel(
#'   nx = 2, nfacet = 3,
#'   ntimes = 5,
#'   sim_dist = sim_varf_normal(2, 3, 0, 1, 10)
#' ) %>% unnest(data)
#'
#' sim_panel_data %>%
#'   ggplot() +
#'   geom_boxplot(aes(x = as.factor(id_x), y = sim_data)) +
#'   facet_wrap(~id_facet)
#' compute_quantiles(sim_panel_data) %>%
#'   unnest(c(sim_data_quantile)) %>%
#'   ggplot() +
#'   geom_boxplot(aes(x = as.factor(id_x), y = sim_data_quantile)) +
#'   facet_wrap(~id_facet)
#'
#'
#'
#' sim_varall_normal <- function(nx, nfacet, mean, sd, w) {
#'   dist_normal((mean + seq(0,
#'     (nx *
#'       nfacet - 1),
#'     by = 1
#'   ) * w), sd)
#' }
#' sim_panel_data <- sim_panel(
#'   nx = 2, nfacet = 3,
#'   ntimes = 5,
#'   sim_dist = sim_varall_normal(2, 3, 0, 1, 10)
#' ) %>% unnest(data)
#'
#' sim_panel_data %>%
#'   ggplot() +
#'   geom_boxplot(aes(x = as.factor(id_x), y = sim_data)) +
#'   facet_wrap(~id_facet)
#' compute_quantiles(sim_panel_data) %>%
#'   unnest(c(sim_data_quantile)) %>%
#'   ggplot() +
#'   geom_boxplot(aes(x = as.factor(id_x), y = sim_data_quantile)) +
#'   facet_wrap(~id_facet)
#' compute_pairwise_max(sim_panel_data, "id_x", "id_facet",
#'   response = sim_data
#' )

sim_panel <- function(nx = 2, nfacet = 3,
                      ntimes = 500,
                      sim_dist =
                        sim_varall) {
  sim_varall <- function(nx,
                         nfacet,
                         mean = 0,
                         sd = 1,
                         w = 2) {
    dist_normal((mean + seq(0, (nx * nfacet - 1), by = 1) * w), sd)
  }


  if (typeof(sim_dist) == "list") {
    sim_dist_data <- sim_dist
  }
  else {
    sim_dist_data <- sim_dist(nx, nfacet, mean, sd, w)
  }
  id_x <- rep(seq_len(nx), nfacet)
  id_facet <- rep(seq_len(nfacet), each = nx)

  sim_data2 <- tibble(
    id_x,
    id_facet,
    sim_dist_data
  ) %>%
    group_by(
      id_facet,
      id_x
    ) %>%
    summarise(sim_data = generate(sim_dist_data,
      times = ntimes
    ), .groups = "drop") %>%
    unnest(sim_data)

  sim_data2 %>%
    mutate(nfacet = nfacet, nx = nx) %>%
    select(nfacet, nx, id_facet, id_x, sim_data) %>%
    group_by(
      nfacet, nx,
      id_facet, id_x
    ) %>%
    nest()
}
