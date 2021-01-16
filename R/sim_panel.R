#' @title sim panel
#' @param nx
#' @param nfacet
#' @param ntimes
#' @param sim_dist
#' @return
#' @author Sayani07
#' @examples
#' sim_panel_data <- sim_panel(nx = 2, nfacet = 3,
#' ntimes = 5,
#' sim_dist = sim_varx_normal(2, 3, 0, 1, 10)) %>% unnest(data)
#'
#' sim_panel_data %>%
#' ggplot() +
#' geom_boxplot(aes(x = as.factor(id_x), y = sim_data)) +
#' facet_wrap(~id_facet)
#'
#' @export

sim_panel <- function(nx = 2, nfacet = 3,
                      ntimes = 5,
                      sim_dist =
                        sim_varf_dist1) {

  sim_varf_dist1 <- function(nx, nfacet) {
    rep(dist_normal(seq(mean, mean * nfacet, by = mean), sd), each = nx)
  }

  if(typeof(sim_dist)=="list"){
    sim_dist_data <- sim_dist
  }
  else{
  sim_dist_data <- sim_dist(nx, nfacet, ...)
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
