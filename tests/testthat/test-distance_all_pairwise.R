set.seed(117117)
sim_panel_data <- sim_panel(
  nx = 3,
  nfacet = 2,
  ntimes = 10,
  sim_dist = distributional
  ::dist_normal(5, 10)
) %>%
  unnest(c(data))


sim_panel_quantiles <-
  compute_quantiles(sim_panel_data,
    quantile_prob = c(0.5, 0.75)
  )

dist_data <- distance_all_pairwise(sim_panel_quantiles, quantile_prob = c(0.5, 0.75))


test_that("length of the dist dataet is less than nx*nfacet combination 2", {
  expect_lte(nrow(dist_data), 15)
})
