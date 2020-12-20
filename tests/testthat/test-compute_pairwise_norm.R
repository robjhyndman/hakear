test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


library(tidyverse)
library(gravitas)
library(parallel)
library(ggplot2)
library(distributional)
sim_panel_data  = sim_panel(nx = 3,
                            nfacet = 4,
                            ntimes = 500,
                            sim_dist = distributional
                            ::dist_normal(5, 10)) %>%
  unnest (c(data))
sim_panel_quantiles  =
  compute_quantiles(sim_panel_data,
                    quantile_prob = seq(0.01, 0.99, 0.01))

distance_all_pairwise(sim_panel_quantiles, lambda = 0.5)


compute_pairwise_norm(sim_panel_data,
                      gran_x = "id_x",
                      gran_facet = "id_facet",
                      response = sim_data)
