# This code contains the tests for all the designs from computational as well as graphical standpoint

library(distributional)
library(tidyverse)
library(testthat)

# design varx works the way it should

sim_varx_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nx - 1, by = 1) * w), sd), nfacet)
}

set.seed(1111)
sim_panel_1111 <- sim_panel(
  nx = 3,
  nfacet = 2,
  ntimes = 4,
  sim_dist = sim_varx_normal(3, 2, 0, 1, w = 3)
) %>%
  tidyr::unnest(data)
set.seed(9999)
sim_panel_9999 <- sim_panel(
  nx = 3,
  nfacet = 2,
  ntimes = 4,
  sim_dist = sim_varx_normal(3, 2, 0, 1, w = 3)
) %>%
  tidyr::unnest(data)


sim_panel_1111 %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~id_facet)

x2 <- sim_panel_1111 %>% dplyr::filter(id_facet == 1, id_x == 2)
x3 <- sim_panel_1111 %>% dplyr::filter(id_facet == 1, id_x == 3)
f2 <- sim_panel_1111 %>% dplyr::filter(id_facet == 2, id_x == 2)


test_that("design varx is working such that within-facet distances close to w", {
  expect_gte(mean(x3$sim_data) - mean(x2$sim_data), 3)
})

test_that("design varx is working such that between-facet distances close to 0", {
  expect_lte(mean(f2$sim_data) - mean(x2$sim_data), 0)
})

test_that("length of the dataset is nx*nfacet*ntimes", {
  expect_equal(nrow(sim_panel_1111), 24)
})


test_that("different seeds produce different observations", {
  expect_equal(
    sim_panel_1111$sim_data - sim_panel_9999$sim_data,
    c(
      -1.17067923, 0.47941557, 0.14531299, 1.94780263, -2.78752574,
      -3.83973030, 1.64058471, 1.30606069, -0.03632688, 0.29750263,
      -0.52653933, -0.08082995, -1.11771041, -1.38786226, 0.47229198,
      1.16244362, 0.09872577, -3.29538290, 0.41125692, -1.99185321,
      0.99158768, -0.23084793, -0.87478004, -0.43136713
    )
  )
})


# design varf works the way it should

sim_varf_normal <- function(nx, nfacet, mean, sd, w) {
  rep(dist_normal((mean + seq(0, nfacet - 1, by = 1) * w), sd), each = nx)
}

set.seed(1111)
simf_1111 <- sim_panel(
  nx = 3,
  nfacet = 2,
  ntimes = 4,
  sim_dist = sim_varf_normal(3, 2, 0, 1, w = 3)
) %>%
  tidyr::unnest(data)
set.seed(9999)
simf_9999 <- sim_panel(
  nx = 3,
  nfacet = 2,
  ntimes = 4,
  sim_dist = sim_varf_normal(3, 2, 0, 1, w = 3)
) %>%
  tidyr::unnest(data)


simf_1111 %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~id_facet)

x2 <- simf_1111 %>% dplyr::filter(id_facet == 1, id_x == 2)
x3 <- simf_1111 %>% dplyr::filter(id_facet == 1, id_x == 3)
f2 <- simf_1111 %>% dplyr::filter(id_facet == 2, id_x == 2)
f3 <- simf_1111 %>% dplyr::filter(id_facet == 1, id_x == 3)


test_that("design varf is working such that within-facet distances close to 0", {
  expect_gte(mean(x3$sim_data) - mean(x2$sim_data), 0)
  expect_lte(mean(x3$sim_data) - mean(x2$sim_data), 1)
})

test_that("design varf is working such that between-facet distances close to w", {
  expect_lte(mean(f2$sim_data) - mean(x2$sim_data), 3)
  expect_gte(mean(f2$sim_data) - mean(x2$sim_data), 2)
})

test_that("length of the dataset is nx*nfacet*ntimes", {
  expect_equal(nrow(simf_1111), 24)
})


test_that("different seeds produce different observations", {
  expect_equal(
    simf_1111$sim_data - simf_9999$sim_data,
    c(
      -1.17067923, 0.47941557, 0.14531299, 1.94780263, -2.78752574,
      -3.83973030, 1.64058471, 1.30606069, -0.03632688, 0.29750263,
      -0.52653933, -0.08082995, -1.11771041, -1.38786226, 0.47229198,
      1.16244362, 0.09872577, -3.29538290, 0.41125692, -1.99185321,
      0.99158768, -0.23084793, -0.87478004, -0.43136713
    )
  )
})

test_that("how varx and varf behave relative to each other", {
  expect_equal(
    -simf_1111$sim_data + sim_panel_1111$sim_data,
    c(0, 0, 0, 0, 3, 3, 3, 3, 6, 6, 6, 6, -3, -3, -3, -3, 0, 0, 0, 0, 3, 3, 3, 3)
  )
})

# simf_1111 %>% select(1:4) %>%
#   dplyr::bind_cols(varf = simf_1111$sim_data,
#                                         varx = sim_panel_1111$sim_data)


# design varall works the way it should

sim_varall_normal <- function(nx, nfacet, mean, sd, w) {
  dist_normal((mean + seq(0,
    (nx *
      nfacet - 1),
    by = 1
  ) * w), sd)
}

set.seed(1111)
simv_1111 <- sim_panel(
  nx = 3,
  nfacet = 2,
  ntimes = 4,
  sim_dist = sim_varall_normal(3, 2, 0, 1, w = 3)
) %>%
  tidyr::unnest(data)
set.seed(9999)
simv_9999 <- sim_panel(
  nx = 3,
  nfacet = 2,
  ntimes = 4,
  sim_dist = sim_varall_normal(3, 2, 0, 1, w = 3)
) %>%
  tidyr::unnest(data)


simv_1111 %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(id_x), y = sim_data)) +
  facet_wrap(~id_facet)


x2 <- simv_1111 %>% dplyr::filter(id_facet == 1, id_x == 2)
x3 <- simv_1111 %>% dplyr::filter(id_facet == 1, id_x == 3)
f2 <- simv_1111 %>% dplyr::filter(id_facet == 2, id_x == 2)
f3 <- simv_1111 %>% dplyr::filter(id_facet == 1, id_x == 3)

test_that("design varf is working such that within-facet distances close to 0", {
  expect_gte(mean(x3$sim_data) - mean(x2$sim_data), 3)
})

test_that("design varf is working such that between-facet distances close to w", {
  expect_gte(mean(f2$sim_data) - mean(x2$sim_data), 8.5)
})
