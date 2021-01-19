library(philentropy)


# JS computes Jensen-Shannon distance
# based on quantiles q and p at probabilities prob

set.seed(1111)
data1 <- rnorm(200, 0, 1)
data2 <- runif(500, -9, 9)
prob <- seq(0.01, 0.99, 0.01)
quan1 <- quantile(data1, prob)
quan2 <- quantile(data2, prob)
x <- seq(min(quan1, quan2), max(quan1, quan2), l = 201)
# pmf computes approximate discretized density (like a probability mass function) at each x (equally spaced) given quantiles q with probabilities p
qpmf <- pmf(x, prob, quan1)
ppmf <- pmf(x, prob, quan2)
data_pmf <- rbind(qpmf, ppmf) # probability vectors should add up to 1

test_that("the JS function exactly what JSD from R package philentropy does", {
  expect_equal(JS(prob, quan1, quan2), as.numeric(JSD(data_pmf)))
})
