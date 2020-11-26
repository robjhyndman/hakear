#' @title computing all within and between facet distances between quantile categories given quantile data
#'
#' @param sim_panel_quantiles quantile data
#' @param dist_ordered if categories are ordered
#' @param quantile_prob numeric vector of probabilities with value #'in [0,1]  whose sample quantiles are wanted. Default is set to #' "decile" plot
#'
#' @return
#' @author Sayani07
#' @export distance_all_pairwise
distance_all_pairwise <- function(sim_panel_quantiles,
                           quantile_prob = seq(0.01, 0.99, 0.01),
                           dist_ordered = TRUE,
                           lambda = 0.67) {
  # ncoly <- sim_panel_quantiles %>%
  #   distinct(id_facet) %>%
  #   nrow()
  # nrowy <- sim_panel_quantiles %>%
  #   distinct(id_x) %>%
  #   nrow()

  # range of i, j and k are defined in this way since some cyclic granularities start from 0 and others from 1 -  it was creating a problem while filtering in m1 and m2, where m2 was leading to a tibble of 0 rows and JS function was failing

  if (class(sim_panel_quantiles$id_x) %in% c("character", "integer")) {
    sim_panel_quantiles$id_x <- as.numeric(as.factor(sim_panel_quantiles$id_x)) %>% factor()
  }
  if (class(sim_panel_quantiles$id_facet) %in% c("character", "integer")) {
    sim_panel_quantiles$id_facet <- as.numeric(as.factor(sim_panel_quantiles$id_facet)) %>% factor()
  }

  vm =  sim_panel_quantiles %>% mutate(row_number = row_number())

  allcomb = combn(vm$row_number,2) %>%
    t() %>%
    as_tibble()

  all_data = allcomb %>%
    left_join(vm, by = c("V1"="row_number")) %>%
    left_join(vm, by = c("V2"="row_number")) %>%
    mutate(dist_type = if_else(id_facet.x == id_facet.y,
                               "within-facet",
                               "between-facet"))

  if(dist_ordered)
  {

    all_data <- all_data %>%
      mutate(remove_row =
               if_else(id_facet.x == id_facet.y &
                                    abs(as.numeric(id_x.y) - as.numeric(id_x.x))!=1,1,0)) %>%
      filter(remove_row==0)
  }

  all_dist = lapply(seq_len(nrow(all_data)),
         function(x){
           distance = JS(prob = quantile_prob,
                         unlist(all_data[x, ]$sim_data_quantile.x),
                         unlist(all_data[x, ]$sim_data_quantile.y))
         }) %>% unlist() %>% as_tibble()


  return_data <- all_data %>%
    rename( "id_facet_1" = "id_facet.x",
            "id_facet_2" = "id_facet.y",
            "id_x_1" = "id_x.x",
            "id_x_2" = "id_x.y") %>%
    select(id_facet_1,
           id_x_1,
           id_facet_2,
           id_x_2,
           dist_type) %>% bind_cols(all_dist) %>%
    mutate(trans_value = if_else(dist_type == "within-facet",
                            lambda*value,
                            (1/lambda)*value))

  return_data

}

JS <- function(prob, q, p) {
  # Compute approximate densities
  x <- seq(min(q, p), max(q, p), l = 201)
  qpmf <- pmf(x, prob, q)
  ppmf <- pmf(x, prob, p)
  m <- 0.5 * (ppmf + qpmf)
  JS <- suppressWarnings(0.5 * (sum(stats::na.omit(ppmf * log(ppmf / m, base = 2))) +
                                  sum(stats::na.omit(qpmf * log(qpmf / m, base = 2)))))
  return(JS)
}

# Compute approximate discretized density (like a probability mass function)
# at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q) {
  qcdf <- stats::approx(q, p, xout = x, yleft = 0, yright = 1, ties = max, na.rm = TRUE)$y
  qpmf <- c(0, diff(qcdf) / (x[2] - x[1]))
  return(qpmf / sum(qpmf))
}

