#' Select harmonies with significant patterns
#' @param .data a tsibble or data with already computed categories
#' @param harmony_tbl A tibble containing one or more hamronies with facet_variable, x_variable, facet_levels and x_levels
#' @param dist_ordered if categories are ordered
#' @param quantile_prob numeric vector of probabilities with value #'in [0,1]  whose sample quantiles are wanted. Default is set to #' "decile" plot
#' @param lambda value of tuning parameter for computing weighted pairwise distances
#' @param nperm number of permutations for normalization
#' @param response the response variable
#' @param use_perm should permutation approach for normalization be used
#' @param nsamp number of permutation for computing the threshold
#' @examples
#' library(gravitas)
#' library(parallel)
#' library(dplyr)
#' library(tidyr)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10017936"))
#' harmonies <- sm %>%
#'   harmony(
#'     ugran = "month",
#'     filter_in = "wknd_wday",
#'     filter_out = c("hhour", "fortnight")
#'   )
#' all_harmony <- select_harmonies(sm,
#'   harmony_tbl = harmonies,
#'   response = general_supply_kwh
#' )
#'@export
select_harmonies <- function(.data,
                             harmony_tbl = NULL,
                             response = NULL,
                             quantile_prob = seq(0.01, 0.99, 0.01),
                             dist_ordered = TRUE,
                             lambda = 0.67,
                             nperm = 20,
                             use_perm = FALSE,
                             nsamp = 2) {

  select_harmony <- value <- NULL

  wpd_obs <- wpd(.data,
                 harmony_tbl,
                 {{response}},
                 quantile_prob = seq(0.01, 0.99, 0.01),
                 dist_ordered,
                 lambda,
                 nperm,
                 use_perm) %>%
    unlist() %>%
    tibble::as_tibble()



    wpd_sample <- parallel::mclapply((1:nsamp), function(x){

      response_sample <- .data %>%
        tibble::as_tibble() %>%
        dplyr::ungroup() %>%
        dplyr::select({{response}}) %>%
        dplyr::sample_frac(size = 1)


      data_sample <- .data %>%
        dplyr::select(-{{response}}) %>%
        dplyr::bind_cols(response = response_sample)


wpd(data_sample,
             harmony_tbl,
             {{response}},
             quantile_prob = seq(0.01, 0.99, 0.01),
             dist_ordered,
             lambda,
             nperm,
             use_perm)
})

    threshold_01 <- stats::quantile(unlist(wpd_sample), probs = 0.99, na.rm = TRUE)

    threshold_02 <- stats::quantile(unlist(wpd_sample), probs = 0.95, na.rm = TRUE)

    threshold_03 <- stats::quantile(unlist(wpd_sample), probs = 0.90, na.rm = TRUE)


    harmony_tbl %>%
      dplyr::bind_cols(wpd_obs) %>%
      dplyr::rename(wpd = value) %>%
      dplyr::mutate(wpd = round(wpd, 3)) %>%
      dplyr::mutate(select_harmony =
                      if_else(wpd_obs > threshold_01,
                              paste(wpd, "***", sep = " "),
                              if_else(wpd_obs > threshold_02, paste(wpd, "**", sep = " "),
                                      if_else(wpd_obs > threshold_03, paste(wpd, "*", sep = " "), as.character(wpd))))) %>%
      dplyr::arrange(-wpd)

}
