#' computes wpd for one harmony or harmony table
#' chooses compute_pairwise_max for smaller levels (<=5)
#' chooses compute_pairwise_norm for higher levels (>5)
#'
#' @param .data
#' @param harmony_tbl
#' @param response
#' @examples
#' library(gravitas)
#' library(parallel)
#' library(dplyr)
#' library(tidyr)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c("10017936"))
#'harmonies <- sm %>%
#' harmony(
#'   ugran = "month",
#'   filter_in = "wknd_wday",
#'   filter_out = c("hhour", "fortnight")
#' )
#' all_harmony <- wpd(sm,
#'   harmony_tbl = harmonies[13,],
#'   response = general_supply_kwh
#' )


wpd <- function(.data,
                harmony_tbl = NULL,
                response = NULL,
                quantile_prob = seq(0.01, 0.99, 0.01),
                dist_ordered = TRUE,
                lambda = 0.67,
                nperm = 20,
                use_perm = TRUE) {


  # one row or all harmonies of the harmony table

  if(nrow(harmony_tbl)!=1)
  {
    harmony_data <- create_harmony_tbl_data(.data,
                                            harmony_tbl =  harmony_tbl,
                                            response = {{response}})
}
  else{
    harmony_data <- create_harmony_data(.data,
                                        harmony_tbl_row = harmony_tbl,
                                        {{response}}) %>% list()
  }

  harmony_tbl_lev <-   harmony_tbl %>%
    mutate(lev = if_else(facet_levels <= 5 & x_levels <= 5,"low", "high"))


  if(!use_perm){
    lapply(harmony_data,
           function(x)
             compute_pairwise_norm_scalar(
               x,
               gran_x = "id_x",
               gran_facet = "id_facet",
               response = sim_data,
               quantile_prob,
               dist_ordered,
               lambda)
    )
  }

  else{
    mclapply(seq_len(nrow(harmony_tbl_lev)),
             function(x){
               if(harmony_tbl_lev[x, ]$lev == "high"){
                 compute_pairwise_norm_scalar(
                   h %>% extract2(x),
                   gran_x = "id_x",
                   gran_facet = "id_facet",
                   response = sim_data,
                   quantile_prob,
                   dist_ordered,
                   lambda)
               }
               else {
                 value = compute_pairwise_norm(
                   h %>% extract2(x),
                   gran_x = "id_x",
                   gran_facet = "id_facet",
                   response = sim_data,
                   quantile_prob,
                   dist_ordered,
                   lambda,
                   nperm = 20)
               }
             })

  }
}

