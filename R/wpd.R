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
#'   harmony_tbl = harmonies,
#'   response = general_supply_kwh
#' )


wpd <- function(.data,
                harmony_tbl = NULL,
                response = NULL,
                quantile_prob = seq(0.01, 0.99, 0.01),
                dist_ordered = TRUE,
                lambda = 0.67) {


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
                                        {{response}})
  }

  mclapply(harmony_data, function(x){
         compute_pairwise_max(
          x,
         gran_x = "id_x",
         gran_facet = "id_facet",
         response = sim_data,
         quantile_prob,
         dist_ordered,
         lambda
         )
  }
  )
}

