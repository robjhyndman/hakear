#' Title
#'
#' @param .data
#' @param harmony_tbl
#' @param response
#'
#' @return
#' @export
#'
#' @examples
rank_harmony <- function(.data,
                         harmony_tbl,
                         response, nperm = 20) {

  if(nrow((harmony_tbl)==1))
  {
    panel_data = create_harmony_data(sm,
                                     harmony_tbl, response)
    value = compute_mmpd_panel(panel_data, dist_ordered = TRUE, nperm)
  }
  else
  {
    panel_data2 = create_harmony_tbl_data(sm,
                                     harmony_tbl, response)
    value = compute_mmpd_panel_grid(panel_data)
  }
}
