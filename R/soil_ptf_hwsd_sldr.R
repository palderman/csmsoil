#' Determine soil drainage based on WRB number
#'
#' Determine soil drainage parameter (SLDR) based on the drainage
#'  class as defined in the Harmonized World Soil Database (HWSD)
#'  for a given world resources based (WRB) number
#'
#' @param wrb_number the world resources based (WRB) number for soil types
#'
#' @export
#'
soil_ptf_hwsd_sldr <- function(wrb_number){
  # Create output dataset with same type and dimensions:
  sldr_out <- wrb_number

  sldr_out[] <-
    data.frame(wrb_number = wrb_number[]) |>
    # wrb_table stored as internal data:
    within({
      wrb_row_index = ifelse(wrb_number == 255, NA_integer_, wrb_number)
      wrb_row_index = wrb_row_index + 1
    }) |>
    with({
      wrb_sldr_table[wrb_row_index, "SLDR"]
    })

  sldr_out
}
