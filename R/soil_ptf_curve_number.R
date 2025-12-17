#' Determine curve number
#'
#' Determine soil conservation service (SCS) curve number for antecedent
#'  water condition II based on saturated conductivity
#'  with depth and the method described in Chapter 7 - "Hydrologic Soil
#'   Groups" of the NRCS Part 630 National Engineering Handbook (2009)
#'
#' @param ksat a vector or SpatRaster with saturated hydraulic conductivity
#'  values in units of cm per hour (optional if the hsg argument is provided).
#'
#' @param depth a vector or SpatRaster with dimensions corresponding to ksat
#'  with the depth to the base of soil layer (optional if the hsg argument is
#'  provided).
#'
#' @param slope a vector or SpatRaster with slope values in units of percent
#'
#' @param hsg an optional vector or SpatRaster of hydrologic soil group values
#'  (unitless). If not provided the hsg will be determined based on the values
#'  provided by ksat and depth.
#'
#' @export
#'
soil_ptf_curve_number <- function(ksat, depth, slope, hsg){

  if(missing(hsg)){
    hsg <- soil_ptf_nrcs_hsg(ksat, depth)
  }

  curve_number <- hsg
  curve_number[slope >= 0 & slope <= 2 & hsg == 1] <- 61
  curve_number[slope > 2 & slope <= 5 & hsg == 1] <- 64
  curve_number[slope > 5 & slope <= 10 & hsg == 1] <- 68
  curve_number[slope > 10 & hsg == 1] <- 71
  curve_number[slope > 0 & slope <= 2 & hsg == 2] <- 73
  curve_number[slope > 2 & slope <= 5 & hsg == 2] <- 76
  curve_number[slope > 5 & slope <= 10 & hsg == 2] <- 80
  curve_number[slope > 10 & hsg == 2] <- 83
  curve_number[slope >= 0 & slope <= 2 & hsg == 3] <- 81
  curve_number[slope > 2 & slope <= 5 & hsg == 3] <- 84
  curve_number[slope > 5 & slope <= 10 & hsg == 3] <- 88
  curve_number[slope > 10 & hsg == 3] <- 91
  curve_number[slope >= 0 & slope <= 2 & hsg == 4] <- 84
  curve_number[slope > 2 & slope <= 5 & hsg == 4] <- 87
  curve_number[slope > 5 & slope <= 10 & hsg == 4] <- 91
  curve_number[slope > 10 & hsg == 4] <- 94

  return(curve_number)
}
