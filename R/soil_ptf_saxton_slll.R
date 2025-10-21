#' Saturated conductivity based on Saxton and Rawls (2006)
#'
#' @export
#'
#' @description
#' Estimates saturated conductivity based on the pedotransfer functions
#' defined in Saxton and Rawls (2006).
#'
#' @param silt silt percentage of soil (percent)
#'
#' @param clay clay percentage of soil (percent)
#'
#' @param soc soil organic carbon of soil (percent)
#'
#' @param bulk_density soil bulk density (g cm^{-3})
#'
#' @param coarse_fraction volumetric fraction of coarse  (> 2mm diameter)
#'  fragments (cm^3 cm^{-3})
#'
soil_ptf_saxton_slll <- function(silt, clay, soc, bulk_density,
                                 coarse_fraction){
  sand <- 1 - silt/100 - clay/100
  clay <- clay/100
  som <- soc*1.72
  theta_1500t = -0.024*sand + 0.487*clay + 0.006*som + 0.005*sand*som -
    0.013*clay*som + 0.068*sand*clay + 0.031
  theta_1500 = theta_1500t + 0.14*theta_1500t - 0.02
  a <- bulk_density/2.65
  Rv <- (a*coarse_fraction)/(1-coarse_fraction*(1-a))
  slll <- theta_1500*(1-Rv)
  return(slll)
}
