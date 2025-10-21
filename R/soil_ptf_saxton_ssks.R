#' Saturated conductivity based on Saxton and Rawls (2006)
#'
#' @export
#'
#' @description
#' Estimates saturated conductivity based on the pedotransfer functions
#' defined in Saxton and Rawls (2006).
#'
#' @param theta_s volumetric water at saturation (cm^3 cm^{-3})
#'
#' @param theta_33 volumetric water at -33 kPa (cm^3 cm^{-3})
#'
#' @param theta_1500 volumetric water at -1500 kPa (cm^3 cm^{-3})
#'
#' @param coarse_fraction volumetric fraction of coarse  (> 2mm diameter)
#'  fragments (cm^3 cm^{-3})
#'
#' @param bulk_density soil bulk density (g cm^{-3})
#'
soil_ptf_saxton_ssks <- function(theta_s, theta_33, theta_1500,
                                 coarse_fraction, bulk_density){
  lambda <- (log(theta_33)-log(theta_1500))/(log(1500)-log(33))
  Ks <- 1930*(theta_s-theta_33)^(3-lambda)
  Kb <- Ks*(1-coarse_fraction)/(1-coarse_fraction*(1-3*(bulk_density/2.65)/2))/10
  return(Kb)
}
