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
#'  #@param density_adjustment whether to adjust drained upper limit for
#'  # soil density. If TRUE apply the adjustment, if FALSE (default) do
#'  # not apply the adjustment.
#'
soil_saxton_ssat <- function(silt, clay, soc, bulk_density,
                             coarse_fraction){
  sand <- 1 - silt/100 - clay/100
  clay <- clay/100
  som <- soc*1.72
  theta_33t <- -0.251*sand + 0.195*clay + 0.011*som + 0.006*sand*som -
    0.027*clay*som + 0.452*sand*clay + 0.299
  theta_33 <- theta_33t + 1.283*(theta_33t)^2 - 0.374*theta_33t - 0.015
  theta_S33t = 0.278*sand + 0.034*clay + 0.022*som - 0.018*sand*som -
    0.027*clay*som - 0.584*sand*clay + 0.078
  theta_S33 = theta_S33t + 0.636*theta_S33t-0.107
  theta_S <- theta_33 + theta_S33 - 0.097*sand + 0.043
  a <- bulk_density/2.65
  Rv <- (a*coarse_fraction)/(1-coarse_fraction*(1-a))
  theta_S <- theta_33 + theta_S33 - 0.097*sand + 0.043
  # The following is commented out until we can identify where the DF
  # comes from and when the adjustment would apply:
  # if(density_adjustment){
  #    bulk_density_adj <- bulk_density*DF
  #    theta_SDF <- 1-(bulk_density_adj/2.65)
  #    ssat <- theta.SDF*(1-Rv)
  # }else{
  ssat <- theta_S*(1-Rv)
  # }
  return(ssat)
}
