#' Determine stage 1 soil evaporation limit
#'
#' Determine the stage 1 soil evaporation limit as half the difference between
#'  the saturated volumetric soil water and permanent wilting point weighted by
#'  layer thickness and summed across the first 15-cm of the soil profile.
#'
#' @param sat a vector or SpatRaster with saturated volumetric soil water in
#'  units of cm^3 cm^-3
#'
#' @param pwp a vector or SpatRaster with permanent wilting point values as
#'  volumetric soil water in units of cm^3 cm^-3
#'
#' @param depth a vector or SpatRaster with dimensions corresponding to sat and
#'  pwp with the depth to the base of soil layer
#'
#' @export
#'
soil_ptf_slu1 <- function(sat, pwp, depth){

  if("SpatRaster" %in% class(sat)){
    if(!requireNamespace("terra")){
      stop("The terra package is required to use soil_ptf_slu1() with input
           data of class SpatRaster. Please use install.packages(\"terra\")
           to install terra and try again.")
    }else{
      diff_r <- (sat - pwp)/2
      slu1 <- terra::xapp(diff_r, depth,
                          fun = \(.diff, .depth){
                            if(any(.depth > 15)){
                              .lyr_gt15 <- which(.depth > 15) |> min()
                              .diff[1:.lyr_gt15]
                              .depth <- .depth[1:.lyr_gt15]
                              .depth[.lyr_gt15] <- 15
                            }
                            sum(.diff*diff(c(0, .depth))*10)
                          })
    }
  }else if(is.numeric(sat)){
    if(any(depth > 15)){
      lyr_gt15 <- which(depth > 15) |> min()
      sat <- sat[1:lyr_gt15]
      pwp <- pwp[1:lyr_gt15]
      depth <- depth[1:lyr_gt15]
      depth[lyr_gt15] <- 15
    }
    slu1 <- sum((sat - pwp)/2*diff(c(0, depth))*10)
  }
  return(slu1)
}
