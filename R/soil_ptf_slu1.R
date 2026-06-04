#' Determine stage 1 soil evaporation limit
#'
#' Determine the stage 1 soil evaporation limit following Ritchie et al. (1989)
#'  based on sand, silt and clay percentage in the first 15-cm of the soil
#'  profile.
#'
#' @param silt a vector or SpatRaster with percent silt content
#'
#' @param clay a vector or SpatRaster with percent clay content
#'
#' @param depth a vector or SpatRaster with dimensions corresponding to silt and
#'  clay with the depth to the base of soil layer
#'
#' @export
#'
soil_ptf_slu1 <- function(silt, clay, depth){

  sand = 100 - silt - clay

  if("SpatRaster" %in% class(silt)){
    if(!requireNamespace("terra")){
      stop("The terra package is required to use soil_ptf_slu1() with input
           data of class SpatRaster. Please use install.packages(\"terra\")
           to install terra and try again.")
    }else{
      clay <- terra::xapp(clay, depth,
                          fun = \(.clay, .depth){
                            if(any(.depth > 15)){
                              .lyr_gt15 <- which(.depth > 15) |> min()
                              .depth <- .depth[1:.lyr_gt15]
                              .depth[.lyr_gt15] <- 15
                            }
                            sum(.clay*diff(c(0, .depth))/sum(.depth))
                          })
      silt <- terra::xapp(silt, depth,
                          fun = \(.silt, .depth){
                            if(any(.depth > 15)){
                              .lyr_gt15 <- which(.depth > 15) |> min()
                              .depth <- .depth[1:.lyr_gt15]
                              .depth[.lyr_gt15] <- 15
                            }
                            sum(.silt*diff(c(0, .depth))/sum(.depth))
                          })
      sand <- 100 - silt - clay
    }
  }else if(is.numeric(silt)){
    if(any(depth > 15)){
      lyr_gt15 <- which(depth > 15) |> min()
      sand <- sand[1:lyr_gt15]
      clay <- clay[1:lyr_gt15]
      depth <- depth[1:lyr_gt15]
      depth[lyr_gt15] <- 15
    }
    sand <- sum(sand*diff(c(0, depth))/sum(depth))
    clay <- sum(clay*diff(c(0, depth))/sum(depth))
  }
  slu1 <- sand
  slu1[clay < 50 & sand < 80] <- 8 + 0.08*clay
  slu1[clay >= 50] <- 11 - 0.06 * clay
  slu1[sand >= 80] <- 20 - 0.15 * sand
  return(slu1)
}
