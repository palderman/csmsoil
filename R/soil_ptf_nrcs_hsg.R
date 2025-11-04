#' Determine hydrologic soil group
#'
#' Determine hydrologic soil group (HSG) based on saturated conductivity
#'  with depth and the method described in Chapter 7 - "Hydrologic Soil
#'   Groups" of the NRCS Part 630 National Engineering Handbook (2009)
#'
#' @param ssks a vector or SpatRaster with saturated hydraulic
#'  conductivity values in units of cm per hour
#'
#' @param depth a vector or SpatRaster with dimensions
#'  corresponding to ssks with the depth to the base of soil layer
#'
#' @export
#'
soil_ptf_nrcs_hsg <- function(ssks, depth){

  if("SpatRaster" %in% class(ssks)){
    gt50_index <- terra::app(depth,
                             fun = \(.x) which(.x > 50) |> min())
    gt100_index <- terra::app(depth,
                              fun = \(.x) which(.x > 100) |> min())
    ksat_50 <- terra::rapp(ksat,
                           first = 1,
                           last = gt50_index,
                           fun = min)
    ksat_100 <- terra::rapp(ksat,
                            first = 1,
                            last = gt100_index,
                            fun = min)
    sl_depth <- terra::app(depth, fun = max)
  }else if(is.numeric(ssks)){
    gt50_index <- which(depth > 50) |> min()
    gt100_index <- which(depth > 100) |> min()
    ksat_50 <- min(ssks[1:gt50_index])
    ksat_100 <- min(ssks[1:gt100_index])
    sl_depth <- max(depth)
  }

  hsg <- ksat_50
  # multiply ksat thresholds by 0.36 to convert original units (um/s) to cm/hr:
  hsg[ksat_50 > 40*0.36 & sl_depth > 50] <- 1
  hsg[ksat_100 > 10*0.36 & sl_depth >= 100]  <- 1
  hsg[ksat_50 <= 40*0.36 & ksat_50 > 10*0.36 & sl_depth > 50] <- 2
  hsg[ksat_100 <= 10*0.36 & ksat_100 > 4*0.36 & sl_depth >= 100] <- 2
  hsg[ksat_50 <= 10*0.36 & ksat_50 > 1*0.36 & sl_depth > 50] <- 3
  hsg[ksat_100 <= 4*0.36 & ksat_100 > 0.4*0.36 & sl_depth >= 100]  <- 3
  hsg[ksat_50 <= 1*0.36 | sl_depth <= 50] <- 4
  hsg[ksat_100 <= 0.4*0.36 & sl_depth >= 100]  <- 4

  hsg
}
