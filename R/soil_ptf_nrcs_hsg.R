#' Determine hydrologic soil group
#'
#' Determine hydrologic soil group (HSG) based on saturated conductivity
#'  with depth and the method described in Chapter 7 - "Hydrologic Soil
#'  Groups" of the NRCS Part 630 National Engineering Handbook (2009). The
#'  current implementation assumes the absence of a high water table.
#'
#' @param ksat a vector or SpatRaster with saturated hydraulic
#'  conductivity values in units of cm per hour
#'
#' @param depth a vector or SpatRaster with dimensions
#'  corresponding to ksat with the depth to the base of soil layer
#'
#' @export
#'
soil_ptf_nrcs_hsg <- function(ksat, depth){

  if("SpatRaster" %in% class(ksat)){
    if(!requireNamespace("terra")){
      stop("The terra package is required to use soil_ptf_nrcs_hsg() with input
           data of class SpatRaster. Please use install.packages(\"terra\")
           to install terra and try again.")
    }else{
      gt50_index <- terra::app(depth,
                               fun = \(.x){
                                 if(any(.x > 50)){
                                   which(.x > 50) |> min()
                                 }else{
                                   length(.x)
                                 }
                               })
      gt100_index <- terra::app(depth,
                                fun = \(.x){
                                  if(any(.x > 100)){
                                    which(.x > 100) |> min()
                                  }else{
                                    length(.x)
                                  }
                                  })
      ksat_50 <- terra::rapp(ksat,
                             first = 1,
                             last = gt50_index,
                             fun = min)
      ksat_100 <- terra::rapp(ksat,
                              first = 1,
                              last = gt100_index,
                              fun = min)
      sl_depth <- terra::app(depth, fun = max)
    }
  }else if(is.numeric(ssks)){
    if(any(depth > 50)){
      gt50_index <- which(depth > 50) |> min()
      ksat_50 <- min(ksat[1:gt50_index])
    }else{
      ksat_50 <- min(ksat)
    }
    if(any(depth > 100)){
      gt100_index <- which(depth > 100) |> min()
      ksat_100 <- min(ksat[1:gt100_index])
    }else{
      ksat_100 <- min(ksat)
    }
    sl_depth <- max(depth)
  }

  hsg <- ksat_50
  # multiply ksat thresholds by 0.36 to convert original units (um/s) to cm/hr:
  hsg[ksat_50 > 40*0.36 & sl_depth > 50 & sl_depth <= 100] <- 1
  hsg[ksat_100 > 10*0.36 & sl_depth > 100]  <- 1
  hsg[ksat_50 <= 40*0.36 & ksat_50 > 10*0.36 & sl_depth > 50 & sl_depth <= 100] <- 2
  hsg[ksat_100 <= 10*0.36 & ksat_100 > 4*0.36 & sl_depth > 100] <- 2
  hsg[ksat_50 <= 10*0.36 & ksat_50 > 1*0.36 & sl_depth > 50 & sl_depth <= 100] <- 3
  hsg[ksat_100 <= 4*0.36 & ksat_100 > 0.4*0.36 & sl_depth > 100]  <- 3
  hsg[(ksat_50 <= 1*0.36 & sl_depth > 50 & sl_depth <= 100)| sl_depth < 50] <- 4
  hsg[ksat_100 <= 0.4*0.36 & sl_depth > 100]  <- 4

  hsg
}
