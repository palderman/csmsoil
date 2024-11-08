#' Pull STATSGO or SSURGO soil profile by soil series name
#'
#' @param soil_names a character vector of soil series names to query
#'
#' @param state a character vector corresponding to `soil_names` that gives
#'   the state of the location from which the profile should be queried
#'
#' @param county a character vector corresponding to `soil_names` that gives
#'   the county of the location from which the profile should be queried
#'
#' @param pt_geom an sf object which contains the coordinates
#'   (latitude and longitude) of the point for which soil profile
#'   data are being pulled
#'
#' @param lat a numeric value of the decimal degree latitude
#'   of the point for which soil profile data are being pulled
#'
#' @param long a numeric value of the decimal degree longitude
#'   of the point for which soil profile data are being pulled
#'
#' @param STATSGO logical argument indicating whether STATSGO profiles
#'  should be included in query
#'
#' @param SSURGO logical argument indicating whether SSURGO profiles
#'  should be included in query
#'
#' @export
#'
# @examples
#'
#' @importFrom stringr str_c str_detect
#' @importFrom dplyr "%>%" mutate select full_join filter pull group_by
#'     arrange summarize_all
#' @importFrom tibble tibble
#' @importFrom soilDB SDA_query
#' @importFrom tidyr replace_na fill
#' @importFrom sf st_point st_is st_geometry st_transform st_set_crs
#'   st_sfc
#'
#'
pull_profile_by_name <- function(soil_name, state = "", county = "",
                                  pt_geom = NULL, lat = NULL, long = NULL,
                                  SSURGO = TRUE, STATSGO = !SSURGO){

  str_c("Pulling profile for ",soil_name) %>%
    message()

  comp_condition <- str_c("'",soil_name,"'") %>%
    str_c(collapse = ",") %>%
    str_c("compname IN (", . ,")")


  if(is.null(pt_geom)&&(is.null(lat)||is.null(long))){
    str_c("Either pt_geom or both lat and long must be supplied\n",
          "  for soil profiles to be filtered by coordinates.") %>%
      warning()
  }else if(is.null(pt_geom)){
    pt_geom <- st_point(c(long, lat), dim = "XY") %>%
      st_sfc() %>%
      # Assume WGS84 for lat and long
      st_set_crs("+proj=longlat +datum=WGS84")
  }else if(!st_is(st_geometry(pt_geom),"POINT")){
    str_c("pt_geom geometry is not a point. Soil profiles will not\n",
          "  be filtered by coordinates.") %>%
      warning()
    pt_geom <- NULL
  }else if(is.na(st_crs(pt_geom))){
    str_c("pt_geom is missing a coordinate reference system (CRS). Assuming\n",
          "  that CRS is WGS84 Latitude and Longitude") %>%
      warning()
    pt_geom <- pt_geom %>% 
      # Assume WGS84 for lat and long
      st_set_crs("+proj=longlat +datum=WGS84")
  }

  if(!SSURGO){
    source_condition <- " AND lgd.areaname = 'United States'"
  }else if(!STATSGO){
    source_condition <- " AND lgd.areaname != 'United States'"
  }else{
    source_condition <- ""
  }

  # Construct query for pulling map unit keys for each soil name
  mukey_query <- str_c("SELECT
                          lgd.areaname AS area_name,
                          compname AS soil_name,
                          co.mukey AS mukey,
                          cokey
                       FROM
                         component co
                       INNER JOIN
                         mapunit mu ON co.mukey = mu.mukey
                       INNER JOIN
                         legend lgd ON mu.lkey = lgd.lkey
                       WHERE ",
                       comp_condition,
                       source_condition,
                       " AND cokey IN (SELECT cokey FROM chorizon)")

  mukey_out <- SDA_query(mukey_query)

  # if(nrow(mukey_out) > 1){
  #
  #   # Construct tibble to hold regular expressions for filtering map unit keys
  #   area_names <- tibble(soil_name = soil_name,
  #                        county = county,
  #                        state = state) %>%
  #     mutate(county_regex = ifelse(county == "", ".*", str_c(county," County,")),
  #            state_regex = ifelse(state == "", ".*", str_c(" ",state)))
  #   mukey_out <- mukey_out %>%
  #     full_join(area_names)
  #
  #   state_matches <- mukey_out %>%
  #     summarize(n = sum(str_detect(area_name,state_regex))) %>%
  #     pull(n)
  #
  #   if(state_matches > 0){
  #     # Filter map unit keys by state
  #     mukey_out <- mukey_out %>%
  #       filter(str_detect(area_name,state_regex))
  #
  #     county_matches <- mukey_out %>%
  #       summarize(n = sum(str_detect(area_name,county_regex))) %>%
  #       pull(n)
  #
  #     if(nrow(mukey_out) > 1 & county_matches > 0){
  #       # Filter map unit keys by county
  #       mukey_out <- mukey_out %>%
  #         filter(str_detect(area_name,county_regex))
  #     }
  #
  #   }
  #
  #   mukey_out <- mukey_out %>%
  #     select(-matches("_regex"))
  #
  # }

  if(nrow(mukey_out) > 1 & !is.null(pt_geom)){
    # filter by coordinates
    mukey_out <- filter_mukey_by_coord(mukey_out, pt_geom, soil_name)
  }

  comp_tbl <- mukey_out %>%
    pull(cokey) %>%
    str_c("'",.,"'") %>%
    str_c(collapse=",") %>%
    str_c("SELECT
             compname,
             cokey,
             COALESCE(comppct_r,'') AS comppct_r,
             COALESCE(hydgrp,'') AS hydgrp,
             COALESCE(slope_r,'') AS slope_r,
             COALESCE(drainagecl,'') AS drainage,
             COALESCE(albedodry_r,'') AS albedodry_r
           FROM component
           WHERE
             cokey IN (",
          .,
          ")",
          sep = " ") %>%
    SDA_query()

  # Query horizon data for each cokey
  horizon_tbl <- mukey_out %>%
    gen_horiz_query() %>% 
    SDA_query()

  soil <- sda_to_dssat_tbl(comp_tbl, horizon_tbl, state, pt_geom)

  return(soil)
}

#' Filter data associated with SSURGO or STATSGO map unit keys by latitude and longitude
#'
#' @param mukey_tbl a numeric or character vector of map unit keys
#'
#' @param lat a numeric value of latitude
#'
#' @param long a numeric value of longitude
#'
#' @importFrom stringr str_c
#' @importFrom dplyr "%>%" filter pull row_number
#' @importFrom soilDB SDA_query processSDA_WKT
#' @importFrom sf st_as_sf st_point st_sfc st_nearest_feature st_crs st_transform
#'   st_as_text st_buffer
#'
filter_mukey_by_coord <- function(mukey_tbl, pt_geom, soil_name){

  # Expanding search radii in meters (equivalent to 1km to 2000km)
  radii <- c(1e3, 1e4, 5e4, 1e5*1:10, 1e6+1e5*1:10)

  for(radius in radii){

    sprintf("Attempting search radius of %ikm", radius/1000) %>%
      message()

    if(radius/1000 > 200){
      sprintf("For %s attempting search radius of %ikm", soil_name, radius/1000) %>%
        warning()
    }

    query_wkt <- pt_geom %>%
      # Extract geometry
      st_geometry() %>%
      # Transform to Albers Equal Area projection for buffering operation
      st_transform(st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")) %>%
      # Buffer point with expanding radius at each iteration
      st_buffer(dist = radius) %>%
      # Transform back to WGS84 for SDA query
      st_transform(st_crs("+proj=longlat +datum=WGS84")) %>%
      # Pull geometry out
      st_geometry() %>%
      # Convert to WKT
      st_as_text()

    # Run spatial query with specified radius
    mukey_subset <- mukey_tbl %>%
      pull(mukey) %>%
      str_c(collapse=",") %>%
      str_c("SELECT mukey, muname
             FROM mapunit
             WHERE mukey IN (
                SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('", query_wkt, "')
              )
              AND mukey IN (",.,")") %>%
      SDA_query()

    if(!is.null(mukey_subset) > 0) break

  }

  if(nrow(mukey_subset) > 1){

    # Query geometries for list of map unit keys
    mukey_geom <- mukey_subset %>%
      pull(mukey) %>%
      str_c(collapse=",") %>%
      str_c("SELECT G.MupolygonWktWgs84 AS geom, mapunit.mukey AS mukey
             FROM mapunit
               CROSS APPLY SDA_Get_MupolygonWktWgs84_from_Mukey(mapunit.mukey) AS G
                     WHERE mukey IN (", .,")") %>%
      SDA_query() %>%
      # Convert to sf
      st_as_sf(wkt = "geom", crs = "+proj=longlat +datum=WGS84") %>%
      # Transform to Albers Equal Area projection for finding nearest mukey
      st_transform(st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))

    query_pt <- pt_geom %>%
      # Make sure query point is sf object
      st_as_sf() %>%
      # Transform to Albers Equal Area projection for finding nearest mukey
      st_transform(st_crs(mukey_geom))

    # Find nearest mukey
    nearest_mukey <- mukey_geom %>%
      filter(., row_number() == st_nearest_feature(query_pt,.)) %>%
      pull(mukey)

  }else{
    nearest_mukey <- mukey_subset %>%
      pull(mukey)
  }

  # filter mukey to nearest
  mukey_out <- mukey_tbl %>%
    filter(mukey == nearest_mukey)

  return(mukey_out)
}


#' Convert returned SSURGO or STATSGO data to a soil DSSAT_tbl
#'
#' @param sda_tbl a tibble returned from numeric or character vector of map unit keys
#'
#' @param lat a numeric value of latitude
#'
#' @param long a numeric value of longitude
#'
#' @export
#'
#' @importFrom stringr str_c str_detect str_replace_all str_pad
#'   str_to_upper str_sub
#' @importFrom dplyr "%>%" rename mutate group_by select summarize_all arrange
#'   full_join pull ungroup
#' @importFrom tidyr replace_na fill
#' @importFrom DSSAT as_DSSAT_tbl
#' @importFrom sf st_coordinates
#'
sda_to_dssat_tbl <- function(comp_tbl, horizon_tbl, site, pt_geom){

  dssat_tbl <- horizon_tbl %>%
    mutate(# Adjust SLCF for bedrock (r or R) layers
           # Assume remaining missing SLCF is 0% coarse fraction
           SLCF = ifelse(str_detect(hzname,"[rR]") & is.na(fragvol_r),
                         99,
                         fragvol_r) %>%
             replace_na(0),
           SBDM = calc_sbdm(.),
           SSAT = calc_ssat(.),
           SDUL = calc_sdul(.),
           SLLL = wfifteenbar_r/100.,
           # Adjust SRGF, and SSKS for bedrock (r or R) layers
           SRGF = ifelse(str_detect(hzname,"[rR]"),
                         1-SLCF/100,
                         1),
           SSKS = ifelse((str_detect(hzname,"[rR]") & is.na(ksat_r)) | ksat_r < 0.001/60/60*10000,
                         0.001,
                         # 60*60/10000 converts from micrometers per second to cm per hour
                         ksat_r*60*60/10000),
           SLOC = om_r/1.724,
           SLNI = NA,
           SLHW = NA,
           SLHB = NA,
           SCEC = NA,
           SADC = NA
    ) %>%
    rename(SLB = hzdepb_r,
           SLCL = claytotal_r,
           SLSI = silttotal_r,
           SLMH = hzname) %>%
    #fill missing values with values from row prior.
    fill(.,c(SDUL,SLLL,SSAT,SLSI,SLCL,SBDM,SLOC)) %>%
    select(-matches("_r$")) %>%
    group_by(cokey, SLB) %>%
    summarize_all(~if(is.numeric(.)){
      mean(., na.rm=TRUE)
    }else if(all(is.na(.))){
      as.numeric(NA)
    }else{
      str_c(unique(.),collapse="/")
    }) %>%
    arrange(cokey, SLB) %>%
    summarize_all(~list(.))

  soil_tbl <- comp_tbl %>%
    full_join(dssat_tbl) %>%
    mutate(SLDR = switch(drainage,
                         'Excessively drained' = 0.85,
                         'Somewhat excessively drained' = 0.75,
                         'Well drained' = 0.60,
                         'Moderately well drained' = 0.40,
                         'Somewhat poorly drained' = 0.25,
                         'Poorly drained' = 0.05,
                         'Very poorly drained' = 0.01),
           SLRO = slro_fun(hyd.group = hydgrp, slope = slope_r)) %>% #converting hydgrp letter to hydgrp number
    ungroup() %>%
    mutate(PEDON = compname %>% #generate unique soil ID for each compname. Should be 10 digits long.
             abbreviate(.,minlength = 8) %>%
             toupper(.) %>%
             str_replace_all(.,c('\\. +' = '_',
                                 ' +' = '_')) %>%
             str_pad(.,width=8,side='left',pad = '0') %>%
             str_c({str_sub(site,1,2) %>% str_to_upper()},.),
           SOURCE = "NRCS SDA",
           TEXTURE = NA,
           DEPTH = SLB %>%
             unlist() %>%
             max(),
           DESCRIPTION = NA,
           SITE = site,
           COUNTRY = "USA",
           LAT = if(!is.null(pt_geom)){
             pt_geom %>%
               st_coordinates() %>%
               {.[1,2]}
           }else{
             NA
           },
           LONG = if(!is.null(pt_geom)){
             pt_geom %>%
               st_coordinates() %>%
               {.[1,1]}
           }else{
             NA
           },
           `SCS FAMILY` = NA,
           SCOM = NA,
           SLU1 = 6,
           SLNF = 1,
           SLPF = 1,
           SMHB = "IB001",
           SMPX = "IB001",
           SMKE = "IB001") %>%
    rename(SALB = albedodry_r) %>%
    select(c(c("PEDON"),
                c("SITE","COUNTRY","LAT","LONG","SCS FAMILY"),
                c("SCOM","SALB","SLU1","SLDR","SLRO","SLNF","SLPF","SMHB","SMPX",
                  "SMKE"),
                c("SLB","SLMH","SLLL","SDUL","SSAT","SRGF","SSKS","SBDM","SLOC",
                  "SLCL", "SLSI","SLCF","SLNI","SLHW","SLHB","SCEC","SADC"))) %>%
    as_DSSAT_tbl(
      tier_info = list(c("PEDON"),
                       c("SITE","COUNTRY","LAT","LONG","SCS FAMILY"),
                       c("SCOM","SALB","SLU1","SLDR","SLRO","SLNF","SLPF","SMHB","SMPX",
                         "SMKE"),
                       c("SLB","SLMH","SLLL","SDUL","SSAT","SRGF","SSKS","SBDM","SLOC",
                         "SLCL", "SLSI","SLCF","SLNI","SLHW","SLHB","SCEC","SADC")))

  return(soil_tbl)
}

#' @importFrom dplyr "%>%" mutate coalesce pull
#' @importFrom stringr str_c
calc_ssat <- function(h_tbl){

  ssat_tbl <- h_tbl %>% 
    mutate(# Fill missing values in particle density with default value of 2.65
           partdensity = coalesce(partdensity, 2.65),
           # Set dbtenthbar_r to NA if wtenthbar_r is NA
           dbtenthbar_r = ifelse(is.na(wtenthbar_r), NA, dbtenthbar_r),
           # Set dbthirdbar_r to NA if wthirdbar_r is NA
           dbthirdbar_r = ifelse(is.na(wthirdbar_r), NA, dbthirdbar_r),
           # SSAT as 95% pore space estimated from tenthbar bulk density
           ssat_tenth = 0.95*(1-dbtenthbar_r/partdensity),
           # SSAT as 95% pore space estimated from tenthbar bulk density
           ssat_third = 0.95*(1-dbthirdbar_r/partdensity),
           # SSAT as volumetric water content at/near 0 bar tension
           ssat_wsat = wsatiated_r/100,
           # Order of preference from left to right
           ssat = coalesce(ssat_wsat, ssat_tenth, ssat_third))

  # Check for missing ssat values
  if({ssat_tbl %>% 
        pull(ssat) %>% 
        is.na() %>% 
        any()}){
    
    str_c("Missing values present in SSAT. Will attempt to fill\n",
          "  using dbovendry_r from SSURGO. Per SSURGO documentation,\n",
          "  dbovendry_r excludes desiccation cracks in its calculation\n",
          "  of bulk density and, thus, may vastly underestimate SSAT\n",
          "  for soils with high shrink/swell potential. Please check\n",
          "  output for errant SSAT values.") %>% 
      warning()
    
    ssat_tbl <- ssat_tbl %>% 
      mutate(# SSAT as 95% pore space estimated from oven dry bulk density
             #   (per SSURGO documentation dbovendry_r excludes volume of
             #    desiccation cracks)
             ssat_dry = 0.95*(1-dbovendry_r/partdensity),
             ssat = coalesce(ssat, ssat_dry))
  }
  ssat <- ssat_tbl %>% 
    pull(ssat)
  
  return(ssat)
}

#' @importFrom dplyr "%>%" mutate coalesce pull
calc_sdul <- function(h_tbl){
  sdul <- h_tbl %>% 
    mutate(# Determine if layer is coarsely textured:
           coarse = is_coarse(sandtotal_r, silttotal_r, claytotal_r),
           # Convert percent wthirdbar to fraction
           wthird = wthirdbar_r/100,
           # Fill missing values in wtenthbar with wthirdbar 
           wtenth = coalesce(wtenthbar_r/100, wthird), 
           sdul = ifelse(coarse, wtenth, wthird)) %>% 
    pull(sdul)
  
  return(sdul)
}

is_coarse <- function(sand, silt, clay){
  coarse <- 
    # Sand
    (sand >= 85 & (silt+1.5*clay) <= 15) |
    # Loamy Sand
    ((sand >= 85 & sand < 90 & (silt + 1.5*clay) >= 15) |
     (sand >= 70 & sand < 85 & (silt + 2*clay) <= 30)) |
    # Sandy Loam
    ((clay <= 20 & sand >= 52 & (silt + 2*clay) > 30) |
     (clay < 7 & silt < 50 & sand > 43 & sand < 52))
  return(coarse)
}

#' @importFrom dplyr "%>%" mutate coalesce pull
#' @importFrom stringr str_c
calc_sbdm <- function(h_tbl){
  
  sbdm_tbl <- h_tbl %>% 
    mutate(# Order of preference from left to right
      sbdm = coalesce(dbtenthbar_r, dbthirdbar_r))
  
  # Check for missing ssat values
  if({sbdm_tbl %>% 
      pull(sbdm) %>% 
      is.na() %>% 
      any()}){
    
    str_c("Missing values present in SBDM. Will attempt to fill\n",
          "  using dbovendry_r from SSURGO. Per SSURGO documentation,\n",
          "  dbovendry_r excludes desiccation cracks in its calculation\n",
          "  of bulk density and, thus, may vastly overestimate SBDM\n",
          "  for soils with high shrink/swell potential. Please check\n",
          "  output for errant values.") %>% 
      warning()
    
    sbdm_tbl <- sbdm_tbl %>% 
      mutate(
        #   (per SSURGO documentation dbovendry_r excludes volume of
        #    desiccation cracks)
        sbdm = coalesce(sbdm, dbovendry_r))
  }
  
  sbdm <- sbdm_tbl %>% 
    pull(sbdm)
  
  return(sbdm)
}

slro_fun <- function(hyd.group,slope){

  hyd.group <- substr(hyd.group,1,1)

  if(slope<0|!hyd.group%in%c('A','B','C','D')) return(NA)

  slro <- matrix(c(61,73,81,84,
                   64,76,84,87,
                   68,80,88,91,
                   71,83,91,94),
                 byrow=TRUE,
                 ncol=4)

  if(slope>=0&slope<=2){
    i=1
  }else if(slope>2&slope<=5){
    i=2
  }else if(slope>5&slope<=10){
    i=3
  }else if(slope>10){
    i=4
  }
  j <- switch(hyd.group,
              A=1,
              B=2,
              C=3,
              D=4)
  return(slro[i,j])
}

#' Pull STATSGO or SSURGO soil profile by coordinates
#'
#' @param pt_geom an sf object which contains the coordinates
#'   (latitude and longitude) of the point for which soil profile
#'   data are being pulled
#'
#' @param lat a numeric value of the decimal degree latitude
#'   of the point for which soil profile data are being pulled
#'
#' @param long a numeric value of the decimal degree longitude
#'   of the point for which soil profile data are being pulled
#'
#' @param site the site name for which the soil profile is being pulled
#'
#' @param STATSGO logical argument indicating whether STATSGO profiles
#'  should be included in query
#'
#' @param SSURGO logical argument indicating whether SSURGO profiles
#'  should be included in query
#'
#' @export
#'
# @examples
#'
#' @importFrom stringr str_c
#' @importFrom dplyr "%>%"
#' @importFrom soilDB SDA_query
#' @importFrom sf st_point st_is st_geometry st_as_text st_set_crs
#'
pull_profile_by_coords <- function(pt_geom = NULL,
                                   lat = NULL, long = NULL,
                                   site = "",
                                   SSURGO = TRUE, STATSGO = !SSURGO){

  if(is.null(pt_geom)&&(is.null(lat)|is.null(long))){
    stop("Either pt_geom or lat and long must be supplied.")
  }else if(is.null(pt_geom)){
    pt_geom <- st_point(c(long, lat), dim = "XY") %>%
      st_sfc() %>%
      # Assume WGS84 for lat and long
      st_set_crs("+proj=longlat +datum=WGS84")
  }else if(!st_is(st_geometry(pt_geom),"POINT")){
    stop("pt_geom geometry is not a point.")
  }else if(is.na(st_crs(pt_geom))){
    str_c("pt_geom is missing a coordinate reference system (CRS). Assuming\n",
          "  that CRS is WGS84 Latitude and Longitude") %>%
      warning()
    pt_geom <- pt_geom %>% 
      # Assume WGS84 for lat and long
      st_set_crs("+proj=longlat +datum=WGS84")
  }

  # str_c("Pulling profile for coordinates:",pt_geom) %>%
  #   message()

  if(!SSURGO){
    source_condition <- "lgd.areaname = 'United States'"
  }else if(!STATSGO){
    source_condition <- "lgd.areaname != 'United States'"
  }else{
    source_condition <- NULL
  }

  # Construct query for pulling map unit keys for each soil name
  mukey_query <- pt_geom %>%
    st_geometry() %>%
    st_as_text() %>%
    str_c("SELECT
             lgd.areaname AS area_name,
             compname AS soil_name,
             co.mukey AS mukey,
             cokey
           FROM
             component co
           INNER JOIN
             mapunit mu ON co.mukey = mu.mukey
           INNER JOIN
             legend lgd ON mu.lkey = lgd.lkey
           WHERE mu.mukey IN(
             SELECT
               *
             FROM
               SDA_Get_Mukey_from_intersection_with_WktWgs84('",.,"')",
          ")
          ")

  if(!is.null(source_condition)){
    mukey_query <- mukey_query %>%
      str_c("AND
              ",source_condition)
  }

  mukey_out <- SDA_query(mukey_query)

  comp_tbl <- mukey_out %>%
    pull(cokey) %>%
    str_c("'",.,"'") %>%
    str_c(collapse=",") %>%
    str_c("SELECT
             compname,
             cokey,
             COALESCE(comppct_r,'') AS comppct_r,
             COALESCE(hydgrp,'') AS hydgrp,
             COALESCE(slope_r,'') AS slope_r,
             COALESCE(drainagecl,'') AS drainage,
             COALESCE(albedodry_r,'') AS albedodry_r
           FROM component
           WHERE
             cokey IN (",
          .,
          ")",
          sep = " ") %>%
    SDA_query()

  if(nrow(comp_tbl) > 1){

    warn_msg <- comp_tbl %>%
      pull(compname) %>%
      str_c(collapse = ", ") %>%
      str_c("More than one soil type found for query point:\n",
            "  ", ., "\n\n",
            "  Attempting to filter to soil type with greatest \n",
            "  mapunit component percentage...")

    comp_tbl <- comp_tbl %>%
      filter(comppct_r == max(comppct_r))

    if(nrow(comp_tbl) == 1){

      warn_msg <- comp_tbl %>%
        pull(compname) %>%
        str_c(warn_msg,
              "SUCCESS!\n\n  The selected soil type is: ", .,
              "\n  If you wish to use a different soil type,\n",
              "  consider using pull_profile_by_name()")

    }else{

      comp_tbl <- comp_tbl %>%
        head(1)

      warn_msg <- comp_tbl %>%
        pull(compname) %>%
        str_c(warn_msg,
              "FAILURE!\n\n  More than one soil type is equal to the maximum\n",
              "  mapunit component percentage. Arbitrarily selecting\n  ", .,
              ". If you wish to use a different soil type,\n",
              "  consider using pull_profile_by_name().")

    }

    warning(warn_msg)
  }

  # Query horizon data for each cokey
  horizon_tbl <- comp_tbl %>%
    gen_horiz_query() %>% 
    SDA_query()

  soil <- sda_to_dssat_tbl(comp_tbl, horizon_tbl, site, pt_geom)

  return(soil)
}

#' @importFrom stringr str_c
#' @importFrom dplyr "%>%" pull
#' 
gen_horiz_query <- function(comp_tbl){

  query <- comp_tbl %>% 
    pull(cokey) %>%
    str_c("'",.,"'") %>%
    str_c(collapse=",") %>%
    str_c("SELECT
             hzdepb_r,
             dbovendry_r,
             dbtenthbar_r,
             dbthirdbar_r,
             dbfifteenbar_r,
             wsatiated_r,
             wtenthbar_r,
             wthirdbar_r,
             partdensity,
             ksat_r,
             wfifteenbar_r,
             sandtotal_r,
             claytotal_r,
             silttotal_r,
             om_r,
             hzname,
             fragvol_r,
             cokey
           FROM chorizon
           LEFT JOIN chfrags on chfrags.chkey = chorizon.chkey
           WHERE
             cokey IN (",
          .,
          ")",
          "ORDER BY cokey, hzdepb_r",
          sep = " ")

  return(query)

}