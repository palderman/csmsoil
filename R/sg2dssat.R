read.sg.layered <- function(dssat.var='SLOC',res='1km',mask=NULL){
    require(raster)
    files <- paste0(dssat.to.sg(dssat.var),'_M_sl',1:7,'_',res,'_ll.tif')
    if(is.null(mask)){
        the.data <- stack(files)
    }else{
        the.data <- crop(stack(files),mask)
    }
    if(dssat.var=='SLOC'){
        the.data <- the.data/10
    }else if(dssat.var=='SBDM'){
        the.data <- the.data * 0.001
    }else if(dssat.var%in%c('SLLL','SDUL','SSAT','SLCF','AWCh3')){
        the.data <- the.data * 0.01
    }
    the.data <- the.data[[1:6]]/2+the.data[[2:7]]/2
    return(the.data)
}

read.sg <- function(var='BDTICM',res='1km',mask=NULL){
    require(raster)
    file <- paste0(var,'_M_',res,'_ll.tif')
    if(var=='TAXNWRB') file=paste0(var,'_',res,'_ll.tif')
    if(is.null(mask)){
        the.data <- raster(file)
    }else{
        the.data <- crop(raster(file),mask)
    }
    return(the.data)
}

dssat.to.sg <- function(dssat){
    dssat2sg <- structure(
        c("AWCh1", "AWCtS", "BLDFIE", "CLYPPT", "CRFVOL","ORCDRC", "SLTPPT",
          "WWP"),
        .Names = c("SDUL", "SSAT", "SBDM", "SLCL", "SLCF", "SLOC", "SLSI",
                   "SLLL"))
    if(dssat%in%names(dssat2sg)){
        return(dssat2sg[dssat])
    }else{
        return(dssat)
    }
}

get.sldr <- function(taxnwrb){

    require(GSIF)
    require(dplyr)
    require(Hmisc)

    data(soil.classes)

    taxnwrb <- ratify(taxnwrb)

    rat <- levels(taxnwrb)[[1]]

    rat$FAO_90=soil.classes$FAO1990.WRB$FAO_90[rat$ID]

    hwsd <- mdb.get('HWSD.mdb')

    rat <- group_by(hwsd$HWSD_DATA,SU.SYM90) %>%
        dplyr::summarize(.,drainage=round(mean(DRAINAGE,na.rm=TRUE))) %>%
        merge(.,rat,by.x='SU.SYM90',by.y='FAO_90')

    rat$drainage[is.nan(rat$drainage)]=1

    levels(taxnwrb)[[1]] <- rat[,c('ID','drainage')]


    drainage.class <- deratify(taxnwrb,att='drainage')
    sldr <- drainage.class
    sldr[drainage.class == 1] = 0.01
    sldr[drainage.class == 2] = 0.05
    sldr[drainage.class == 3] = 0.25
    sldr[drainage.class == 4] = 0.40
    sldr[drainage.class == 5] = 0.60
    sldr[drainage.class == 6] = 0.75
    sldr[drainage.class == 7] = 0.85
    return(sldr)
}

get.hsg <- function(ksat,sl.depth){

    ksat.50 <- stackApply(ksat,c(rep(1,4),2,3),fun=min)[[1]]
    ksat.100 <- stackApply(ksat,c(rep(1,5),2),fun=min)[[1]]
    hsg <- ksat.50
    hsg[ksat.50 > 40*0.36 & sl.depth > 50] = 1
    hsg[ksat.100 > 10*0.36 & sl.depth >= 100] = 1
    hsg[ksat.50 <= 40*0.36 & ksat.50 > 10*0.36 & sl.depth > 50] = 2
    hsg[ksat.100 <= 10*0.36 & ksat.100 > 4*0.36 & sl.depth >= 100] = 2
    hsg[ksat.50 <= 10*0.36 & ksat.50 > 1*0.36 & sl.depth > 50] = 3
    hsg[ksat.100 <= 4*0.36 & ksat.100 > 0.4*0.36 & sl.depth >= 100] = 3
    hsg[ksat.50 <= 1*0.36 | sl.depth <= 50] = 4
    hsg[ksat.100 <= 0.4*0.36 & sl.depth >= 100] = 4

    return(hsg)
}

get.slro <- function(ksat,slope,sl.depth){

    hsg <- get.hsg(ksat,sl.depth)

    slro <- hsg
    slro[slope >= 0 & slope <= 2 & hsg == 1] = 61
    slro[slope > 2 & slope <= 5 & hsg == 1] = 64
    slro[slope > 5 & slope <= 10 & hsg == 1] = 68
    slro[slope > 10 & hsg == 1] = 71
    slro[slope > 0 & slope <= 2 & hsg == 2] = 73
    slro[slope > 2 & slope <= 5 & hsg == 2] = 76
    slro[slope > 5 & slope <= 10 & hsg == 2] = 80
    slro[slope > 10 & hsg == 2] = 83
    slro[slope >= 0 & slope <= 2 & hsg == 3] = 81
    slro[slope > 2 & slope <= 5 & hsg == 3] = 84
    slro[slope > 5 & slope <= 10 & hsg == 3] = 88
    slro[slope > 10 & hsg == 3] = 91
    slro[slope >= 0 & slope <= 2 & hsg == 4] = 84
    slro[slope > 2 & slope <= 5 & hsg == 4] = 87
    slro[slope > 5 & slope <= 10 & hsg == 4] = 91
    slro[slope > 10 & hsg == 4] = 94

    return(slro)
}

get.slb <- function(depth){
    SLB <- brick(c(depth,depth,depth,depth,depth,depth))
    values(SLB[[1]]) <- 5
    values(SLB[[2]]) <- 15
    values(SLB[[3]]) <- 30
    values(SLB[[4]]) <- 60
    values(SLB[[5]]) <- 100
    values(SLB[[6]]) <- 200
    SLB[[1]][depth < 5 & depth > 0] <- depth[ depth < 5 & depth > 0]
    SLB[[1]][depth <= 0] <- NA
    SLB[[2]][depth < 5 & depth > 15] <- depth[ depth < 5 & depth > 15]
    SLB[[2]][depth <= 15] <- NA
    SLB[[3]][depth < 30 & depth > 15] <- depth[ depth < 30 & depth > 15]
    SLB[[3]][depth <= 30] <- NA
    SLB[[4]][depth < 60 & depth > 30] <- depth[ depth < 60 & depth > 30]
    SLB[[4]][depth <= 30] <- NA
    SLB[[5]][depth < 100 & depth > 60] <- depth[ depth < 100 & depth > 60]
    SLB[[5]][depth <= 60] <- NA
    SLB[[6]][depth < 200 & depth > 100] <- depth[ depth < 200 & depth > 100]
    SLB[[6]][depth <= 100] <- NA
    return(SLB)
}

nc.dssat.soil <- function(file.name='ok_soilgrids.nc',mask=NULL,res='1km',chirps.grid=NULL){
    require(raster)
    require(dplyr)
    require(ncdf4)

    if(is.null(chirps.grid)){
        chirps.grid <-
            raster('~/projects/ok_nasa_rig/data_source/chirps_ok.nc') %>%
            crop(.,mask)
    }

    depth <- read.sg('BDTICM',res=res,mask=mask)

    cat('Processing SLB...')
    SLB <- get.slb(depth)
    aggregate(SLB,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SLB.nc',varname='SLB',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SRGF...')
    SRGF <- SLB
    SRGF[[1]] <- 1
    SRGF[[2]] <- 1
    SRGF[[3]] <- 0.64
    SRGF[[4]] <- 0.41
    SRGF[[5]] <- 0.20
    SRGF[[6]] <- 0.05
    aggregate(SRGF,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SRGF.nc',varname='SRGF',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SLCF...')
    SLCF <- read.sg.layered('SLCF',res=res,mask=mask)
    SLCF[is.na(SLB)] <- NA
    aggregate(SLCF,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SLCF.nc',varname='SLCF',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SBDM...')
    SBDM <- read.sg.layered('SBDM',res=res,mask=mask)
    SBDM[is.na(SLB)] <- NA
    aggregate(SBDM,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SBDM.nc',varname='SBDM',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SLOC...')
    SLOC <- read.sg.layered('SLOC',res=res,mask=mask)
    SLOC[is.na(SLB)] <- NA
    aggregate(SLOC,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SLOC.nc',varname='SLOC',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SLCL...')
    SLCL <- read.sg.layered('SLCL',res=res,mask=mask)
    SLCL[is.na(SLB)] <- NA
    aggregate(SLCL,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SLCL.nc',varname='SLCL',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SLSI...')
    SLSI <- read.sg.layered('SLSI',res=res,mask=mask)
    SLSI[is.na(SLB)] <- NA
    aggregate(SLSI,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SLSI.nc',varname='SLSI',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SLLL...')
    SLLL <- saxton.06.slll(SLSI,SLCL,SLOC,SBDM,SLCF)
    SLLL[is.na(SLB)] <- NA
    aggregate(SLLL,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SLLL.nc',varname='SLLL',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SDUL...')
    SDUL <- saxton.06.sdul(SLSI,SLCL,SLOC,SBDM,SLCF)
    SDUL[is.na(SLB)] <- NA
    aggregate(SDUL,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SDUL.nc',varname='SDUL',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SSAT...')
    SSAT <- saxton.06.ssat(SLSI,SLCL,SLOC,SBDM,SLCF)
    SSAT[is.na(SLB)] <- NA
    aggregate(SSAT,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SSAT.nc',varname='SSAT',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SSKS...')
    SSKS <- saxton.06.ssks(theta.S=SSAT,theta.33=SDUL,theta.1500=SLLL,
                           Rw=SLCF,BD=SBDM)
    SSKS[is.na(SLB)] <- NA
    aggregate(SSKS,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SSKS.nc',varname='SSKS',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SLU1...')
    SLU1 <- SDUL[[1:2]]-(SDUL[[1:2]]+SLLL[[1:2]])/2
    SLU1[[1]] <- SLU1[[1]]*5*10
    SLU1[[2]] <- SLU1[[2]]*10*10
    stackApply(SLU1,c(1,1),sum) %>%
        aggregate(.,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SLU1.nc',varname='SLU1',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SALB...')
    SALB <- SLB[[1]]
    values(SALB) <- 0.13
        aggregate(SALB,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SALB.nc',varname='SALB',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SLDR...')
    TAXNWRB <- read.sg('TAXNWRB',res=res,mask=mask)
    SLDR <- get.sldr(TAXNWRB) %>%
        aggregate(.,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SLDR.nc',varname='SLDR',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    cat('Processing SLRO...')
    slope <- raster('ok_slope_1km.tif') %>%
        projectRaster(.,depth)
    get.slro(SSKS,slope,depth) %>%
        aggregate(.,fact=5) %>%
        resample(.,chirps.grid) %>%
        writeRaster(.,'SLRO.nc',varname='SLRO',zname='layer',overwrite=TRUE,NAflag=-99)
    cat('done.\n')

    for(v in c('SLLL','SDUL','SSAT','SLCF','SBDM','SSKS',
               'SLOC','SLSI','SLCL','SLU1','SLDR','SLRO',
               'SRGF','SALB')){
        cat(paste0('Appending ',v,'...'))
        system(paste0('ncks -A ',v,'.nc SLB.nc'))
        cat('done.\n')
    }

    system(paste0("ncap -O -s 'latitude=float(latitude);longitude=float(longitude)' SLB.nc ",file.name))

    for(v in c('SLB','SLLL','SDUL','SSAT','SLCF','SBDM','SSKS',
               'SLOC','SLSI','SLCL','SLU1','SLDR','SLRO','SALB','SRGF')){
         system(paste0('rm ',v,'.nc'))
    }

#    nc <- nc_open('oksoilgrid.nc',write=TRUE)
#    lyrdim <- nc$dim[['layer']]
#    SLB <- ncvar_def('SLB','cm',list(lyrdim),-99)
#    nc <- ncvar_add(nc,SLB)
#    ncvar_put(nc,'SLB',c(5,15,30,60,100,200))
#    nc_close(nc)

}

get.slu1 <- function(fc,wp,slb){
    require(raster)
    return(150*(stackApply(fc,rep(1,nlayers(fc)),fun=sum)/nlayers(fc)-
          0.5*stackApply(wp,rep(1,nlayers(wp)),fun=sum)/nlayers(wp)))
}
