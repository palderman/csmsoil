get.ksat.raster <- function (silt, clay) 
{
    ksat = silt
    sand = 100 - silt - clay

    ksat[sand >= 85 & silt + 1.5 * clay <= 15] <- 21

    ksat[(sand >= 85 & sand < 90 & silt + 1.5 * clay >= 15) |
         (sand >= 70 & sand < 85 & silt + 2 * clay <= 30)] <- 6.11

    ksat[(clay <= 20 & sand >= 52 & silt + 2 * clay > 30) |
         (clay < 7 & silt < 50 & sand > 43 & sand < 52)] <- 2.59

    ksat[clay >= 7 & clay <= 27 & silt >= 28 & silt < 50 &
         sand >= 23 & sand < 52] <- 1.32

    ksat[(silt >= 50 & silt <= 88 & clay >= 12 & clay <= 27) |
         (silt >= 50 & clay >= 0 & clay < 12)] <- 0.68

    ksat[clay >= 20 & clay < 35 & silt >= 0 & silt < 28 & sand >= 45] <- 0.43

    ksat[clay >= 27 & clay < 40 & sand >= 20 & sand < 45] <- 0.23

    ksat[clay >= 27 & clay < 40 & sand >= 0 & sand < 20] <- 0.15

    ksat[clay >= 35 & sand >= 45] <- 0.12

    ksat[clay >= 40 & silt >= 40 ] <- 0.09

    ksat[clay >= 40 & sand < 45 & silt < 40] <- 0.06

    return(ksat)
}
