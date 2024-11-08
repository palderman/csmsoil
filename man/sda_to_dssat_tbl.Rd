% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSURGO_functions.R
\name{sda_to_dssat_tbl}
\alias{sda_to_dssat_tbl}
\title{Convert returned SSURGO or STATSGO data to a soil DSSAT_tbl}
\usage{
sda_to_dssat_tbl(comp_tbl, horizon_tbl, site, pt_geom)
}
\arguments{
\item{sda_tbl}{a tibble returned from numeric or character vector of map unit keys}

\item{lat}{a numeric value of latitude}

\item{long}{a numeric value of longitude}
}
\description{
Convert returned SSURGO or STATSGO data to a soil DSSAT_tbl
}
