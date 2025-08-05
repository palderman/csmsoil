# csmsoil

The purpose of this package is to provide interfaces to soil data sources,
pedotransfer functions and other utilities for preparing data suitable for crop
modeling.

The package currently provides:

- an interface to the NRCS SSURGO/STATSGO/NATSGO databases

Future versions will include:

- Interfaces to other soil databases:
    - SoilGrids
    - Harmonized World Soil Database (HWSD)
- Implementations of common pedotransfer functions (PTF):
    - Rawls et al (1982)
    - Rawls and Brakensiek (1985)
    - Saxton and Rawls (2006)
    - Gijsman et al. (2007)
    - Rosetta3 (Zhang and Schaap, 2017)
- a function for coercing existing data frames to a form suitable for use with the `write_wth()` function from the DSSAT R package ([https://cran.r-project.org/package=DSSAT](https://cran.r-project.org/package=DSSAT))
- additional functions to facilitate unit conversion
