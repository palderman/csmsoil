library(tinytest)

expect_error(csmsoil::soil_ptf_hwsd_sldr())

expect_equal(csmsoil::soil_ptf_hwsd_sldr(30), NA_real_)

expect_equal_to_reference(csmsoil::soil_ptf_hwsd_sldr(0:29),
                          file = "soil_ptf_hwsd_sldr.rds")
