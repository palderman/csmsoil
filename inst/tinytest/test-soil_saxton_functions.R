library(tinytest)

# From Saxton and Rawls (2006):
# Table 3. Example estimated water characteristic values for texture
#  classes at 2.5%w organic matter (OM), no salinity, gravel or
#  density adjustment.
table3 <-
  structure(list(
    texture_class = c("Sa", "LSa", "SaL", "L", "SiL",
                      "Si", "SaCL", "CL", "SiCL", "SiC", "SaC", "C"),
    sand = c(88, 80, 65, 40, 20, 10, 60, 30, 10, 10, 50, 25),
    clay = c(5, 5, 10, 20, 15, 5, 25, 35, 35, 45, 40, 50),
    theta_1500 = c(5, 5, 8, 14, 11, 6, 17, 22, 22, 27, 25, 30),
    theta_33 = c(10, 12, 18, 28, 31, 30, 27, 36, 38, 41, 36, 42),
    theta_s = c(46, 46, 45, 46, 48, 48, 43, 48, 51, 52, 44, 50),
    plant_avail = c(5, 7, 10, 14, 20, 25, 10, 14, 17, 14, 11, 12),
    Ks = c(108.1, 96.7, 50.3, 15.5, 16.1, 22,
           11.3, 4.3, 5.7, 3.7, 1.4, 1.1),
    matric_density = c(1.43, 1.43, 1.46, 1.43, 1.38, 1.38,
                       1.5, 1.39, 1.3, 1.26, 1.47, 1.33)),
    row.names = c(NA, -12L), class = "data.frame")

# Convert from volumetric percent to volumetric fraction
for(.c in c("theta_s", "theta_33", "theta_1500")){
  table3[[.c]] <- table3[[.c]]/100
}
# Convert from mm/h to cm/h
table3[["Ks"]] <- table3[["Ks"]]/10

actual <- table3[c("theta_s", "theta_33", "theta_1500", "Ks")]

actual[["theta_1500"]] <-
  csmsoil::soil_saxton_slll(silt = 100 - table3[["sand"]] - table3[["clay"]],
                            clay = table3[["clay"]],
                            soc = 2.5/1.72,
                            coarse_fraction = 0,
                            bulk_density = table3[["matric_density"]])

expect_equal(
  round(actual[["theta_1500"]], digits = 2),
    table3[["theta_1500"]])

  actual[["theta_33"]] <-
    csmsoil::soil_saxton_sdul(silt = 100 - table3[["sand"]] - table3[["clay"]],
                            clay = table3[["clay"]],
                            soc = 2.5/1.72,
                            coarse_fraction = 0,
                            bulk_density = table3[["matric_density"]])

    expect_equal(
      round(actual[["theta_33"]], digits = 2),
      table3[["theta_33"]])

    actual[["theta_s"]] <-
      csmsoil::soil_saxton_ssat(silt = 100 - table3[["sand"]] - table3[["clay"]],
                                clay = table3[["clay"]],
                                soc = 2.5/1.72,
                                coarse_fraction = 0,
                                bulk_density = table3[["matric_density"]])

    expect_equal(
      round(actual[["theta_s"]], digits = 2),
      table3[["theta_s"]])

    actual[["Ks"]] <-
      csmsoil::soil_saxton_ssks(theta_s = actual[["theta_s"]],
                                theta_33 = actual[["theta_33"]],
                                theta_1500 = actual[["theta_1500"]],
                                coarse_fraction = 0,
                                bulk_density = table3[["matric_density"]])

expect_equal(
  round(actual[["Ks"]], digits = 2),
  table3[["Ks"]])

