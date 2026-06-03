library(tinytest)

# Use Table 1 from Allen et al (2005) for validation
table_1 <- structure(
  list(
    sand = c(92, 84, 65, 40, 20, 7, 10, 8, 22),
    silt = c(4, 6, 25, 40, 65, 88, 55, 47, 20),
    clay = c(4, 10, 10, 20, 15, 5, 35, 45, 58),
    slu1 = c(6, 9, 9, 10, 9, 8, 11, 12, 8)),
  row.names = c(NA, -9L), class = "data.frame")

# Value in second row of Table 1 was incorrect
table_1[2, "slu1"] <- 7

actual <-
  table_1 |>
  with({
    mapply(silt = silt,
           clay = clay,
           depth = 15,
           csmsoil::soil_ptf_slu1)
  }) |>
  round()


expect_equal(actual,
             table_1$slu1)
