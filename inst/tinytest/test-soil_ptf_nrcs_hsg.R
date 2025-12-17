library(tinytest)

# test for missing terra package

# check conditions for various skss and soil depths

# skss restricting in < 50cm

# skss restricting in < 100cm

# soil depth < 100cm

# Testing for hsg = 1
expect_equal(
  soil_ptf_nrcs_hsg(41*0.36, 51),
  1
)

expect_equal(
  soil_ptf_nrcs_hsg(11*0.36, 100),
  1
)

# Testing for hsg = 2
expect_equal(
  soil_ptf_nrcs_hsg(40*0.36, 51),
  2
)

expect_equal(
  soil_ptf_nrcs_hsg(10*0.36, 100),
  2
)

# Testing for hsg = 3
expect_equal(
  soil_ptf_nrcs_hsg(10*0.36, 51),
  3
)

expect_equal(
  soil_ptf_nrcs_hsg(4*0.36, 100),
  3
)

# Testing for hsg = 4
expect_equal(
  soil_ptf_nrcs_hsg(1*0.36, 50),
  4
)

expect_equal(
  soil_ptf_nrcs_hsg(0.4*0.36, 100),
  4
)
