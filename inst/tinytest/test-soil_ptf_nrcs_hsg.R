library(tinytest)

##############################
# tests for SpatRaster inputs:
##############################

# test for missing terra package

# Create 3 x 3 SpatRaster inputs that correspond to the 9 conditions tested
# under the vector inputs below

##########################
# Tests for vector inputs:
##########################

# Testing for hsg = 1

expect_equal(
  csmsoil::soil_ptf_nrcs_hsg(41*0.36, 51),
  1
)

expect_equal(
  csmsoil::soil_ptf_nrcs_hsg(11*0.36, 101),
  1
)

# Testing for hsg = 2
expect_equal(
  csmsoil::soil_ptf_nrcs_hsg(40*0.36, 51),
  2
)

expect_equal(
  csmsoil::soil_ptf_nrcs_hsg(10*0.36, 101),
  2
)

# Testing for hsg = 3
expect_equal(
  csmsoil::soil_ptf_nrcs_hsg(10*0.36, 51),
  3
)

expect_equal(
  csmsoil::soil_ptf_nrcs_hsg(4*0.36, 101),
  3
)

# Testing for hsg = 4

expect_equal(
  csmsoil::soil_ptf_nrcs_hsg(1*0.36, 25),
  4
)

expect_equal(
  csmsoil::soil_ptf_nrcs_hsg(1*0.36, 51),
  4
)

expect_equal(
  csmsoil::soil_ptf_nrcs_hsg(0.4*0.36, 101),
  4
)
