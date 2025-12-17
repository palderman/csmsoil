library(tinytest)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 1, hsg = 1),
  61
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 1, hsg = 2),
  73
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 1, hsg = 3),
  81
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 1, hsg = 4),
  84
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 3, hsg = 1),
  64
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 3, hsg = 2),
  76
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 3, hsg = 3),
  84
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 3, hsg = 4),
  87
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 7, hsg = 1),
  68
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 7, hsg = 2),
  80
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 7, hsg = 3),
  88
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 7, hsg = 4),
  91
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 12, hsg = 1),
  71
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 12, hsg = 2),
  83
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 12, hsg = 3),
  91
)

expect_equal(
  csmsoil::soil_ptf_curve_number(slope = 12, hsg = 4),
  94
)
