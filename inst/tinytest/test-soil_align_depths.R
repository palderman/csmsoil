library(tinytest)

soil_data <-
  data.frame(SLB = c(20, 40, 60, 80, 100, 120),
             SLLL = c(0.21, 0.22, 0.23, 0.24, 0.25, 0.26),
             SDUL = c(0.31, 0.32, 0.33, 0.34, 0.35, 0.36),
             SSAT = c(0.41, 0.42, 0.43, 0.44, 0.45, 0.46))

actual <-
  soil_data |>
  csmsoil::soil_align_depths(new_depths = c(5, 25, 50, 75, 100, 120))

expected <-
  with(soil_data,{
    data.frame(SLB = c(5, 25, 50, 75, 100, 120),
               SLLL = c(SLLL[1],
                        (SLLL[1]*15 + SLLL[2]*5)/20,
                        (SLLL[2]*15 + SLLL[3]*10)/25,
                        (SLLL[3]*10 + SLLL[4]*15)/25,
                        (SLLL[4]*5 + SLLL[5]*20)/25,
                        SLLL[6]),
               SDUL = c(SDUL[1],
                        (SDUL[1]*15 + SDUL[2]*5)/20,
                        (SDUL[2]*15 + SDUL[3]*10)/25,
                        (SDUL[3]*10 + SDUL[4]*15)/25,
                        (SDUL[4]*5 + SDUL[5]*20)/25,
                        SDUL[6]),
               SSAT = c(SSAT[1],
                        (SSAT[1]*15 + SSAT[2]*5)/20,
                        (SSAT[2]*15 + SSAT[3]*10)/25,
                        (SSAT[3]*10 + SSAT[4]*15)/25,
                        (SSAT[4]*5 + SSAT[5]*20)/25,
                        SSAT[6])
    )
  })

expect_equal(actual, expected)
