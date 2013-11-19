context("C++ function matches R function for basic trajectories")

 mc <-sim_biomass_effort(t_end = 10L, num_pop = 2L,
     alpha = rep(0.5, 10), beta = 1/150, m = 0.01, n = 3,
     sigma = 0, q = 0.01, cpar = 1, p = 0.5)
 mr <-sim_biomass_effort(t_end = 10L, num_pop = 2L,
     alpha = rep(0.5, 10), beta = 1/150, m = 0.01, n = 3,
     sigma = 0, q = 0.01, cpar = 1, p = 0.5)

test_that("cpp matches R output", {
  expect_equal(mc, mr)
})


