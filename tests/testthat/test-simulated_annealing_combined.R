data(demo_asa24)

test_that("Function runs on demo data", {
  result <- simulated_annealing_combined(demo_asa24, candidate = 1, niter = 100, diet_score = "HEI2015")
  expect_type(result, "list")
})
