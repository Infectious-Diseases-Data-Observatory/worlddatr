test_that("compute_bmi_works", {
  input_df = data.frame(WEIGHT = c(45), HEIGHT = c(150), AGE = c(34), BMI = c(222))

  output_df = data.frame(WEIGHT = c(45), HEIGHT = c(150), AGE = c(34), BMI = c(20))

  expect_equal(DERIVE_BMI(input_df), output_df)
})
