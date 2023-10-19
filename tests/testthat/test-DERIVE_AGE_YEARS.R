test_that("DERIVE_AGE_YEARS works", {
  input_df = data.frame(AGE = c(5, 16, 58, 60, 50),
                    AGEU = c("YEARS", "MONTHS", "years", "MONTHS", "WEEKS"))

  output_df = data.frame(AGE = c(5, 1, 58, 5, 0),
                         AGEU = c("YEARS", "YEARS", "YEARS", "YEARS", "YEARS"))

  expect_equal(DERIVE_AGE_YEARS(input_df), output_df)
})
