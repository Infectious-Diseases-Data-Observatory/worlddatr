test_that("Check country names are converted to ISO", {
  countries = data.frame(
    country = c("St. Lucia", "Saint Lucia", "St. Lucia")
  )

  out <- convert_country_to_iso(countries, "country")

  expect_equal(nrow(out), nrow(countries))
  expect_all_true(out$alpha_3_code == "LCA")
})

test_that("Check missing country name does not convert", {
  countries = data.frame(
    country = c("St. Lucia", "Island of Saint Lucia")
  )

  out <- convert_country_to_iso(countries, "country")
})

test_that("Check non-ASCII characters are converted", {
  countries = data.frame(
    country = c("Côte d'Ivoire", "Réunion", "São Tomé and Príncipe")
  )

  out <- convert_country_to_iso(countries, "country")

  expect_equal(out$alpha_3_code[1], "CIV")
  expect_equal(out$alpha_3_code[2], "REU")
  expect_equal(out$alpha_3_code[3], "STP")
})

test_that("Check print message shows accurate information", {
  countries_one_missing = data.frame(
    country = c("Côte d'Ivoire", "Réunion", "unknown country")
  )

  countries_none_missing = data.frame(
    country = c("Côte d'Ivoire", "Réunion", "Benin")
  )

  msg_one_missing <- capture.output(
    convert_country_to_iso(countries_one_missing, "country")
  )
  msg_none_missing <- capture.output(
    convert_country_to_iso(countries_none_missing, "country")
  )

  expect_true(any(grepl("Number of rows.*: 1", msg_one_missing)))
  expect_true(any(grepl("Number of rows.*: 0", msg_none_missing)))
})
