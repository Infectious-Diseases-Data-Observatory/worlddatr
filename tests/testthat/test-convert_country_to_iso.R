# test_that("Check country names are converted to ISO", {
#   countries = data.frame(
#     country = c("St. Lucia", "Saint Lucia", "St. Lucia")
#   )
#
#   out <- convert_country_to_iso(countries, "country")
#
#   expect_equal(nrow(out), nrow(countries))
#   expect_all_true(out$alpha_3_code == "LCA")
# })
#
# test_that("Check missing country name does not convert", {
#   countries = data.frame(
#     country = c("St. Lucia", "Island of Saint Lucia")
#   )
#
#   out <- convert_country_to_iso(countries, "country")
# })check

