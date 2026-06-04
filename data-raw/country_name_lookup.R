library(readxl)
library(usethis)

country_name_lookup = read_xlsx("inst/extdata/country_name_lookup.xlsx")

use_data(country_name_lookup, internal = TRUE, overwrite = TRUE)
