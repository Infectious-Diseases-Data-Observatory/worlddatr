#' Convert country names to ISO country codes
#'
#' Matches a column of country names in your data with a bank of various country
#' names (`country_name_lookup.xlsx` in `inst/extdata`) and appends two new
#' columns for the ISO 3 and 2 letter codes in order to use standardised codes
#' instead of messy country names. The function reports in the R console how
#' many rows did not get matched with the data bank.
#'
#' @param data Data frame with a column of country names
#' @param country_name_col Character. The name of the column containing the
#'   country names.
#'
#' @returns The input data frame with two new columns alpha_3_code and
#'   alpha_2_code representing the ISO standardised 3 & 2 letter country codes
#'
#' @export
#'
#' @examples
#' country_names = data.frame(country = sample(world_income$country, 100))
#'
#' \dontrun{
#'   convert_country_to_iso(country_names, "country")
#' }
convert_country_to_iso = function(data, country_name_col){

  country_name_lookup <- read_excel("inst/extdata/country_name_lookup.xlsx")

  country_col_index = which(names(data) == country_name_col)

  colnames(data)[country_col_index] = "country_name"

  data_merge = data %>%
    left_join(country_name_lookup, by = "country_name")

  n_missing = data_merge %>%
    filter(is.na(alpha_3_code)) %>%
    nrow()

  print(str_c(
    "Number of rows missing an ISO country code after convert_country_to_iso: ", n_missing
    ))

  return(data_merge)
}
