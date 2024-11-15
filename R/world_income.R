#' Country Codes and Income Groups of Countries and Territories
#'
#' A dataset containing a row per country/territory with information on the
#' country/territory ISO codes and income group as defined by the World Bank
#'
#' @format ## A data frame with 250 rows and 5 columns:
#' \describe{
#'    \item{alpha_3_code}{The three digit ISO code for the country/territory}
#'    \item{alpha_2_code}{The two digit ISO code for the country/territory}
#'    \item{numeric}{The numeric ISO code for the country/territory}
#'    \item{country}{Name of country or territory}
#'    \item{economy}{Name of country or territory according to the World Bank data}
#'    \item{income_group}{Income classification of the country/territory}
#'    \item{redcap_number}{The numeric value for the country/territory in the IDDO REDCap codebook}
#' }
#'
"world_income"
