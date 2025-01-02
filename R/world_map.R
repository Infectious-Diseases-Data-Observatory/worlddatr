#' Global Coordinates of Countries and Territories, with Country Codes and
#' Income Group Data
#'
#' A dataset with coordinate data of the perimeter of each country/territory in
#' the world, along with information on the country/territory ISO codes and
#' income group as defined by the World Bank.
#'
#' Note that region refers to the area which the points create a perimeter for.
#'
#' Dates that data was accessed on:
#'   - World Bank: 11/11/2024
#'   - ISO:        18/06/2024
#'   - REDCap:     15/11/2024
#'   - map_data:   18/11/2024 (ggplot2 v3.4.3)
#'
#' @format ## A data frame with 99,338 rows and 11 columns:
#' \describe{
#'    \item{alpha_3_code}{The three digit ISO code for the country/territory}
#'    \item{alpha_2_code}{The two digit ISO code for the country/territory}
#'    \item{numeric}{}
#'    \item{lat}{Latitude of point}
#'    \item{long}{Longitude of point}
#'    \item{group}{Numerical identifer for the region}
#'    \item{order}{Sequence of points within region}
#'    \item{region}{Name of Country or Territory}
#'    \item{subregion}{Name of subregion, if appropiate}
#'    \item{country}{Name of country or territory according to ISO data}
#'    \item{economy}{Name of country or territory according to the World Bank data}
#'    \item{income_group}{Income classification of the country/territory}
#'    \item{redcap_number}{The numeric value for the country/territory in the IDDO REDCap codebook}
#' }
#'
"world_map"
