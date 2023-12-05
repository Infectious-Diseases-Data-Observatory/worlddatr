#' @title Derive TIMING variable to subset baseline information.
#'
#' @description Creates a new variable 'TIMING' which takes the EPOCH variable,
#'   and where NAs exist, they are filled with values from the VISITDY variable.
#'   This results in TIMING being a mixture of EPOCH and VISITDY, which is used
#'   to subset data to TIMING == "BASELINE or TIMING == 1 in order to get
#'   baseline information from domains.
#'
#' @param DOMAIN Domain name in the global environment.
#'
#' @return Data frame with the new TIMING variable appended.
#'
#' @export
#'
#' @author Rhys Peploe
#'
DERIVE_TIMING = function(DOMAIN){
  DATA = DOMAIN %>%
    mutate(TIMING = as.character(.data$EPOCH),
           VISITDY = as.character(.data$VISITDY))

  DATA[which(is.na(DATA$TIMING)), "TIMING"] =
    DATA[which(is.na(DATA$TIMING)), "VISITDY"]

  return(DATA)
}
