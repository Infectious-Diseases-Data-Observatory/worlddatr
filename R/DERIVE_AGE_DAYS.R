#' Create a variable for AGE in days.
#'
#' Takes the AGE variable and converts the units into days. This is an
#' additional variable and the original AGE and AGEU will be retained when
#' returned. DAYS = WEEKS x 7; MONTHS x 30.417; YEARS x 365.25, result is then
#' rounded down.
#'
#' AGE_DAYS is used to calculate WHO growth standards in the DERIVE_ANTHRO() function.
#'
#' @param DATA Data frame containing the AGE and AGEU variables, typically the
#'   Demographics (DM) domain.
#'
#' @return Data frame with the additional AGE_DAYS variable, providing the ages
#'   in days.
#' @export
#'
#'
DERIVE_AGE_DAYS = function(DATA) {
  DATA = DATA %>%
    mutate(AGEU = str_to_upper(AGEU))

  AGE_DAYS = NA

  for (i in seq(1, nrow(DATA), 1)) {
    if(is.na(DATA$AGEU[i])) {
      next
    }
    else if (DATA$AGEU[i] == "DAYS") {
      AGE_DAYS[i] = floor(DATA$AGE[i])
    }
    else if (DATA$AGEU[i] == "WEEKS") {
      AGE_DAYS[i] = floor(DATA$AGE[i]*7)
    }
    else if (DATA$AGEU[i] == "MONTHS") {
      AGE_DAYS[i] = floor(DATA$AGE[i]*30.417)
    }
    else if (DATA$AGEU[i] == "YEARS") {
      AGE_DAYS[i] = floor(DATA$AGE[i]*365.25)
    }
  }

  return(cbind(DATA, AGE_DAYS))
}
