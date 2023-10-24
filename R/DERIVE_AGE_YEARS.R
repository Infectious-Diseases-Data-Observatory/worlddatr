#' @title Convert AGE to years.
#'
#' @description Convert the AGE of all subjects to years and change the AGEU to
#'   "YEARS".
#'
#' @param DATA Data frame containing the AGE and AGEU variables; typically the
#'   Demographics (DM) domain.
#'
#' @return Data frame with AGE and AGEU in years as opposed to the original
#'   values.
#' @export
#'
#' @author Rhys Peploe
#'
#'
DERIVE_AGE_YEARS = function(DATA){
  for (i in seq(1, nrow(DATA), 1)){
    if(is.na(DATA$AGEU[i])){
      next
    }
    else if(DATA$AGEU[i] == "DAYS"){
      DATA$AGE[i] = floor(DATA$AGE[i]/365.25)
      DATA$AGEU[i] = "YEARS"
    }
    else if(DATA$AGEU[i] == "WEEKS"){
      DATA$AGE[i] = floor(DATA$AGE[i]/52)
      DATA$AGEU[i] = "YEARS"
    }
    else if(DATA$AGEU[i] == "MONTHS"){
      DATA$AGE[i] = floor(DATA$AGE[i]/12)
      DATA$AGEU[i] = "YEARS"
    }
    else{
      DATA$AGE[i] = floor(DATA$AGE[i])
      DATA$AGEU[i] = "YEARS"
    }
  }
  return(DATA)
}
