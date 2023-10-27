#' Derive the season of enrollment.
#'
#' Creates a SEASON column based on the month in RFSTDTC with either:
#'    WINTER (December, January, February)
#'    SPRING (March, April, May)
#'    SUMMER (June, July, August)
#'    AUTUMN (September, October, November)
#'
#' @param DATA Data frame containing the RFSTDTC variable; typically the Demographics (DM) domain.
#'
#' @return The data frame with a SEASON column appended.
#'
#' @export
#'
#' @author Rhys Peploe
#'
DERIVE_SEASON = function(DATA){
  DATA$SEASON = NA

  for(i in 1:nrow(DATA)){
    if(is.na(DATA$RFSTDTC[i])){
      next
    }

    if(month(DATA$RFSTDTC[i]) <= 2 | month(DATA$RFSTDTC[i]) == 12){
      DATA$SEASON[i] = "WINTER"
    }

    else if(month(DATA$RFSTDTC[i]) > 2 & month(DATA$RFSTDTC[i]) <= 5){
      DATA$SEASON[i] = "SPRING"
    }

    else if(month(DATA$RFSTDTC[i]) > 5 & month(DATA$RFSTDTC[i]) <= 8){
      DATA$SEASON[i] = "SUMMER"
    }

    else if(month(DATA$RFSTDTC[i]) > 8 & month(DATA$RFSTDTC[i]) <= 11){
      DATA$SEASON[i] = "AUTUMN"
    }
  }

  DATA = DATA %>%
    relocate(SEASON, .after = RFSTDTC)

  return(DATA)
}
