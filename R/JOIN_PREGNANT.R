#' Join multiple pregnancy indicator variables into one combined variable for
#' analysis.
#'
#' Pregnancy can be confirmed with a lab test (LB Domain) or by asking the
#' subject (RP Domain). This takes the LB test column and where NAs exist, they
#' are populated with the result of PREGIND, the pregnancy indicator from the RP
#' domain.
#'
#' @param DATA Data frame containing both the MB test for HIV and the SA event
#'   for HIV.
#'
#' @return Data frame with a PREGNANT column, and PREGIND & HCG removed.
#'
#' @author Rhys Peploe
#'
JOIN_PREGNANT = function(DATA){
  if(("HCG" %in% names(DATA)) & ("PREGIND" %in% names(DATA))){
    DATA$PREGNANT = DATA$HCG

    DATA[which(is.na(DATA$PREGNANT)), "PREGNANT"] =
      DATA[which(is.na(DATA$PREGNANT)), "PREGIND"]

    DATA = DATA %>%
      dplyr::select(-PREGIND, -HCG)

    if("EGA" %in% names(DATA)){
      DATA = DATA %>%
        relocate(EGA, .after = PREGNANT)
    }
  }

  return(DATA)
}
