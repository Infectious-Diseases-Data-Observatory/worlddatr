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
#' @export
#'
#' @author Rhys Peploe
#'
JOIN_PREGNANT = function(DATA){
  if(("HCG" %in% names(DATA)) & ("PREGIND" %in% names(DATA))){
    DATA$DOMAIN_IND = NA

    DATA$PREGNANT = DATA$HCG
    DATA$PREGNANT_UNITS = DATA$HCG_UNITS

    DATA[which(!is.na(DATA$PREGNANT) | !is.na(DATA$PREGNANT_UNITS)), "DOMAIN_IND"] = "LB"

    DATA[which(is.na(DATA$DOMAIN_IND)), "PREGNANT"] =
      DATA[which(is.na(DATA$DOMAIN_IND)), "PREGIND"]

    DATA[which(is.na(DATA$DOMAIN_IND)), "PREGNANT_UNITS"] =
      DATA[which(is.na(DATA$DOMAIN_IND)), "PREGIND_UNITS"]

    DATA = DATA %>%
      dplyr::select(-"PREGIND", -"HCG", -"DOMAIN_IND",
                    -"PREGIND_UNITS", -"HCG_UNITS")

    if("EGA" %in% names(DATA)){
      DATA = DATA %>%
        relocate("EGA_UNITS", .after = "PREGNANT_UNITS") %>%
        relocate("EGA", .after = "PREGNANT_UNITS")
    }
  }

  return(DATA)
}
