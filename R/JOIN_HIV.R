#' Join multiple HIV variables into one combined variable for analysis.
#'
#' HIV can be represented as a Microbiology test (MB Domain) and an Clinical and
#' Adverse Effect (SA Domain). This takes the MB test and where NAs exist, it is
#' populated by the SA entries if they exist.
#'
#' @param DATA Data frame containing both the MB test for HIV and the SA event
#'   for HIV.
#' @param SA_TIME If TRUE, HIV_SAEVINTX (HIV_TIME) will be relocated after the
#'   joint HIV variable. Boolean. Requires HIV_SAEVINTX in data frame.
#' @param SA_DUR If TRUE, HIV_SADUR (HIV_DUR) will be relocated after the joint
#'   HIV variable. Boolean. Requires HIV_SADUR in data frame.
#'
#' @return Data frame with a combined HIV column and the HIV_SAOCCUR column
#'   removed
#'
#' @author Rhys Peploe
#'
JOIN_HIV = function(DATA, SA_TIME = FALSE, SA_DUR = FALSE){
  if(("HIV_SAOCCUR" %in% names(DATA)) & ("HIV" %in% names(DATA))){
    DATA[which(is.na(DATA$HIV)), "HIV"] =
      DATA[which(is.na(DATA$HIV)), "HIV_SAOCCUR"]

    DATA = DATA %>%
      dplyr::select(-HIV_SAOCCUR)

    if(SA_TIME == TRUE){
      DATA = DATA %>%
        relocate(HIV_SAEVINTX, .after = HIV)
    }

    if(SA_DUR == TRUE){
      DATA = DATA %>%
        relocate(HIV_SADUR, .after = HIV)
    }
  }

  if("HIV_PRESP" %in% names(DATA)){
    DATA = DATA %>%
      relocate(HIV_PRESP, .after = HIV)
  }

  return(DATA)
}
