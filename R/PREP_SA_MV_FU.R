#' Further prepare the SA domain for follow up analysis specifically for Malaria
#' and VL variables.
#'
#' Prepare the Clinical and Adverse Effects (SA) domain for use in follow up
#' analysis data sets focusing on Malaria and Visceral Leishmaniasis. Takes a
#' IDDO-SDTM curated SA domain, transforms and pivots it in order to merge it
#' into a follow up analysis data set with other domains using the
#' ANALYSE_FOLLOW_UP() function. PREP_SA_BL() and PREP_SA_MV_BL() would be merged
#' in the ANALYSE_FOLLOW_UP() function.
#'
#' @param DATA_SA The SA domain data frame, as named in the global environment.
#'
#' @return Data frame with one row per USUBJID/subject per day, with Malaria and VL
#'   specific SATERMs as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_SA_MV_FU = function(DATA_SA){
  DATA_SA = DATA_SA %>%
    convert_blanks_to_na() %>%
    mutate(SASTRES = str_to_upper(as.character(SADECOD)),
           SAMODIFY = str_to_upper(as.character(SAMODIFY)),
           SATERM = str_to_upper(as.character(SATERM)),
           DAY = SADY,
           START_DAY = SASTDY,
           END_DAY = SAENDY) %>%
    CLEAN_SA()

  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] =
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SAMODIFY"]
  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] =
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SATERM"]

  DATA_SA = DATA_SA %>%
    filter(SASTRES %in% c("MALARIA"))

  if(any(is.na(DATA_SA$SAPRESP))) {
    DATA_SA[which(is.na(DATA_SA$SAPRESP)), "SAPRESP"] = "N"
    DATA_SA[which(DATA_SA$SAPRESP == "N"), "SAOCCUR"] = "Y"
  }

  DATA_EMPTY = DATA_SA %>%
    filter(is.na(VISITDY) & is.na(VISITNUM) & is.na(DAY) & is.na(START_DAY) & is.na(END_DAY)) %>%
    DERIVE_EMPTY_TIME()

  MALARIA_SEVERE = DATA_SA %>%
    filter(SASTRES == "MALARIA" & SASEV == "SEVERE") %>%
    mutate(MALARIA_SEV = "Y") %>%
    dplyr::select(USUBJID, MALARIA_SEV)

  DATA = DATA_SA %>%
    left_join(DATA_EMPTY) %>%
    mutate(SAOCCUR = as.factor(SAOCCUR)) %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID, VISITDY, VISITNUM, DAY, START_DAY, END_DAY, EMPTY_TIME),
                names_from = SASTRES,
                values_from = SAOCCUR,
                values_fn = first)

  DATA = DATA %>%
    clean_names(case = "all_caps")

  DATA = DATA %>%
    left_join(MALARIA_SEVERE)

  return(DATA)
}
