#' Prepare the DS Domain for VL outcome analysis.
#'
#' Prepare the Disposition (DS) domain for use in outcome analysis data sets
#' studying Visceral Leishmaniasis. Takes a IDDO-SDTM curated DS domain,
#' transforms and pivots it in order to merge it into an outcome analysis data
#' set with other domains using the ANALYSE_OUTCOME() function.
#'
#' @param DATA_DS The DS domain data frame, as named in the global environment.
#'
#' @return Two functions which each create a data frame focusing on outcome
#'   measures for VL
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_DS_OUT_VL = function(DATA_DS){
  DATA = DATA_DS %>%
    convert_blanks_to_na() %>%
    group_by(USUBJID) %>%
    mutate(ROWN = row_number()) %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID),
                names_from = ROWN, names_glue = "DISPOSITION{ROWN}_{.value}",
                values_from = c(DSDECOD, VISITDY, DSDY), names_vary = "slowest")

  colnames(DATA) = str_replace_all(colnames(DATA), "DSDY", "DAY")
  colnames(DATA) = str_replace_all(colnames(DATA), "_DSDECOD", "")

  return(DATA)
}

################################################################################
PREP_DS_OUT_VL2 = function(DATA_DS){
  DATA_DS = DATA_DS %>%
    convert_blanks_to_na() %>%
    mutate(DSDECOD = as.character(DSDECOD),
           DAY = DSDY,
           START_DAY = DSSTDY)

  # DATA_DS_EMPTY = DATA_DS %>%
  #   filter(is.na(VISITDY) & is.na(VISITNUM) & is.na(EPOCH) & is.na(DAY) & is.na(START_DAY)) %>%
  #   DERIVE_EMPTY_TIME()
  #
  # DATA_DS_JOIN = left_join(DATA_DS, DATA_DS_EMPTY)
  #
  # DATA_DS_JOIN = DATA_DS_JOIN[order(DATA_DS_JOIN$USUBJID, DATA_DS_JOIN$VISITNUM,
  #                                   DATA_DS_JOIN$VISITDY, DATA_DS_JOIN$DAY,
  #                                   DATA_DS_JOIN$START_DAY, DATA_DS_JOIN$EMPTY_TIME), ]

  DATA = DATA_DS %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID),
                names_from = DOMAIN,
                values_from = c(DSDECOD, DAY, START_DAY),
                values_fn = dplyr::last) %>%
    rename("FINAL_DISPOSITION" =  "DSDECOD_DS",
           "FINAL_DISPOSITION_DAY" = "DAY_DS",
           "FINAL_DISPOSITION_START_DAY" = "START_DAY_DS")

  return(DATA)
}
