#' Further prepare the IN domain for follow up analysis, specifically for Blood
#' Transfusion events.
#'
#' Prepare the Treatments and Interventions (IN) domain for use in follow up
#' analysis data sets focusing on Blood Transfusions. Takes a
#' IDDO-SDTM curated IN domain, transforms and pivots it in order to merge it
#' into a follow up analysis data set with other domains using the
#' ANALYSE_FOLLOW_UP() function. PREP_IN_FU() and PREP_IN_B_FU() would be
#' merged in the ANALYSE_FOLLOW_UP() function.
#'
#' @param DATA_IN The IN domain data frame, as named in the global environment.
#'
#' @return Dataframe containing a row per USUBJID/subject per day, with IN terms as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_IN_B_FU = function(DATA_IN){
  DATA_IN = DATA_IN %>%
    convert_blanks_to_na() %>%
    mutate(INSTRES = str_to_upper(as.character(INDECOD)),
           INMODIFY = str_to_upper(as.character(INMODIFY)),
           INTRT = str_to_upper(as.character(INTRT)),
           DAY = INDY,
           START_DAY = INSTDY,
           END_DAY = INENDY)  %>%
    CLEAN_IN()

  DATA_IN[which(is.na(DATA_IN$INSTRES)), "INSTRES"] =
    DATA_IN[which(is.na(DATA_IN$INSTRES)), "INMODIFY"]
  DATA_IN[which(is.na(DATA_IN$INSTRES)), "INSTRES"] =
    DATA_IN[which(is.na(DATA_IN$INSTRES)), "INTRT"]

  DATA_IN = DATA_IN %>%
    filter(INSTRES %in% "BLOOD_TRANSFUSION") %>%
    mutate(INPRESP = str_to_upper(INPRESP),
           INOCCUR = str_to_upper(INOCCUR))

  if(any(is.na(DATA_IN$INPRESP) == T)) {
    DATA_IN[which(is.na(DATA_IN$INPRESP)), "INPRESP"] = "N"
    DATA_IN[which(DATA_IN$INPRESP == "N"), "INOCCUR"] = "Y"
  }

  DATA_EMPTY = DATA_IN %>%
    filter(is.na(VISITDY) & is.na(VISITNUM) & is.na(DAY) & is.na(START_DAY) & is.na(END_DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA = DATA_IN %>%
    left_join(DATA_EMPTY) %>%
    mutate(INOCCUR = as.factor(INOCCUR)) %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID, VISITDY, VISITNUM, DAY, START_DAY, END_DAY, EMPTY_TIME),
                names_from = INTRT, names_glue = "{INTRT}",
                values_from = INOCCUR,
                values_fn = first)

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
