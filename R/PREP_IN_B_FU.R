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
    mutate(INSTRES = str_to_upper(as.character(.data$INDECOD)),
           INMODIFY = str_to_upper(as.character(.data$INMODIFY)),
           INTRT = str_to_upper(as.character(.data$INTRT)),
           DAY = .data$INDY,
           START_DAY = .data$INSTDY,
           END_DAY = .data$INENDY)  %>%
    CLEAN_IN()

  DATA_IN[which(is.na(DATA_IN$INSTRES)), "INSTRES"] =
    DATA_IN[which(is.na(DATA_IN$INSTRES)), "INMODIFY"]
  DATA_IN[which(is.na(DATA_IN$INSTRES)), "INSTRES"] =
    DATA_IN[which(is.na(DATA_IN$INSTRES)), "INTRT"]

  DATA_IN = DATA_IN %>%
    filter(.data$INSTRES %in% "BLOOD_TRANSFUSION") %>%
    mutate(INPRESP = str_to_upper(.data$INPRESP),
           INOCCUR = str_to_upper(.data$INOCCUR))

  if(any(is.na(DATA_IN$INPRESP) == T)) {
    DATA_IN[which(is.na(DATA_IN$INPRESP)), "INPRESP"] = "N"
    DATA_IN[which(DATA_IN$INPRESP == "N"), "INOCCUR"] = "Y"
  }

  DATA_EMPTY = DATA_IN %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY) &
             is.na(.data$START_DAY) & is.na(.data$END_DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA = DATA_IN %>%
    left_join(DATA_EMPTY) %>%
    mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID, .data$VISITDY, .data$VISITNUM,
                            .data$DAY, .data$START_DAY, .data$END_DAY, .data$EMPTY_TIME),
                names_from = .data$INTRT, names_glue = "{INTRT}",
                values_from = .data$INOCCUR,
                values_fn = first)

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
