#' Prepare the RP domain for follow up analysis.
#'
#' Prepare the Reproductive System Findings (RP) domain for use in follow up
#' analysis data sets. Takes a IDDO-SDTM curated RP domain, transforms and
#' pivots it in order to merge it into a follow up analysis data set with other
#' domains using the ANALYSE_FOLLOW_UP() functions.
#'
#' @param DATA_RP The RP domain data frame, as named in the global environment.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for RPTESTCD as
#'   specified in the RP section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("LMPSTDTC").
#'
#' @return Wide data frame containing a row per subject per day, with RPTESTCDs as
#'   columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_RP_FU = function(DATA_RP, VARS = NULL){
  RP_VARS = c("PREGIND", "EGESTAGE", str_to_upper(VARS))

  DATA_RP = DATA_RP %>%
    convert_blanks_to_na() %>%
    filter(.data$RPTESTCD %in% RP_VARS) %>%
    mutate(RPSTRES = as.character(.data$RPSTRESN),
           RPSTRESC = as.character(.data$RPSTRESC),
           RPORRES = as.character(.data$RPORRES),
           DAY = .data$RPDY)

  DATA_RP$RPSTRESC = str_replace_all(DATA_RP$RPSTRESC, "NEGATIVE", "N")

  DATA_EMPTY = DATA_RP %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA = DATA_RP %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$RPSTRES)), "RPSTRES"] =
    DATA[which(is.na(DATA$RPSTRES)), "RPSTRESC"]
  DATA[which(is.na(DATA$RPSTRES)), "RPSTRES"] =
    DATA[which(is.na(DATA$RPSTRES)), "RPORRES"]

  DATA = DATA %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID, .data$VISITDY, .data$VISITNUM,
                            .data$DAY, .data$EMPTY_TIME), names_from = .data$RPTESTCD,
                values_from = .data$RPSTRES, names_vary = "slowest",
                names_sort = T, values_fn = first)

  if("EGESTAGE" %in% names(DATA)){
    DATA = DATA %>%
      rename("EGA" = "EGESTAGE")
  }

  return(DATA)
}
