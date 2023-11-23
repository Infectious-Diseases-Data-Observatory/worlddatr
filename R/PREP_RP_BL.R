#' Prepare the RP domain for baseline analysis.
#'
#' Prepare the Reproductive System Findings (RP) domain for use in baseline
#' analysis data sets. Takes a IDDO-SDTM curated RP domain, transforms and
#' pivots it in order to merge it into a baseline analysis data set with other
#' domains using the ANALYSE_BASELINE() functions.
#'
#' @param DATA_RP The RP domain data frame, as named in the global environment.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for RPTESTCD as
#'   specified in the RP section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("EGESTAGE", "LMPSTDTC").
#'
#' @return Wide data frame containing a row per USUBJID/subject, with RPTESTCDs
#'   as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_RP_BL = function(DATA_RP, VARS = NULL){
  RP_VARS = c("PREGIND", "EGESTAGE", str_to_upper(VARS))

  DATA = DATA_RP %>%
    convert_blanks_to_na() %>%
    filter(.data$RPTESTCD %in% RP_VARS) %>%
    DERIVE_TIMING() %>%
    mutate(RPSTRES = as.character(.data$RPSTRESN),
           RPSTRESC = as.character(.data$RPSTRESC),
           RPORRES = as.character(.data$RPORRES))

  DATA$RPSTRESC = str_replace_all(DATA$RPSTRESC, "NEGATIVE", "N")

  DATA[which(is.na(DATA$RPSTRES)), "RPSTRES"] =
    DATA[which(is.na(DATA$RPSTRES)), "RPSTRESC"]
  DATA[which(is.na(DATA$RPSTRES)), "RPSTRES"] =
    DATA[which(is.na(DATA$RPSTRES)), "RPORRES"]

  DATA = DATA %>%
    filter(TIMING == 1 | TIMING == "BASELINE") %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$RPTESTCD,
                values_from = .data$RPSTRES, names_vary = "slowest",
                names_sort = T,
                values_fn = first)

  if("EGESTAGE" %in% names(DATA)){
    DATA = DATA %>%
      rename("EGA" = "EGESTAGE")
  }

  return(DATA)
}
