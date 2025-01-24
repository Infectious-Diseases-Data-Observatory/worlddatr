#' Prepare the RP domain for baseline analysis.
#'
#' Prepare the Reproductive System Findings (RP) domain for use in baseline
#' analysis data sets. Takes a IDDO-SDTM curated RP domain, transforms and
#' pivots it in order to merge it into a baseline analysis data set with other
#' domains using the ANALYSE_BASELINE() functions. Default variables are:
#' "PREGIND" & "EGESTAGE" (renamed as "EGA")
#'
#' @param DATA_RP The RP domain data frame, as named in the global environment.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for RPTESTCD as
#'   specified in the RP section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("LMPSTDTC").
#'
#' @return Wide data frame containing a row per USUBJID/subject, with RPTESTCDs
#'   and the units as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_RP_BL <- function(DATA_RP, VARS = NULL) {
  RP_VARS <- c("PREGIND", "EGESTAGE", str_to_upper(VARS))

  DATA <- DATA_RP %>%
    convert_blanks_to_na() %>%
    filter(.data$RPTESTCD %in% RP_VARS) %>%
    DERIVE_TIMING() %>%
    mutate(
      RPSTRES = str_to_upper(as.character(.data$RPSTRESN)),
      RPSTRESC = str_to_upper(as.character(.data$RPSTRESC)),
      RPORRES = str_to_upper(as.character(.data$RPORRES)),
      RPUNITS = as.character(NA)
    )

  DATA$RPSTRESC <- str_replace_all(DATA$RPSTRESC, "NEGATIVE", "N")

  DATA[which(is.na(DATA$RPSTRES)), "RPSTRES"] <-
    DATA[which(is.na(DATA$RPSTRES)), "RPSTRESC"]
  DATA[which(is.na(DATA$RPSTRES)), "RPSTRES"] <-
    DATA[which(is.na(DATA$RPSTRES)), "RPORRES"]

  DATA[which(!is.na(DATA$RPSTRESC) | !is.na(DATA$RPSTRESN)), "RPUNITS"] <-
    DATA[which(!is.na(DATA$RPSTRESC) | !is.na(DATA$RPSTRESN)), "RPSTRESU"]
  DATA[which(is.na(DATA$RPSTRESC) & is.na(DATA$RPSTRESN)), "RPUNITS"] <-
    DATA[which(is.na(DATA$RPSTRESC) & is.na(DATA$RPSTRESN)), "RPORRESU"]

  DATA <- DATA %>%
    filter(.data$TIMING == "1" | .data$TIMING == "BASELINE") %>%
    pivot_wider(
      id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$RPTESTCD,
      values_from = c(.data$RPSTRES, .data$RPUNITS), names_vary = "slowest",
      names_sort = T,
      values_fn = first, names_glue = "{RPTESTCD}_{.value}"
    )

  if ("EGESTAGE" %in% names(DATA)) {
    DATA <- DATA %>%
      rename("EGA" = "EGESTAGE")
  }

  colnames(DATA) <- gsub("_RPSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("RPUNITS", "UNITS", colnames(DATA))

  return(DATA)
}
