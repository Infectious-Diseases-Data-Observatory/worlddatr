#' @title Prepare the DM domain for all outputs.
#'
#' @description Prepare the Demographics (DM) Domain for use in baseline,
#'   follow-up, treatment and outcome data sets. Takes a IDDO-SDTM curated DM
#'   domain and prepares the domain for merging into an analysis data set with
#'   other domains.
#'
#' @param DATA_DM The DM domain data frame, as named in the global environment.
#' @param DISEASE The name of the disease theme being analysed. Character
#'   string. Default is NULL (selects base variables). Select from: "MALARIA",
#'   "VL" or "EBOLA". If selection is missing or misspelt, then the base
#'   variables will be used.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use column names as specified in the DM section
#'   of the 'IDDO SDTM Implementation Manual'. i.e. c("DTHFL", "DTHDTC").
#'
#' @return Dataframe containing a row per USUBJID/subject.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_DM <- function(DATA_DM, DISEASE = NULL, VARS = NULL) {
  DISEASE <- str_to_upper(DISEASE)

  if (DISEASE == "MALARIA") {
    COLUMNS <- c(
      "STUDYID", "USUBJID", "SITEID", "AGE", "AGE_DAYS", "SEX", "ARMCD",
      "ARM", "COUNTRY", "RFSTDTC", "RACE", "ETHNIC", str_to_upper(VARS)
    )
  } else if (DISEASE == "VL") {
    COLUMNS <- c(
      "STUDYID", "USUBJID", "SITEID", "AGE", "AGE_DAYS", "SEX", "ARMCD",
      "ARM", "COUNTRY", "RFSTDTC", "ETHNIC", str_to_upper(VARS)
    )
  } else if (DISEASE == "EBOLA") {
    COLUMNS <- c(
      "STUDYID", "USUBJID", "SITEID", "AGE", "SEX", "ARMCD",
      "ARM", "COUNTRY", str_to_upper(VARS)
    )
  } else {
    COLUMNS <- c(
      "STUDYID", "USUBJID", "SITEID", "AGE", "AGE_DAYS", "SEX", "ARMCD",
      "ARM", "COUNTRY", "RFSTDTC", "RACE", "ETHNIC", str_to_upper(VARS)
    )
  }

  if ("AGE_DAYS" %in% COLUMNS) {
    DATA <- DATA_DM %>%
      convert_blanks_to_na() %>%
      DERIVE_AGE_DAYS() %>%
      DERIVE_AGE_YEARS() %>%
      relocate("AGE_DAYS", .after = "AGE")
  } else {
    DATA <- DATA_DM %>%
      convert_blanks_to_na() %>%
      DERIVE_AGE_YEARS()
  }

  DATA <- DATA[, which(names(DATA) %in% COLUMNS)]

  DATA <- DATA %>%
    rename("TREATMENT" = "ARM")

  if ("ETHNIC" %in% names(DATA)) {
    DATA <- DATA %>%
      mutate(ETHNIC = str_to_upper(.data$ETHNIC))
  }

  return(DATA)
}
