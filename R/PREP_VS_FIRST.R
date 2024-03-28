#' Prepare the VS domain for analysis on the first occurrence of events.
#'
#' Prepare the Vital Signs (VS) domain for use in first occurrence analysis data
#' sets. Takes a IDDO-SDTM curated VS domain, transforms and pivots it in order
#' to merge it into a first occurrence analysis data set with other domains
#' using the ANALYSE_FIRST() function. Default variables are: "WEIGHT", "HEIGHT",
#' "MUARMCIR", "BMI", "DIABP", "HR", "PULSE", "RESP", "SYSBP". Ebola specific
#' variables are listed in 'Details'
#'
#' Ebola default variables: "RESP", "HR", "SYSBP", "DIABP"
#'
#' @param DATA_VS The VS domain data frame, as named in the global environment.
#' @param DISEASE The name of the disease theme being analysed. Character
#'   string. Default is empty (selects base variables). Select from: "MALARIA",
#'   "VL" or "EBOLA". If selection is missing or misspelt, then the base
#'   variables will be used.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for VSTESTCD as
#'   specified in the VS section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("MAP").
#'
#' @return Data frame containing a row per USUBJID/subject, with VSTESTCDs and the
#'   day of first occurrence of each as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_VS_FIRST <- function(DATA_VS, DISEASE = "", VARS = NULL) {
  DISEASE <- str_to_upper(DISEASE)

  if (DISEASE == "EBOLA") {
    VS_VARS <- c("RESP", "HR", "SYSBP", "DIABP", str_to_upper(VARS))
  } else {
    VS_VARS <- c(
      "WEIGHT", "HEIGHT", "MUARMCIR", "BMI", "DIABP", "HR",
      "PULSE", "RESP", "SYSBP", str_to_upper(VARS)
    )
  }

  DATA <- DATA_VS %>%
    convert_blanks_to_na() %>%
    filter(.data$VSTESTCD %in% VS_VARS) %>%
    mutate(
      VSSTRES = as.character(.data$VSSTRESN),
      VSSTRESC = as.character(.data$VSSTRESC),
      VSORRES = as.character(.data$VSORRES),
      DAY = .data$VSDY,
      VSUNITS = as.character(NA)
    )

  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] <-
    DATA[which(is.na(DATA$VSSTRES)), "VSSTRESC"]
  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] <-
    DATA[which(is.na(DATA$VSSTRES)), "VSORRES"]

  DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSUNITS"] <-
    DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSSTRESU"]
  DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSUNITS"] <-
    DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSORRESU"]

  DATA <- DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$VISITDY, DATA$DAY), ]

  DATA <- DATA %>%
    pivot_wider(
      id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$VSTESTCD,
      names_glue = "{VSTESTCD}_{.value}", values_from = c(.data$VSSTRES, .data$VSUNITS, .data$DAY),
      names_sort = T, names_vary = "slowest",
      values_fn = first
    )

  colnames(DATA) <- gsub("_VSSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("VSUNITS", "UNITS", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  colnames(DATA) <- gsub("_VSSTRES", "", colnames(DATA))

  return(DATA)
}
