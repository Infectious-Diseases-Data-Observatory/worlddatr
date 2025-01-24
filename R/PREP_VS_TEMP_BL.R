#' Prepare the Temperature variable in the VS domain for baseline analysis.
#'
#' Prepare the Temperature variable in the Vital Signs (VS) domain for use in
#' baseline analysis data sets. Takes a IDDO-SDTM curated VS domain, filters
#' just the temperature records, transforms and pivots it in order to merge it
#' into a baseline analysis data set with other domains using the
#' ANALYSE_BASELINE() function.
#'
#' This allows the Temperature location to be recorded in the analysis dataset
#' without including the location for all of the other VS variables
#'
#' @param DATA_VS The VS domain data frame, as named in the global environment.
#'
#' @return Data frame with one row per USUBJID/subject, with TEMP, TEMP_UNITS
#'   and TEMP_LOC as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_VS_TEMP_BL <- function(DATA_VS) {
  DATA <- DATA_VS %>%
    convert_blanks_to_na() %>%
    filter(.data$VSTESTCD == "TEMP") %>%
    DERIVE_TIMING() %>%
    mutate(
      VSSTRES = str_to_upper(as.character(.data$VSSTRESN)),
      VSSTRESC = str_to_upper(as.character(.data$VSSTRESC)),
      VSORRES = str_to_upper(as.character(.data$VSORRES)),
      VSUNITS = as.character(NA)
    )

  DATA <- DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$VISITDY, DATA$VSDY), ]

  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] <-
    DATA[which(is.na(DATA$VSSTRES)), "VSSTRESC"]
  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] <-
    DATA[which(is.na(DATA$VSSTRES)), "VSORRES"]

  DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSUNITS"] <-
    DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSSTRESU"]
  DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSUNITS"] <-
    DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSORRESU"]

  DATA <- DATA %>%
    filter(.data$TIMING == "1" | .data$TIMING == "BASELINE") %>%
    pivot_wider(
      id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$VSTESTCD,
      values_from = c(.data$VSSTRES, .data$VSUNITS, .data$VSLOC),
      names_sort = T, names_vary = "slowest",
      values_fn = first, names_glue = "{VSTESTCD}_{.value}"
    )

  colnames(DATA) <- gsub("_VSSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("VSUNITS", "UNITS", colnames(DATA))
  colnames(DATA) <- gsub("VSLOC", "LOC", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
