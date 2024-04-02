#' Prepare the Temperature variable in the VS domain for follow up analysis.
#'
#' Prepare the Temperature variable in the Vital Signs (VS) domain for use in
#' follow up analysis data sets. Takes a IDDO-SDTM curated VS domain, filters
#' just the temperature records, transforms and pivots it in order to merge it
#' into a follow up analysis data set with other domains using the
#' ANALYSE_FOLLOW_UP() function.
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
PREP_VS_TEMP_FU <- function(DATA_VS) {
  DATA_VS <- DATA_VS %>%
    convert_blanks_to_na() %>%
    filter(.data$VSTESTCD == "TEMP") %>%
    mutate(
      VSSTRES = as.character(.data$VSSTRESN),
      VSSTRESC = as.character(.data$VSSTRESC),
      VSORRES = as.character(.data$VSORRES),
      VISIT = as.character(.data$VISIT),
      DAY = .data$VSDY,
      VSUNITS = as.character(NA)
    )

  DATA_EMPTY <- DATA_VS %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()


  DATA <- DATA_VS %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] <-
    DATA[which(is.na(DATA$VSSTRES)), "VSSTRESC"]
  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] <-
    DATA[which(is.na(DATA$VSSTRES)), "VSORRES"]

  DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSUNITS"] <-
    DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSSTRESU"]
  DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSUNITS"] <-
    DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSORRESU"]

  DATA <- DATA %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID, .data$VISITDY, .data$VISITNUM,
        .data$DAY, .data$EMPTY_TIME
      ), names_from = .data$VSTESTCD,
      values_from = c(.data$VSSTRES, .data$VSUNITS, .data$VSLOC),
      names_sort = T, names_vary = "slowest", names_glue = "{VSTESTCD}_{.value}",
      values_fn = first
    )

  colnames(DATA) <- gsub("_VSSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("VSUNITS", "UNITS", colnames(DATA))
  colnames(DATA) <- gsub("VSLOC", "LOC", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
