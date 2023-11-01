#' Prepare the Temperature variable in the VS domain for follow up analysis.
#'
#' Prepare the Temperature variable in the Vital Signs (VS) domain for use in
#' follow up analysis data sets. Takes a IDDO-SDTM curated VS domain, filters
#' just the temperature records, transforms and pivots it in order to merge it
#' into a follow up analysis data set with other domains using the
#' ANALYSE_FOLLOW_UP() function.
#'
#' This allows the Temperature location to be
#' recorded in the analysis dataset without including the location for all of
#' the other VS variables
#'
#' @param DATA_VS The VS domain data frame, as named in the global environment.
#'
#' @return Data frame with one row per USUBJID/subject, with VSTESTCDs as
#'   columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_VS_TEMP_FU = function(DATA_VS){
  DATA_VS = DATA_VS %>%
    convert_blanks_to_na() %>%
    filter(VSTESTCD == "TEMP") %>%
    mutate(VSSTRES = as.character(VSSTRESN),
           VSSTRESC = as.character(VSSTRESC),
           VSORRES = as.character(VSORRES),
           VISIT = as.character(VISIT),
           DAY = VSDY)

  DATA_EMPTY = DATA_VS %>%
    filter(is.na(VISITDY) & is.na(VISITNUM) & is.na(DAY)) %>%
    DERIVE_EMPTY_TIME()


  DATA = DATA_VS %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] =
    DATA[which(is.na(DATA$VSSTRES)), "VSSTRESC"]
  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] =
    DATA[which(is.na(DATA$VSSTRES)), "VSORRES"]

  DATA = DATA %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID, VISITDY, VISITNUM, DAY, EMPTY_TIME), names_from = VSTESTCD,
                values_from = c(VSSTRES, VSLOC),
                names_sort = T, names_vary = "slowest",
                values_fn = first)

  DATA = DATA %>%
    rename("TEMP" = "VSSTRES_TEMP",
           "TEMP_LOC" = "VSLOC_TEMP") %>%
    clean_names(case = "all_caps")

  return(DATA)
}
