#' Prepare the Temperature variable in the VS domain for baseline analysis.
#'
#' Prepare the Temperature variable in the Vital Signs (VS) domain for use in
#' baseline analysis data sets. Takes a IDDO-SDTM curated VS domain, filters
#' just the temperature records, transforms and pivots it in order to merge it
#' into a baseline analysis data set with other domains using the
#' ANALYSE_BASELINE() function.
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
PREP_VS_TEMP_BL = function(DATA_VS){
  DATA = DATA_VS %>%
    convert_blanks_to_na() %>%
    filter(VSTESTCD == "TEMP") %>%
    DERIVE_TIMING() %>%
    mutate(VSSTRES = as.character(VSSTRESN),
           VSSTRESC = as.character(VSSTRESC),
           VSORRES = as.character(VSORRES))

  DATA = DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$VISITDY, DATA$VSDY), ]

  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] =
    DATA[which(is.na(DATA$VSSTRES)), "VSSTRESC"]
  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] =
    DATA[which(is.na(DATA$VSSTRES)), "VSORRES"]

  DATA = DATA %>%
    filter(TIMING == 1 | TIMING == "BASELINE") %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID), names_from = VSTESTCD,
                values_from = c(VSSTRES, VSLOC),
                names_sort = T, names_vary = "slowest",
                values_fn = first, names_glue = "{.value}_TEMP")

  DATA = DATA %>%
    rename("TEMP" = "VSSTRES_TEMP",
           "TEMP_LOC" = "VSLOC_TEMP") %>%
    clean_names(case = "all_caps")

  return(DATA)
}
