#' Further prepare the MB domain for baseline analysis specifically for VL.
#'
#' Prepare the Microbiology (MB) domain for use in baseline analysis data sets
#' with specific actions for Visceral Leishmaniasis (VL). Takes a IDDO-SDTM
#' curated MB domain, transforms and pivots it in order to merge it into a
#' baseline analysis data set with other domains using the ANALYSE_BASELINE()
#' function. PREP_MB_BL() and PREP_MB_VL_BL() would be merged in the
#' ANALYSE_BASELINE() function.
#'
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#'
#' @return Data frame with one row per USUBJID/subject, with VL specific
#'   MBTESTCDs as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MB_BL_VL = function(DATA_MB){
  MB_VARS = c("LSHMANIA", "LDONOV", "LMAJOR")

  DATA = DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(.data$MBTESTCD %in% MB_VARS) %>%
    DERIVE_TIMING() %>%
    CLEAN_MB_VL() %>%
    mutate(MBSTRES = as.character(.data$MBSTRESN),
           MBUNITS = as.character(.data$MBSTRESU),
           MBSTRESC = as.character(.data$MBSTRESC),
           MBMODIFY = as.character(.data$MBMODIFY),
           MBORRES = as.character(.data$MBORRES),
           MBUNITS = as.character(NA))

  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBSTRESC"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBMODIFY"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBORRES"]

  DATA[which(!is.na(DATA$MBSTRESC) | !is.na(DATA$MBSTRESN)), "MBUNITS"] =
    DATA[which(!is.na(DATA$MBSTRESC) | !is.na(DATA$MBSTRESN)), "MBSTRESU"]
  DATA[which(is.na(DATA$MBSTRESC) & is.na(DATA$MBSTRESN)), "MBUNITS"] =
    DATA[which(is.na(DATA$MBSTRESC) & is.na(DATA$MBSTRESN)), "MBORRESU"]

  DATA$MBSTRES = str_replace_all(DATA$MBSTRES, "01-Oct", "1-10")
  DATA$MBSTRES = str_replace_all(DATA$MBSTRES, "01-OCT", "1-10")

  DATA = DATA %>%
    mutate(MBSTRES = str_to_upper(.data$MBSTRES),
           MBUNITS = str_to_upper(.data$MBUNITS)) %>%
    filter(.data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$MBTESTCD,
                names_glue = "{MBTESTCD}_{.value}",
                values_from = c(.data$MBSTRES, .data$MBUNITS, .data$MBLOC, .data$MBSPEC),
                names_sort = T, names_vary = "slowest",
                values_fn = first)

  colnames(DATA) = gsub("_MBSTRES", "", colnames(DATA))
  colnames(DATA) = gsub("_MBUNITS", "_UNITS", colnames(DATA))
  colnames(DATA) = gsub("MBLOC", "LOC", colnames(DATA))
  colnames(DATA) = gsub("MBSPEC", "SPEC", colnames(DATA))

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
