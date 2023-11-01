#' Prepare the MB domain for analysis on the first occurrence of events.
#'
#' Prepare the Microbiology (MB) domain for use in first occurrence
#' analysis data sets. Takes a IDDO-SDTM curated MB domain, transforms and
#' pivots it in order to merge it into a first occurrence analysis data set with other
#' domains using the ANALYSE_FIRST() function.
#'
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#' @param DISEASE The name of the disease theme being analysed. Character
#'   string. Default is empty (selects base variables). Select from: "MALARIA",
#'   "VL" or "EBOLA". If selection is missing or misspelt, then the default
#'   variables will be used.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for MBTESTCD as
#'   specified in the MB section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CRONAVIR").
#'
#' @return Data frame containing a row per USUBJID/subject, with MBTESTCDs and the
#'   day of first occurrence of each as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MB_FIRST = function(DATA_MB, DISEASE = "", VARS = NULL){
  DISEASE = str_to_upper(DISEASE)

  if(DISEASE == "MALARIA"){
    MB_VARS = c("HIV", "AFB", "MTB", "ANCDUOD", "ANCLMTA", "ASCLUM",
                str_to_upper(VARS))
  }

  else if(DISEASE == "VL"){
    MB_VARS = c("HIV", "AFB", "MTB", "ANCDUOD", "ANCLMTA", "ASCLUM",
                "PLSMDM", "PLSMDMA", "PLSMDMS",
                "PFALCIP", "PFALCIPA", "PFALCIPS",
                "PVIVAX", "PVIVAXA", "PVIVAXS",
                str_to_upper(VARS))
  }

  else if(DISEASE == "EBOLA"){
    MB_VARS = c("ZEBOV", str_to_upper(VARS))
  }

  else{
    MB_VARS = c("HIV", "AFB", "MTB", "ANCDUOD", "ANCLMTA", "ASCLUM",
                str_to_upper(VARS))
  }

  DATA = DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(MBTESTCD %in% MB_VARS) %>%
    mutate(MBSTRES = as.character(MBSTRESN),
           MBSTRESC = as.character(MBSTRESC),
           MBORRES = as.character(MBORRES),
           DAY = MBDY)

  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBSTRESC"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBORRES"]

  DATA = DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$VISITDY, DATA$DAY), ]

  DATA = DATA %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID), names_from = MBTESTCD, names_glue = "{MBTESTCD}_{.value}",
                values_from = c(MBSTRES, DAY),
                names_sort = T, names_vary = "slowest",
                values_fn = first)

  DATA = DATA %>%
    clean_names(case = "all_caps")

  colnames(DATA) = gsub("_MBSTRES", "", colnames(DATA))

  return(DATA)
}
