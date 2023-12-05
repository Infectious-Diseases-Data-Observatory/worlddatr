#' Prepare the MB domain for follow up analysis.
#'
#' Prepare the Microbiology (MB) domain for use in follow up
#' analysis data sets. Takes a IDDO-SDTM curated MB domain, transforms and
#' pivots it in order to merge it into a follow up analysis data set with other
#' domains using the ANALYSE_FOLLOW_UP() function.
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
#' @return Data frame with one row per USUBJID/subject per day, with MBTESTCDs as
#'   columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MB_FU = function(DATA_MB, DISEASE = "", VARS = NULL){
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

  else{
    MB_VARS = c("HIV", "AFB", "MTB", "ANCDUOD", "ANCLMTA", "ASCLUM",
                str_to_upper(VARS))
  }

  DATA_MB = DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(.data$MBTESTCD %in% MB_VARS) %>%
    mutate(MBSTRES = as.character(.data$MBSTRESN),
           MBSTRESC = as.character(.data$MBSTRESC),
           MBMODIFY = as.character(.data$MBMODIFY),
           MBORRES = as.character(.data$MBORRES),
           DAY = .data$MBDY)

  DATA_EMPTY = DATA_MB %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA = DATA_MB %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBSTRESC"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBMODIFY"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBORRES"]

  DATA = DATA %>%
    mutate(MBSTRES = str_to_upper(.data$MBSTRES)) %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID, .data$VISITDY,
                            .data$VISITNUM, .data$DAY, .data$EMPTY_TIME),
                names_from = .data$MBTESTCD, values_from = .data$MBSTRES,
                names_sort = T, names_vary = "slowest",
                values_fn = first)

  DATA = DATA %>%
    clean_names(case = "all_caps")

  if("AFB" %in% names(DATA) | "MTB" %in% names(DATA)){
    if("AFB" %in% names(DATA) & "MTB" %in% names(DATA)){
      DATA = DATA %>%
        mutate(TB = .data$MTB)

      DATA[which(is.na(DATA$TB)), "TB"] =
        DATA[which(is.na(DATA$TB)), "AFB"]

      DATA = DATA %>%
        dplyr::select(-"AFB", -"MTB")
    }
    else if("AFB" %in% names(DATA) & "MTB" %!in% names(DATA)){
      DATA = DATA %>%
        rename("TB" = "AFB")
    }
    else if("AFB" %!in% names(DATA) & "MTB" %in% names(DATA)){
      DATA = DATA %>%
        rename("TB" = "MTB")
    }
  }

  return(DATA)
}
