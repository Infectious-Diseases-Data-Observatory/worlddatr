#' Prepare the MB domain for follow up analysis.
#'
#' Prepare the Microbiology (MB) domain for use in follow up analysis data sets.
#' Takes a IDDO-SDTM curated MB domain, transforms and pivots it in order to
#' merge it into a follow up analysis data set with other domains using the
#' ANALYSE_FOLLOW_UP() function. Default variables are: "HIV", "AFB", "MTB",
#' "ANCDUOD", "ANCLMTA", "ASCLUM". Disease specific options are listed in
#' 'Details'.
#'
#' VL: "HIV", "AFB", "MTB", "ANCDUOD", "ANCLMTA", "ASCLUM", "PLSMDM", "PLSMDMA",
#' "PLSMDMS", "PFALCIP", "PFALCIPA", "PFALCIPS", "PVIVAX", "PVIVAXA", "PVIVAXS"
#'
#' Ebola: "ZEBOV"
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
#' @return Data frame with one row per USUBJID/subject per day, with MBTESTCDs
#'   and the units as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MB_FU = function(DATA_MB, DISEASE = "", VARS = NULL){
  DISEASE = str_to_upper(DISEASE)

  if(DISEASE == "VL"){
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

  DATA_MB = DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(.data$MBTESTCD %in% MB_VARS) %>%
    mutate(MBSTRES = as.character(.data$MBSTRESN),
           MBSTRESC = as.character(.data$MBSTRESC),
           MBMODIFY = as.character(.data$MBMODIFY),
           MBORRES = as.character(.data$MBORRES),
           DAY = .data$MBDY,
           MBUNITS = as.character(NA))

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

  DATA[which(!is.na(DATA$MBSTRESC) | !is.na(DATA$MBSTRESN)), "MBUNITS"] =
    DATA[which(!is.na(DATA$MBSTRESC) | !is.na(DATA$MBSTRESN)), "MBSTRESU"]
  DATA[which(is.na(DATA$MBSTRESC) & is.na(DATA$MBSTRESN)), "MBUNITS"] =
    DATA[which(is.na(DATA$MBSTRESC) & is.na(DATA$MBSTRESN)), "MBORRESU"]

  DATA = DATA %>%
    mutate(MBSTRES = str_to_upper(.data$MBSTRES)) %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID, .data$VISITDY,
                            .data$VISITNUM, .data$DAY, .data$EMPTY_TIME),
                names_from = .data$MBTESTCD, values_from = c(.data$MBSTRES, .data$MBUNITS),
                names_sort = T, names_vary = "slowest",
                values_fn = first, names_glue = "{MBTESTCD}_{.value}")

  colnames(DATA) = gsub("_MBSTRES", "", colnames(DATA))
  colnames(DATA) = gsub("MBUNITS", "UNITS", colnames(DATA))

  DATA = DATA %>%
    clean_names(case = "all_caps")

  if("AFB" %in% names(DATA) | "MTB" %in% names(DATA)){
    if("AFB" %in% names(DATA) & "MTB" %in% names(DATA)){
      DATA = DATA %>%
        mutate(TB = .data$MTB,
               TB_UNITS = .data$MTB_UNITS,
               MB_IND = NA)

      DATA[which(!is.na(DATA$MTB)), "MB_IND"] = "MTB"

      DATA[which(is.na(DATA$MB_IND)), "TB"] =
        DATA[which(is.na(DATA$MB_IND)), "AFB"]

      DATA[which(is.na(DATA$MB_IND)), "TB_UNITS"] =
        DATA[which(is.na(DATA$MB_IND)), "AFB_UNITS"]

      DATA = DATA %>%
        dplyr::select(-"AFB", -"MTB",
                      -"AFB_UNITS", -"MTB_UNITS",
                      -"MB_IND")
    }
    else if("AFB" %in% names(DATA) & "MTB" %!in% names(DATA)){
      DATA = DATA %>%
        rename("TB" = "AFB",
               "TB_UNITS" = "AFB_UNITS")
    }
    else if("AFB" %!in% names(DATA) & "MTB" %in% names(DATA)){
      DATA = DATA %>%
        rename("TB" = "MTB",
               "TB_UNITS" = "MTB_UNITS")
    }
  }

  return(DATA)
}
