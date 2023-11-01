#' Prepare the VS domain for follow up analysis.
#'
#' Prepare the Vital Signs (VS) domain for use in follow up
#' analysis data sets. Takes a IDDO-SDTM curated VS domain, transforms and
#' pivots it in order to merge it into a follow up analysis data set with other
#' domains using the ANALYSE_FOLLOW_UP() function.
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
#' @return Data frame with one row per USUBJID/subject, with VSTESTCDs as
#'   columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_VS_FU = function(DATA_VS, DISEASE = "", VARS = NULL){
  DISEASE = str_to_upper(DISEASE)

  if(DISEASE == "MALARIA"){
    VS_VARS = c("WEIGHT", "HEIGHT", "MUARMCIR", "BMI", "DIABP", "HR",
                "PULSE", "RESP", "SYSBP", str_to_upper(VARS))
  }

  else if(DISEASE == "VL"){
    VS_VARS = c("WEIGHT", "HEIGHT", "MUARMCIR", "BMI", "DIABP", "HR",
                "PULSE", "RESP", "SYSBP", str_to_upper(VARS))
  }

  else if(DISEASE == "EBOLA"){
    VS_VARS = c("TEMP", "RESP", "HR", "SYSBP", "DIABP", str_to_upper(VARS))
  }

  else{
    VS_VARS = c("WEIGHT", "HEIGHT", "MUARMCIR", "BMI", "DIABP", "HR",
                "PULSE", "RESP", "SYSBP", str_to_upper(VARS))
  }

  DATA_VS = DATA_VS %>%
    convert_blanks_to_na() %>%
    filter(VSTESTCD %in% VS_VARS) %>%
    mutate(VSSTRES = as.character(VSSTRESN),
           VSSTRESC = as.character(VSSTRESC),
           VSORRES = as.character(VSORRES),
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
    mutate(VSSTRES = str_to_upper(VSSTRES)) %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID, VISITDY, VISITNUM, DAY, EMPTY_TIME), names_from = VSTESTCD,
                values_from = VSSTRES, names_sort = T,
                names_vary = "slowest", values_fn = first)

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
