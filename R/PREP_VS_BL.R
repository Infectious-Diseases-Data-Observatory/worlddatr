#' Prepare the VS domain for baseline analysis.
#'
#' Prepare the Vital Signs (VS) domain for use in baseline
#' analysis data sets. Takes a IDDO-SDTM curated VS domain, transforms and
#' pivots it in order to merge it into a baseline analysis data set with other
#' domains using the ANALYSE_BASELINE() function.
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
PREP_VS_BL = function(DATA_VS, DISEASE = "", VARS = NULL){
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

  DATA = DATA_VS %>%
    convert_blanks_to_na() %>%
    filter(.data$VSTESTCD %in% VS_VARS) %>%
    DERIVE_TIMING() %>%
    mutate(VSSTRES = as.character(.data$VSSTRESN),
           VSSTRESC = as.character(.data$VSSTRESC),
           VSORRES = as.character(.data$VSORRES),
           VSUNITS = as.character(NA))

  DATA = DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$VISITDY, DATA$VSDY), ]

  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] =
    DATA[which(is.na(DATA$VSSTRES)), "VSSTRESC"]
  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] =
    DATA[which(is.na(DATA$VSSTRES)), "VSORRES"]

  DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSUNITS"] =
    DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSSTRESU"]
  DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSUNITS"] =
    DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSORRESU"]

  DATA = DATA %>%
    filter(.data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$VSTESTCD,
                values_from = c(.data$VSSTRES, .data$VSUNITS),
                names_sort = T, names_vary = "slowest",
                values_fn = first, names_glue = "{VSTESTCD}_{.value}")

  colnames(DATA) = gsub("_VSSTRES", "", colnames(DATA))
  colnames(DATA) = gsub("VSUNITS", "UNITS", colnames(DATA))

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
