#' Prepare the VS domain for analysis on the first occurrence of events.
#'
#' Prepare the Vital Signs (VS) domain for use in first occurrence analysis data
#' sets. Takes a IDDO-SDTM curated VS domain, transforms and pivots it in order
#' to merge it into a first occurrence analysis data set with other domains
#' using the ANALYSE_FIRST() function.
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
#' @return Data frame containing a row per USUBJID/subject, with VSTESTCDs and the
#'   day of first occurrence of each as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_VS_FIRST = function(DATA_VS, DISEASE = "", VARS = NULL){
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
    mutate(VSSTRES = as.character(.data$VSSTRESN),
           VSSTRESC = as.character(.data$VSSTRESC),
           VSORRES = as.character(.data$VSORRES),
           DAY = .data$VSDY)

  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] =
    DATA[which(is.na(DATA$VSSTRES)), "VSSTRESC"]
  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] =
    DATA[which(is.na(DATA$VSSTRES)), "VSORRES"]

  DATA = DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$VISITDY, DATA$DAY), ]

  DATA = DATA %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$VSTESTCD,
                names_glue = "{VSTESTCD}_{.value}", values_from = c(.data$VSSTRES, .data$DAY),
                names_sort = T, names_vary = "slowest",
                values_fn = first)

  DATA = DATA %>%
    clean_names(case = "all_caps")

  colnames(DATA) = gsub("_VSSTRES", "", colnames(DATA))

  return(DATA)
}
