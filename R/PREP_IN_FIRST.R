#' Prepare the IN domain for analysis on the first occurrence of events.
#'
#' Prepare the Treatments and Interventions (IN) domain for use in first occurrence
#' analysis data sets. Takes a IDDO-SDTM curated IN domain, transforms and
#' pivots it in order to merge it into a first occurrence analysis data set with other
#' domains using the ANALYSE_FIRST() function.
#'
#' @param DATA_IN The IN domain data frame, as named in the global environment.
#' @param DISEASE The name of the disease theme being analysed. Character
#'   string. Default is empty (selects base variables). Select from: "MALARIA",
#'   "VL" or "EBOLA". If selection is missing or misspelt, then the base
#'   variables will be used.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for INDECOD as
#'   specified in the IN section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("PARACETAMOL").
#'
#' @return Dataframe containing a row per USUBJID/subject, with IN terms and the
#'   day of first occurrence of each as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_IN_FIRST = function(DATA_IN, DISEASE = "", VARS = NULL){
  DISEASE = str_to_upper(DISEASE)

  if(DISEASE == "MALARIA"){
    VS_VARS = c(str_to_upper(VARS))
  }

  else if(DISEASE == "VL"){
    VS_VARS = c(str_to_upper(VARS))
  }

  else if(DISEASE == "EBOLA"){
    IN_VARS = c("MULTIVITAMINS, PLAIN", "VITAMIN A", "ANTIBIOTICS", "ANTIMALARIALS",
                "CEPHALOSPORINS", "INTRAVENOUS FLUIDS", str_to_upper(VARS))
  }

  else{
    VS_VARS = c(str_to_upper(VARS))
  }

  DATA = DATA_IN %>%
    convert_blanks_to_na() %>%
    mutate(INTRT = str_to_upper(INTRT),
           INSTRES = as.character(INDECOD),
           INPRESP = str_to_upper(INPRESP),
           INOCCUR = str_to_upper(INOCCUR),
           DAY = INDY) %>%
    filter((INCAT != "MEDICAL HISTORY" | is.na(INCAT)),
           INPRESP == "Y")

  DATA[which(is.na(DATA$INSTRES)), "INSTRES"] =
    DATA[which(is.na(DATA$INSTRES)), "INTRT"]

  DATA = DATA %>%
    filter(INSTRES %in% IN_VARS)

  DATA = DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$VISITDY, DATA$DAY), ]

  DATA = DATA %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID), names_from = INSTRES, names_glue = "{INSTRES}_{.value}",
                values_from = c(INOCCUR, DAY),
                names_sort = T, names_vary = "slowest",
                values_fn = first)

  DATA = DATA %>%
    clean_names(case = "all_caps")

  colnames(DATA) = gsub("_INOCCUR", "", colnames(DATA))
  colnames(DATA) = gsub("_INPRESP", "_PRESP", colnames(DATA))

  return(DATA)
}
