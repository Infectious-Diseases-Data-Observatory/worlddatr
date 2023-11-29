#' Prepare the SA domain for baseline analysis.
#'
#' Prepare the Clinical and Adverse Effects (SA) domain for use in baseline
#' analysis data sets. Takes a IDDO-SDTM curated SA domain, transforms and
#' pivots it in order to merge it into a baseline analysis data set with other
#' domains using the ANALYSE_BASELINE() function.
#'
#' @param DATA_SA The SA domain data frame, as named in the global environment.
#' @param DISEASE The name of the disease theme being analysed. Character
#'   string. Default is empty (selects base variables). Select from: "MALARIA",
#'   "VL" or "EBOLA". If selection is missing or misspelt, then the default
#'   variables will be used.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for SADECOD as
#'   specified in the SA section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CRONAVIR").
#' @param inc_DUR Should the analysis dataset include the duration of the event?
#'   This is the time from the start of the event till the end. Boolean, default
#'   is FALSE.
#' @param inc_TIME Should the analysis dataset include the time since the event?
#'   This is the time since the end of the event. Boolean, default is FALSE.
#'
#' @return Data frame with one row per USUBJID/subject, with SATERMs as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_SA_BL = function(DATA_SA, DISEASE = "", VARS = NULL, inc_DUR = FALSE, inc_TIME = FALSE){
  DISEASE = str_to_upper(DISEASE)

  if(DISEASE == "MALARIA"){
    SA_VARS = c("FEVER", "ANEMIA", "HIV", "ANOREXIA", "DIARRHOEA", "NAUSEA", "VOMITING",
                "ABDOMINAL PAIN", "DIZZINESS", "SHORTNESS OF BREATH", "JAUNDICE",
                "DARK URINE", "ENLARGED SPLEEN", "ENLARGED LIVER", str_to_upper(VARS))
  }

  else if(DISEASE == "VL"){
    SA_VARS = c("FEVER", "ANEMIA", "HIV", str_to_upper(VARS))
  }

  else if(DISEASE == "EBOLA"){
    SA_VARS = c("FEVER", "LOSS OF APPETITE", "VOMITING", "NAUSEA AND VOMITING","HEADACHE",
                "DIARRHOEA", "ABDOMINAL PAIN", "BLEEDING", "DIFFICULTY SWALLOWING", "HICCOUGHS",
                "DIFFICULTY BREATHING", "PAIN IN THROAT", "FATIGUE", "MUSCLE PAIN",
                "JOINT PAIN", "GENERALIZED ACHES AND PAIN", "ERUPTION OF SKIN", str_to_upper(VARS))
  }

  else{
    SA_VARS = c("FEVER", "ANEMIA", "HIV", str_to_upper(VARS))
  }

  DATA_SA = DATA_SA %>%
    convert_blanks_to_na() %>%
    mutate(SASTRES = str_to_upper(.data$SADECOD),
           SAMODIFY = as.character(.data$SAMODIFY),
           SATERM = as.character(.data$SATERM))

  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] =
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SAMODIFY"]
  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] =
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SATERM"]

  DATA_SA = DATA_SA %>%
    filter(.data$SASTRES %in% SA_VARS) %>%
    DERIVE_TIMING() %>%
    mutate(SAPRESP = str_to_upper(.data$SAPRESP),
           SAOCCUR = str_to_upper(.data$SAOCCUR))

  DATA_SA$SAPRESP = str_replace_all(DATA_SA$SAPRESP, "TRUE", "Y")
  DATA_SA$SAOCCUR = str_replace_all(DATA_SA$SAOCCUR, "TRUE", "Y")
  DATA_SA$SAOCCUR = str_replace_all(DATA_SA$SAOCCUR, "FALSE", "N")
  DATA_SA$SAOCCUR = str_replace_all(DATA_SA$SAOCCUR, "UNKNOWN", "U")

  if(inc_DUR == FALSE & inc_TIME == FALSE){
    if("SACAT" %in% names(DATA_SA)){
      DATA_HIST = DATA_SA %>%
        filter(.data$SACAT == "MEDICAL HISTORY") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                    names_from = .data$SASTRES, names_glue = "HISTORY_{SASTRES}_{.value}",
                    values_from = c(.data$SAOCCUR, .data$SAPRESP), names_vary = "slowest",
                    values_fn = first)

      DATA_SAE = DATA_SA %>%
        filter((.data$SACAT != "MEDICAL HISTORY" | is.na(.data$SACAT)) &
                 (.data$TIMING == 1 | .data$TIMING == "BASELINE")) %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                    names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
                    values_from = c(.data$SAOCCUR, .data$SAPRESP), names_vary = "slowest",
                    values_fn = first)

      DATA = full_join(DATA_HIST, DATA_SAE)
    }

    else{
      DATA = DATA_SA %>%
        filter(.data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                    names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
                    values_from = c(.data$SAOCCUR, .data$SAPRESP), names_vary = "slowest",
                    values_fn = first)
    }
  }

  if(inc_DUR == TRUE & inc_TIME == FALSE){
    DATA_SA = DATA_SA %>%
      mutate(SADUR = str_to_upper(SADUR))

    if("SACAT" %in% names(DATA_SA)){
      DATA_HIST = DATA_SA %>%
        filter(.data$SACAT == "MEDICAL HISTORY") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                    names_from = .data$SASTRES, names_glue = "HISTORY_{SASTRES}_{.value}",
                    values_from = c(.data$SAOCCUR, .data$SAPRESP, .data$SADUR),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA_SAE = DATA_SA %>%
        filter((.data$SACAT != "MEDICAL HISTORY" | is.na(.data$SACAT)) &
                 (.data$TIMING == 1 | .data$TIMING == "BASELINE")) %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                    names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
                    values_from = c(.data$SAOCCUR, .data$SAPRESP, .data$SADUR),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA = full_join(DATA_HIST, DATA_SAE)
    }

    else{
      DATA = DATA_SA %>%
        filter(.data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                    names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
                    values_from = c(.data$SAOCCUR, .data$SAPRESP, .data$SADUR),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)
    }
  }

  if(inc_DUR == FALSE & inc_TIME == TRUE){
    DATA_SA = DATA_SA %>%
      mutate(SAEVINTX = str_to_upper(SAEVINTX))

    if("SACAT" %in% names(DATA_SA)){
      DATA_HIST = DATA_SA %>%
        filter(SACAT == "MEDICAL HISTORY") %>%
        mutate(SAOCCUR = as.factor(SAOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = SASTRES, names_glue = "HISTORY_{SASTRES}_{.value}",
                    values_from = c(SAOCCUR, SAPRESP, SAEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA_SAE = DATA_SA %>%
        filter((SACAT != "MEDICAL HISTORY" | is.na(SACAT)) & (TIMING == 1 | TIMING == "BASELINE")) %>%
        mutate(SAOCCUR = as.factor(SAOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = SASTRES, names_glue = "{SASTRES}_{.value}",
                    values_from = c(SAOCCUR, SAPRESP, SAEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA = full_join(DATA_HIST, DATA_SAE)
    }

    else{
      DATA = DATA_SA %>%
        filter(TIMING == 1 | TIMING == "BASELINE") %>%
        mutate(SAOCCUR = as.factor(SAOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = SASTRES, names_glue = "{SASTRES}_{.value}",
                    values_from = c(SAOCCUR, SAPRESP, SAEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)
    }
  }

  if(inc_DUR == TRUE & inc_TIME == TRUE){
    DATA_SA = DATA_SA %>%
      mutate(SADUR = str_to_upper(SADUR),
             SAEVINTX = str_to_upper(SAEVINTX))

    if("SACAT" %in% names(DATA_SA)){
      DATA_HIST = DATA_SA %>%
        filter(SACAT == "MEDICAL HISTORY") %>%
        mutate(SAOCCUR = as.factor(SAOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = SASTRES, names_glue = "HISTORY_{SASTRES}_{.value}",
                    values_from = c(SAOCCUR, SAPRESP, SADUR, SAEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA_SAE = DATA_SA %>%
        filter((SACAT != "MEDICAL HISTORY" | is.na(SACAT)) & (TIMING == 1 | TIMING == "BASELINE")) %>%
        mutate(SAOCCUR = as.factor(SAOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = SASTRES, names_glue = "{SASTRES}_{.value}",
                    values_from = c(SAOCCUR, SAPRESP, SADUR, SAEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA = full_join(DATA_HIST, DATA_SAE)
    }

    else{
      DATA = DATA_SA %>%
        filter(TIMING == 1 | TIMING == "BASELINE") %>%
        mutate(SAOCCUR = as.factor(SAOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = SASTRES, names_glue = "{SASTRES}_{.value}",
                    values_from = c(SAOCCUR, SAPRESP, SADUR, SAEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)
    }
  }

  colnames(DATA) = gsub("_SAOCCUR", "", colnames(DATA))
  colnames(DATA) = gsub("SAPRESP", "PRESP", colnames(DATA))

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
