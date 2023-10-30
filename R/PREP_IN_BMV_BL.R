#' Further prepare the IN domain for baseline analysis, specifically for Blood
#' Transfusion, Malaria and VL events.
#'
#' Prepare the Treatments and Interventions (IN) domain for use in baseline
#' analysis data sets focusing on Malaria and Visceral Leishmaniasis. Takes a
#' IDDO-SDTM curated IN domain, transforms and pivots it in order to merge it
#' into a baseline analysis data set with other domains using the
#' ANALYSE_BASELINE() function. PREP_IN_BL() and PREP_IN_BMV_BL() would be
#' merged in the ANALYSE_BASELINE() function.
#'
#' @param DATA_IN The IN domain data frame, as named in the global environment.
#' @param inc_DUR Should the analysis dataset include the duration of the event?
#'   This is the time from the start of the event till the end. Boolean, default
#'   is FALSE.
#' @param inc_TIME Should the analysis dataset include the time since the event?
#'   This is the time since the end of the event. Boolean, default is FALSE.
#'
#' @return Dataframe containing a row per USUBJID/subject, with IN terms as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_IN_BMV_BL = function(DATA_IN, inc_DUR = FALSE, inc_TIME = FALSE){
  DATA_IN = DATA_IN %>%
    convert_blanks_to_na() %>%
    mutate(INSTRES = str_to_upper(INDECOD),
           INMODIFY = as.character(INMODIFY),
           INTRT = as.character(INTRT)) %>%
    CLEAN_IN()

  DATA_IN[which(is.na(DATA_IN$INSTRES)), "INSTRES"] =
    DATA_IN[which(is.na(DATA_IN$INSTRES)), "INMODIFY"]
  DATA_IN[which(is.na(DATA_IN$INSTRES)), "INSTRES"] =
    DATA_IN[which(is.na(DATA_IN$INSTRES)), "INTRT"]

  DATA_IN = DATA_IN %>%
    filter(INSTRES %in% c("BLOOD_TRANSFUSION", "MALARIA", "VL")) %>%
    DERIVE_TIMING()

  if(any(is.na(DATA_IN$INPRESP))) {
    DATA_IN[which(is.na(DATA_IN$INPRESP)), "INPRESP"] = "N"
    DATA_IN[which(DATA_IN$INPRESP == "N"), "INOCCUR"] = "Y"
  }

  if(inc_DUR == FALSE & inc_TIME == FALSE){
    if("INCAT" %in% names(DATA_IN)){
      DATA_HIST = DATA_IN %>%
        filter(INCAT == "MEDICAL HISTORY") %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "HISTORY_{INSTRES}_{.value}",
                    values_from = INOCCUR,
                    values_fn = first)

      DATA_INT = DATA_IN %>%
        filter((INCAT != "MEDICAL HISTORY" | is.na(INCAT)) & (TIMING == 1 | TIMING == "BASELINE")) %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "{INSTRES}_{.value}",
                    values_from = INOCCUR,
                    values_fn = first)

      DATA = full_join(DATA_HIST, DATA_INT)
    }

    else{
      DATA = DATA_IN %>%
        filter(TIMING == 1 | TIMING == "BASELINE") %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "{INSTRES}_{.value}",
                    values_from = INOCCUR,
                    values_fn = first)
    }
  }

  if(inc_DUR == TRUE & inc_TIME == FALSE){
    DATA_IN = DATA_IN %>%
      mutate(INDUR = str_to_upper(INDUR))

    if("INCAT" %in% names(DATA_IN)){
      DATA_HIST = DATA_IN %>%
        filter(INCAT == "MEDICAL HISTORY") %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "HISTORY_{INSTRES}_{.value}",
                    values_from = c(INOCCUR, INDUR),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA_INT = DATA_IN %>%
        filter((INCAT != "MEDICAL HISTORY" | is.na(INCAT)) & (TIMING == 1 | TIMING == "BASELINE")) %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "{INSTRES}_{.value}",
                    values_from = c(INOCCUR, INDUR),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA = full_join(DATA_HIST, DATA_INT)
    }

    else{
      DATA = DATA_IN %>%
        filter(TIMING == 1 | TIMING == "BASELINE") %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "{INSTRES}_{.value}",
                    values_from = c(INOCCUR, INDUR),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)
    }
  }

  if(inc_DUR == FALSE & inc_TIME == TRUE){
    DATA_IN = DATA_IN %>%
      mutate(INEVINTX = str_to_upper(INEVINTX))

    if("INCAT" %in% names(DATA_IN)){
      DATA_HIST = DATA_IN %>%
        filter(INCAT == "MEDICAL HISTORY") %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "HISTORY_{INSTRES}_{.value}",
                    values_from = c(INOCCUR, INEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA_INT = DATA_IN %>%
        filter((INCAT != "MEDICAL HISTORY" | is.na(INCAT)) & (TIMING == 1 | TIMING == "BASELINE")) %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "{INSTRES}_{.value}",
                    values_from = c(INOCCUR, INEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA = full_join(DATA_HIST, DATA_INT)
    }

    else{
      DATA = DATA_IN %>%
        filter(TIMING == 1 | TIMING == "BASELINE") %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "{INSTRES}_{.value}",
                    values_from = c(INOCCUR, INEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)
    }
  }

  if(inc_DUR == TRUE & inc_TIME == TRUE){
    DATA_IN = DATA_IN %>%
      mutate(INDUR = str_to_upper(INDUR),
             INEVINTX = str_to_upper(INEVINTX))

    if("INCAT" %in% names(DATA_IN)){
      DATA_HIST = DATA_IN %>%
        filter(INCAT == "MEDICAL HISTORY") %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "HISTORY_{INSTRES}_{.value}",
                    values_from = c(INOCCUR, INDUR, INEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA_INT = DATA_IN %>%
        filter((INCAT != "MEDICAL HISTORY" | is.na(INCAT)) & (TIMING == 1 | TIMING == "BASELINE")) %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "{INSTRES}_{.value}",
                    values_from = c(INOCCUR, INDUR, INEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)

      DATA = full_join(DATA_HIST, DATA_INT)
    }

    else{
      DATA = DATA_IN %>%
        filter(TIMING == 1 | TIMING == "BASELINE") %>%
        mutate(INOCCUR = as.factor(INOCCUR)) %>%
        pivot_wider(id_cols = c(STUDYID, USUBJID),
                    names_from = INSTRES, names_glue = "{INSTRES}_{.value}",
                    values_from = c(INOCCUR, INDUR, INEVINTX),
                    names_sort = T, names_vary = "slowest",
                    values_fn = first)
    }
  }

  if("BLOOD_TRANSFUSION_INOCCUR" %in% names(DATA)){
    DATA = DATA %>%
      rename("BLOOD_TRANSFUSION" = "BLOOD_TRANSFUSION_INOCCUR")
  }
  if("BLOOD_TRANSFUSION_INDUR" %in% names(DATA)){
    DATA = DATA %>%
      dplyr::select(-BLOOD_TRANSFUSION_INDUR)
  }
  if("BLOOD_TRANSFUSION_INEVINTX" %in% names(DATA)){
    DATA = DATA %>%
      rename("BLOOD_TRANSFUSION_TIME" = "BLOOD_TRANSFUSION_INEVINTX")
  }

  DATA = DATA %>%
    dplyr::select(-starts_with("HISTORY_IN_BLOOD_TRANSFUSION")) %>%
    dplyr::select(-starts_with("VL")) %>%
    dplyr::select(-starts_with("MALARIA"))

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
