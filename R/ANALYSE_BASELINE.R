#' Create baseline analysis dataset.
#'
#' Baseline analysis datasets includes events and tests conducted on the
#' subject's first day in the study; this also included medical history. This
#' function prepares, cleans, filters and pivots multiple IDDO-SDTM domains and
#' finally merges them into a single dataset, which can be used for research and
#' analysis. The choice of DISEASE preselects a number of variables which have
#' been chosen with input from Subject Matter Experts. The DM domain is the only
#' one which is required for the code to run, the rest can be optional.
#'
#' If issues are found with the code, please log them at:
#' https://github.com/RhysPeploe/iddoverse/issues
#'
#' @param DISEASE_THEME The name of the disease theme being analysed. Character
#'   string. Default is empty (selects base variables). Select from: "MALARIA",
#'   "VL" or "EBOLA". If selection is missing or misspelt, then the base
#'   variables will be used.
#' @param DATA_DM The DM domain data frame, as named in the global environment.
#'   Required.
#' @param DATA_IN The IN domain data frame, as named in the global environment.
#' @param DATA_LB The LB domain data frame, as named in the global environment.
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#' @param DATA_MP The MP domain data frame, as named in the global environment.
#' @param DATA_RP The RP domain data frame, as named in the global environment.
#' @param DATA_SA The SA domain data frame, as named in the global environment.
#' @param DATA_TS The TS domain data frame, as named in the global environment.
#' @param DATA_VS The VS domain data frame, as named in the global environment.
#'
#' @param DM_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use column names as specified in the DM section
#'   of the 'IDDO SDTM Implementation Manual'. i.e. c("DTHFL", "DTHDTC").
#' @param IN_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for INDECOD as
#'   specified in the IN section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("PARACETAMOL").
#' @param LB_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for LBTESTCD as
#'   specified in the LB section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CHOL").
#' @param MB_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for MBTESTCD as
#'   specified in the MB section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CRONAVIR").
#' @param MB_VARS_SPEC See MB_VARS above.
#' @param MP_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for MPLOC as
#'   specified in the MP section of the 'IDDO SDTM Implementation Manual'. Note
#'   this is only for LENGTH of the organ, not the WIDTH.
#' @param RP_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for RPTESTCD as
#'   specified in the RP section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("EGESTAGE", "LMPSTDTC").
#' @param SA_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for SADECOD as
#'   specified in the SA section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("BLEEDING").
#' @param VS_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for VSTESTCD as
#'   specified in the VS section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("MAP").
#'
#' @param IN_DUR Should the analysis dataset include the duration of the
#'   Treatment / Intervention event? This is the time from the start of the
#'   event till the end. Boolean, default is FALSE.
#' @param IN_TIME Should the analysis dataset include the time since the
#'   Treatment / Intervention event? This is the time since the end of the
#'   event. Boolean, default is FALSE.
#' @param SA_DUR Should the analysis dataset include the duration of the
#'   Clinical / Adverse Effect event? This is the time from the start of the
#'   event till the end. Boolean, default is FALSE.
#' @param SA_TIME Should the analysis dataset include the time since the
#'   Clinical / Adverse Effect event? This is the time since the end of the
#'   event. Boolean, default is FALSE.
#' @param MP_TESTCD Specify which MPTESTCD is desired in the output. Options
#'   are: "LENGTH", "WIDTH" or "BOTH". Default is Length.
#'
#' @return A dataset with one row per subject, and variables from each of the
#'   domains listed in the parameters above.
#'
#' @export
#'
#' @author Rhys Peploe
#'
ANALYSE_BASELINE <- function(DISEASE_THEME = "", DATA_DM,
                             DATA_IN = NULL, DATA_LB = NULL, DATA_MB = NULL,
                             DATA_MP = NULL, DATA_RP = NULL, DATA_SA = NULL,
                             DATA_TS = NULL, DATA_VS = NULL,
                             DM_VARS = NULL, LB_VARS = NULL, MB_VARS = NULL,
                             MB_VARS_SPEC = NULL, MP_VARS = NULL, # User can specify extra variables
                             IN_VARS = NULL, RP_VARS = NULL, SA_VARS = NULL,
                             VS_VARS = NULL,
                             IN_DUR = FALSE, IN_TIME = FALSE, # Choose to include Duration (DUR) or Time since event (TIME)
                             SA_DUR = FALSE, SA_TIME = FALSE,
                             MP_TESTCD = "LENGTH") {
  BASELINE <- PREP_DM(DATA_DM, DISEASE = DISEASE_THEME, VARS = DM_VARS)

  if (is.null(DATA_TS) == FALSE) {
    BASELINE <- BASELINE %>%
      left_join(PREP_TS(DATA_TS, DATA_DM)) %>%
      dplyr::select("STUDYID", "DISEASE", everything())
  } else {
    BASELINE <- BASELINE %>%
      mutate(DISEASE = NA)
  }

  if (is.null(DATA_LB) == FALSE) {
    BASELINE <- BASELINE %>%
      left_join(PREP_LB_BL(DATA_LB, DISEASE = DISEASE_THEME, VARS = LB_VARS))
  }

  if (is.null(DATA_MB) == FALSE) {
    MB_JOIN <- left_join(
      PREP_MB_BL(DATA_MB,
        DISEASE = DISEASE_THEME,
        VARS = MB_VARS
      ),
      PREP_MBSPEC_BL(DATA_MB,
        DISEASE = DISEASE_THEME,
        VARS = MB_VARS_SPEC
      )
    ) %>%
      dplyr::select(order(everything()))

    BASELINE <- BASELINE %>%
      left_join(MB_JOIN)

    if ("VISCERAL LEISHMANIASIS" %in% BASELINE$DISEASE | DISEASE_THEME == "VL") {
      BASELINE <- BASELINE %>%
        left_join(PREP_MB_BL_VL(DATA_MB))
    }

    if ("MALARIA" %in% BASELINE$DISEASE | DISEASE_THEME == "MALARIA") {
      BASELINE <- BASELINE %>%
        left_join(PREP_MB_BL_MAL(DATA_MB))
    }
  }

  if (is.null(DATA_IN) == FALSE) {
    BASELINE <- BASELINE %>%
      left_join(PREP_IN_BL(DATA_IN,
        VARS = IN_VARS,
        inc_DUR = IN_DUR, inc_TIME = IN_TIME
      )) %>%
      left_join(PREP_IN_BMV_BL(DATA_IN,
        inc_DUR = IN_DUR, inc_TIME = IN_TIME
      ))
  }

  if (is.null(DATA_MP) == FALSE) {
    BASELINE <- BASELINE %>%
      left_join(PREP_MP_BL(DATA_MP, MPTEST = MP_TESTCD, VARS = MP_VARS))
  }

  if (is.null(DATA_RP) == FALSE) {
    BASELINE <- BASELINE %>%
      left_join(PREP_RP_BL(DATA_RP, VARS = RP_VARS))
  }

  if (is.null(DATA_SA) == FALSE) {
    BASELINE <- BASELINE %>%
      left_join(PREP_SA_BL(DATA_SA,
        DISEASE = DISEASE_THEME, VARS = SA_VARS,
        inc_DUR = SA_DUR, inc_TIME = SA_TIME
      )) %>%
      left_join(PREP_SA_MV_BL(DATA_SA, inc_DUR = SA_DUR, inc_TIME = SA_TIME))
  }

  if (is.null(DATA_VS) == FALSE) {
    BASELINE <- BASELINE %>%
      left_join(PREP_VS_BL(DATA_VS,
        DISEASE = DISEASE_THEME,
        VARS = VS_VARS
      )) %>%
      left_join(PREP_VS_TEMP_BL(DATA_VS))
  }

  BASELINE <- BASELINE %>%
    DERIVE_BMI() %>%
    JOIN_HIV(SA_TIME, SA_DUR) %>%
    JOIN_MV() %>%
    JOIN_PREGNANT()

  colnames(BASELINE) <- gsub("_SAOCCUR", "", names(BASELINE))
  colnames(BASELINE) <- gsub("_SADUR", "_DUR", names(BASELINE))
  colnames(BASELINE) <- gsub("_SAEVINTX", "_TIME", names(BASELINE))
  colnames(BASELINE) <- gsub("_INOCCUR", "", names(BASELINE))
  colnames(BASELINE) <- gsub("_INDUR", "_DUR", names(BASELINE))
  colnames(BASELINE) <- gsub("_INEVINTX", "_TIME", names(BASELINE))

  if (("HEIGHT" %in% names(BASELINE)) | ("WEIGHT" %in% names(BASELINE))) {
    BASELINE <- BASELINE %>%
      left_join(DERIVE_ANTHRO(BASELINE))
  }

  return(BASELINE)
}
