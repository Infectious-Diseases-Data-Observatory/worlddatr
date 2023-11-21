#' Create follow up analysis dataset.
#'
#' A follow up dataset includes events and results from every recorded day for
#' each subject. This function prepares, cleans, filters and pivots multiple
#' IDDO-SDTM domains and finally merges them into a single dataset, which can be
#' used for research and analysis. The choice of DISEASE preselects a number of
#' variables which have been chosen with input from Subject Matter Experts. The
#' DM domain is the only one which is required for the code to run, the
#' rest can be optional.
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
#' @param DATA_LB The LB domain data frame, as named in the global environment.
#' @param DATA_IN The IN domain data frame, as named in the global environment.
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#' @param DATA_MP The MP domain data frame, as named in the global environment.
#' @param DATA_RP The RP domain data frame, as named in the global environment.
#' @param DATA_SA The SA domain data frame, as named in the global environment.
#' @param DATA_TS The TS domain data frame, as named in the global environment.
#' @param DATA_VS The VS domain data frame, as named in the global environment.
#' @param DATA_SC The SC domain data frame, as named in the global environment.
#' @param DATA_PO The PO domain data frame, as named in the global environment.
#'
#' @param DM_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use column names as specified in the DM section
#'   of the 'IDDO SDTM Implementation Manual'. i.e. c("DTHFL", "DTHDTC").
#' @param LB_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for LBTESTCD as
#'   specified in the LB section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CHOL").
#' @param IN_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for INDECOD as
#'   specified in the IN section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("PARACETAMOL").
#' @param MB_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for MBTESTCD as
#'   specified in the MB section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CRONAVIR").
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
#' @param SC_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for SCTESTCD as
#'   specified in the SC section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("ADEVAIND").
#' @param PO_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for POTERM as
#'   specified in the PO section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("ADEVAIND").
#' @param MP_TESTCD Specify which MPTESTCD is desired in the output. Options are:
#'   "LENGTH", "WIDTH" or "BOTH". Default is Length.
#'
#' @return A dataset with one row per subject per day, and variables from each
#'   of the domains listed in the parameters above.
#'
#' @export
#'
#' @author Rhys Peploe
#'
ANALYSE_FOLLOW_UP = function(DISEASE_THEME = "", DATA_DM, DATA_LB,
                             DATA_IN = NULL, DATA_MB = NULL, DATA_MP = NULL, DATA_RP = NULL,
                             DATA_SA = NULL, DATA_TS = NULL, DATA_VS = NULL, DATA_SC = NULL,
                             DATA_PO = NULL,

                             DM_VARS = NULL, LB_VARS = NULL, IN_VARS = NULL, MB_VARS = NULL,   #User can specify extra variables
                             MP_VARS = NULL, RP_VARS = NULL, SA_VARS = NULL, VS_VARS = NULL,
                             SC_VARS = NULL, PO_VARS = NULL,

                             MP_TESTCD = "LENGTH"){
  DM = PREP_DM(DATA_DM, DISEASE = "", VARS = DM_VARS)

  if(is.null(DATA_TS) == FALSE){
    DM = DM %>%
      left_join(PREP_TS(DATA_TS, DATA_DM)) %>%
      dplyr::select("STUDYID", "DISEASE", everything())
  }

  if(is.null(DATA_LB) == FALSE){
    FU = PREP_LB_FU(DATA_LB, DISEASE = DISEASE_THEME, VARS = LB_VARS) %>%
      mutate(START_DAY = NA,
             END_DAY = NA)
  }

  else{
    FU = data.frame(STUDYID = NA) %>%
      mutate(START_DAY = NA,
             END_DAY = NA)
  }

  if(is.null(DATA_MB) == FALSE){
    MB_JOIN = PREP_MB_FU(DATA_MB, DISEASE = DISEASE_THEME, VARS = MB_VARS) %>%
      left_join(PREP_MBSPEC_FU(DATA_MB, DISEASE = DISEASE_THEME, VARS = MB_VARS)) %>%
      dplyr::select(order(everything()))

    if("VISCERAL LEISHMANIASIS" %in% DM$DISEASE | DISEASE_THEME == "VL"){
      FU = FU %>%
        full_join(PREP_MB_FU_VL(DATA_MB)) %>%
        dplyr::select("USUBJID", "VISITDY", everything()) %>%
        full_join(MB_JOIN)
    }

    else if("MALARIA" %in% DM$DISEASE | DISEASE_THEME == "MALARIA"){
      FU = FU %>%
        full_join(PREP_MB_FU_MAL(DATA_MB))%>%
        dplyr::select("USUBJID", "VISITDY", everything()) %>%
        full_join(MB_JOIN)
    }
  }

  if(is.null(DATA_MP) == FALSE){
    FU = FU %>%
      full_join(PREP_MP_FU(DATA_MP, MPTEST = MP_TESTCD, VARS = MP_VARS))
  }

  if(is.null(DATA_IN) == FALSE){
    FU = FU %>%
      full_join(PREP_IN_B_FU(DATA_IN)) %>%
      full_join(PREP_IN_FU(DATA_IN, VARS = IN_VARS))
  }

  if(is.null(DATA_VS) == FALSE){
    FU = FU %>%
      full_join(PREP_VS_FU(DATA_VS, DISEASE = DISEASE_THEME, VARS = VS_VARS)) %>%
      full_join(PREP_VS_TEMP_FU(DATA_VS)) %>%
      relocate("TEMP_LOC", .after = "TEMP")
  }

  if(is.null(DATA_SA) == FALSE){
    FU = FU %>%
      full_join(PREP_SA_FU(DATA_SA, DISEASE = DISEASE_THEME, VARS = SA_VARS)) %>%
      full_join(PREP_SA_MV_FU(DATA_SA))
  }

  if(is.null(DATA_RP) == FALSE){
    FU = FU %>%
      full_join(PREP_RP_FU(DATA_RP, VARS = RP_VARS))
  }

  if(is.null(DATA_PO) == FALSE){
    FU = FU %>%
      full_join(PREP_PO_FU(DATA_PO, VARS = PO_VARS))
  }

  FU = right_join(DM, FU) %>%
    relocate(ends_with("DY"), .after = "VISITNUM") %>%
    relocate(ends_with("DAY"), .after = "VISITDY") %>%
    DERIVE_BMI() %>%
    JOIN_PREGNANT() %>%
    JOIN_HIV() %>%
    DERIVE_ANTHRO() %>%
    filter(is.na("STUDYID") == FALSE & is.na("USUBJID") == FALSE)

  if("START_DAY" %in% names(FU)){
    FU = FU[order(FU$USUBJID, FU$VISITNUM, FU$VISITDY, FU$DAY, FU$START_DAY, FU$END_DAY), ]
  }

  else{
    FU = FU[order(FU$USUBJID, FU$VISITNUM, FU$VISITDY, FU$DAY), ]
  }

  return(FU)
}
