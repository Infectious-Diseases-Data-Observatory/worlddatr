ANALYSE_HR0SCR_BASELINE = function(DISEASE_THEME = "", DATA_DM,
                            DATA_IN = NULL, DATA_LB = NULL, DATA_MB = NULL,
                            DATA_MP = NULL, DATA_RP = NULL, DATA_SA = NULL,
                            DATA_TS = NULL, DATA_VS = NULL,

                            DM_VARS = NULL, LB_VARS = NULL, MB_VARS = NULL,
                            MB_VARS_SPEC = NULL, MP_VARS = NULL,               #User can specify extra variables
                            IN_VARS = NULL, RP_VARS = NULL, SA_VARS = NULL,
                            VS_VARS = NULL,

                            IN_DUR = FALSE, IN_TIME = FALSE,                   #Choose to include Duration (DUR) or Time since event (TIME)
                            SA_DUR = FALSE, SA_TIME = FALSE,

                            MP_TESTCD = "LENGTH"){
  BASELINE = PREP_DM(DATA_DM, DISEASE = DISEASE_THEME, VARS = DM_VARS)

  if(is.null(DATA_TS) == FALSE){
    BASELINE = BASELINE %>%
      left_join(PREP_TS(DATA_TS, DATA_DM)) %>%
      dplyr::select("STUDYID", "DISEASE", everything())
  }

  else{
    BASELINE = BASELINE %>%
      mutate(DISEASE = NA)
  }

  if(is.null(DATA_LB) == FALSE){
    BASELINE = BASELINE %>%
      left_join(PREP_LB_HR0_BL(DATA_LB, DISEASE = DISEASE_THEME, VARS = LB_VARS))
  }

  if(is.null(DATA_MB) == FALSE){
    MB_JOIN = left_join(PREP_MB_HR0_BL(DATA_MB, DISEASE = DISEASE_THEME, VARS = MB_VARS),
                        PREP_MBSPEC_HR0_BL(DATA_MB, DISEASE = DISEASE_THEME, VARS = MB_VARS_SPEC)) %>%
      dplyr::select(order(everything()))

    BASELINE = BASELINE %>%
      left_join(MB_JOIN)

    if("VISCERAL LEISHMANIASIS" %in% BASELINE$DISEASE | DISEASE_THEME == "VL"){
      BASELINE = BASELINE %>%
        left_join(PREP_MB_BL_VL(DATA_MB))
    }

    if("MALARIA" %in% BASELINE$DISEASE | DISEASE_THEME == "MALARIA"){
      BASELINE = BASELINE %>%
        left_join(PREP_MB_HR0_BL_MAL(DATA_MB))
    }
  }

  if(is.null(DATA_IN) == FALSE){
    BASELINE = BASELINE %>%
      left_join(PREP_IN_BL(DATA_IN, VARS = IN_VARS, inc_DUR = IN_DUR, inc_TIME = IN_TIME)) %>%
      left_join(PREP_IN_BMV_BL(DATA_IN, inc_DUR = IN_DUR, inc_TIME = IN_TIME))
  }

  if(is.null(DATA_MP) == FALSE){
    BASELINE = BASELINE %>%
      left_join(PREP_MP_BL(DATA_MP, MPTEST = MP_TESTCD, VARS = MP_VARS))
  }

  if(is.null(DATA_RP) == FALSE){
    BASELINE = BASELINE %>%
      left_join(PREP_RP_BL(DATA_RP, VARS = RP_VARS))
  }

  if(is.null(DATA_SA) == FALSE){
    BASELINE = BASELINE %>%
      left_join(PREP_SA_BL(DATA_SA, DISEASE = DISEASE_THEME, VARS = SA_VARS, inc_DUR = SA_DUR, inc_TIME = SA_TIME)) %>%
      left_join(PREP_SA_MV_BL(DATA_SA, inc_DUR = SA_DUR, inc_TIME = SA_TIME))
  }

  if(is.null(DATA_VS) == FALSE){
    BASELINE = BASELINE %>%
      left_join(PREP_VS_SCR_BL(DATA_VS, DISEASE = DISEASE_THEME, VARS = VS_VARS)) %>%
      left_join(PREP_VS_SCR_TEMP_BL(DATA_VS))
  }

  BASELINE = BASELINE %>%
    DERIVE_BMI() %>%
    JOIN_HIV(SA_TIME, SA_DUR) %>%
    JOIN_MV() %>%
    JOIN_PREGNANT()

  colnames(BASELINE) = gsub("_SAOCCUR", "", names(BASELINE))
  colnames(BASELINE) = gsub("_SADUR", "_DUR", names(BASELINE))
  colnames(BASELINE) = gsub("_SAEVINTX", "_TIME", names(BASELINE))
  colnames(BASELINE) = gsub("_INOCCUR", "", names(BASELINE))
  colnames(BASELINE) = gsub("_INDUR", "_DUR", names(BASELINE))
  colnames(BASELINE) = gsub("_INEVINTX", "_TIME", names(BASELINE))

  if(("HEIGHT" %in% names(BASELINE)) | ("WEIGHT" %in% names(BASELINE))){
    BASELINE = BASELINE %>%
      left_join(DERIVE_ANTHRO(BASELINE))
  }

  return(BASELINE)
}
