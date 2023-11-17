#' Create first occurrence analysis dataset.
#'
#' First occurrence analysis datasets include information on the first time that
#' an event or finding was recorded, detailing the result of the finding or
#' event and what day this occurred on. This function prepares, cleans, filters
#' and pivots multiple IDDO-SDTM domains and finally merges them into a single
#' dataset, which can be used for research and analysis. The choice of DISEASE
#' preselects a number of variables which have been chosen with input from
#' Subject Matter Experts. The DM domain is required.
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
#' @param DATA_VS The VS domain data frame, as named in the global environment.
#' @param DATA_IN The IN domain data frame, as named in the global environment.
#' @param DATA_SA The SA domain data frame, as named in the global environment.
#' @param DATA_ER The ER domain data frame, as named in the global environment.
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#'
#' @param DM_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use column names as specified in the DM section
#'   of the 'IDDO SDTM Implementation Manual'. i.e. c("DTHFL", "DTHDTC").
#' @param LB_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for LBTESTCD as
#'   specified in the LB section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CHOL").
#' @param VS_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for VSTESTCD as
#'   specified in the VS section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("MAP").
#' @param IN_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for INDECOD as
#'   specified in the IN section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("PARACETAMOL").
#' @param SA_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for SADECOD as
#'   specified in the SA section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("BLEEDING").
#' @param MB_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for MBTESTCD as
#'   specified in the MB section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CRONAVIR").
#'
#' @return A dataset with one row per subject, and variables from each of the
#'   domains listed in the parameters above.
#'
#' @export
#'
#' @author Rhys Peploe
#'
ANALYSE_FIRST_OCCURRENCE = function(DISEASE_THEME = "",
                                    DATA_DM, DATA_LB = NULL, DATA_VS = NULL,
                                    DATA_IN = NULL, DATA_SA = NULL,
                                    DATA_ER = NULL, DATA_MB = NULL,

                                    DM_VARS = NULL, LB_VARS = NULL, VS_VARS = NULL,
                                    IN_VARS = NULL, SA_VARS = NULL, MB_VARS = NULL
){
  FIRST_OCCURANCE = PREP_DM(DATA_DM, DISEASE = DISEASE_THEME, VARS = DM_VARS)

  if(is.null(DATA_LB) == FALSE){
    FIRST_OCCURANCE = FIRST_OCCURANCE %>%
      left_join(PREP_LB_FIRST(DATA_LB, DISEASE = DISEASE_THEME, VARS = LB_VARS))
  }

  if(is.null(DATA_VS) == FALSE){
    FIRST_OCCURANCE = FIRST_OCCURANCE %>%
      left_join(PREP_VS_FIRST(DATA_VS, DISEASE = DISEASE_THEME, VARS = VS_VARS))
  }

  if(is.null(DATA_IN) == FALSE){
    FIRST_OCCURANCE = FIRST_OCCURANCE %>%
      left_join(PREP_IN_FIRST(DATA_IN, DISEASE = DISEASE_THEME, VARS = IN_VARS))
  }

  if(is.null(DATA_SA) == FALSE){
    FIRST_OCCURANCE = FIRST_OCCURANCE %>%
      left_join(PREP_SA_FIRST(DATA_SA, DISEASE = DISEASE_THEME, VARS = SA_VARS))
  }

  if(is.null(DATA_ER) == FALSE){
    FIRST_OCCURANCE = FIRST_OCCURANCE %>%
      left_join(PREP_ER_FIRST(DATA_ER))
  }

  if(is.null(DATA_MB) == FALSE){
    FIRST_OCCURANCE = FIRST_OCCURANCE %>%
      left_join(PREP_MB_FIRST(DATA_MB, DISEASE = DISEASE_THEME, VARS = MB_VARS))
  }

  return(FIRST_OCCURANCE)
}
