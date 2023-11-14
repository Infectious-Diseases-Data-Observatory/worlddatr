#' Create outcome analysis dataset.
#'
#' Outcome analysis datasets include information about the initial and final
#' outcomes of studies. This function prepares, cleans, filters and pivots
#' multiple IDDO-SDTM domains and finally merges them into a single dataset,
#' which can be used for research and analysis. The choice of DISEASE preselects
#' a number of variables which have been chosen with input from Subject Matter
#' Experts. The DM, Ds and RS Domains are required, even if empty.
#'
#' If issues are found with the code, please log them at:
#' https://github.com/RhysPeploe/iddoverse/issues
#'
#' @param DATA_DM The DM domain data frame, as named in the global environment.
#'   Required.
#' @param DATA_DS The DS domain data frame, as named in the global environment.
#'   Required.
#' @param DATA_RS The RS domain data frame, as named in the global environment.
#'   Required.
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#'   Required.
#' @param DM_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use column names as specified in the DM section
#'   of the 'IDDO SDTM Implementation Manual'. i.e. c("AGE").
#'
#' @return A dataset with one row per subject, and variables from each of the
#'   domains listed in the parameters above.
#'
#' @export
#'
#' @author Rhys Peploe
#'
ANALYSE_OUTCOME_VL = function(DATA_DM, DATA_DS, DATA_RS, DATA_MB,

                              DM_VARS = NULL){

  TOC = left_join(PREP_RS_XTRM_OUT_VL(DATA_RS, "TOC"),
                    (PREP_MB_FU_VL(DATA_MB) %>% select(-VISITDY, -VISITNUM, -EMPTY_TIME)),
                    by = c("USUBJID", "STUDYID", "INITIAL_TOC_DAY" = "DAY"))


  OVRLRESP = left_join(PREP_RS_XTRM_OUT_VL(DATA_RS, "OVRLRESP"),
                   (PREP_MB_FU_VL(DATA_MB) %>% select(-VISITDY, -VISITNUM, -EMPTY_TIME)),
                   by = c("USUBJID", "STUDYID", "FINAL_OVERALL_RESP_DAY" = "DAY"))


  # left_join(PREP_DM(DATA_DM, DISEASE = "", VARS = c("DTHFL", "DTHDTC", str_to_upper(DM_VARS))),
  #           PREP_DS_OUT_VL2(DATA_DS)) %>%
  #   left_join(FIRST) %>%
  #   left_join(LAST) %>%
  #   left_join(PREP_DS_OUT_VL(DATA_DS)) %>%
  #   left_join(PREP_RS_OUT_VL(DATA_RS))
}
