#' Prepare the RS Domain for VL outcome analysis.
#'
#' Prepare the Disease Response and Clinical Classification (RS) domain for use
#' in outcome analysis data sets studying Visceral Leishmaniasis. Takes a
#' IDDO-SDTM curated RS domain, transforms and pivots it in order to merge it
#' into an outcome analysis data set with other domains using the
#' ANALYSE_OUTCOME() function.
#'
#' @param DATA_RS The RS domain data frame, as named in the global environment.
#'
#' @return Two functions which each create a data frame focusing on outcome
#'   measures for VL
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_RS_OUT_VL = function(DATA_RS){
  DATA = DATA_RS %>%
    convert_blanks_to_na() %>%
    group_by(USUBJID, RSTESTCD) %>%
    mutate(ROWN = row_number(),
           RELAPSE = NA,
           RELAPSE_DAY = NA) %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID),
                names_from = c(RSTESTCD, ROWN), names_glue = "{RSTESTCD}{ROWN}_{.value}",
                values_from = c(RSSTRESC, VISITDY, RSDY), names_vary = "slowest")


  colnames(DATA) = str_replace_all(colnames(DATA), "RSDY", "DAY")
  colnames(DATA) = str_replace_all(colnames(DATA), "_RSSTRESC", "")
  colnames(DATA) = str_replace_all(colnames(DATA), "OVRLRESP", "OVERALL_RESP")

  DATA = DATA %>%
    dplyr::select(USUBJID, starts_with("TOC"), starts_with("OVERALL_RESP"), everything())

  return(DATA)
}

################################################################################
### Code for initial toc, final overall response, final disposition
################################################################################
PREP_RS_OUT_VL2 = function(DATA_RS, FIRST_LAST = "FIRST"){
  DATA_RS = DATA_RS %>%
    convert_blanks_to_na() %>%
    mutate(RSSTRESC = as.character(RSSTRESC),
           RSSTRESC = str_to_upper(RSSTRESC),
           DAY = RSDY)

  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "RELPASE", "RELAPSE")
  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "FAILURE - ADVERSE EVENT", "FAILURE-ADVERSE EVENT")
  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "FAILURE - RELAPSE", "FAILURE-RELAPSE")

  if(FIRST_LAST == "FIRST"){
    DATA = DATA_RS %>%
      filter(RSTESTCD == "TOC") %>%
      pivot_wider(id_cols = c(STUDYID, USUBJID),
                  names_from = RSTESTCD,
                  values_from = c(RSSTRESC, DAY),
                  values_fn = first) %>%
      rename("INITIAL_TOC" =  "RSSTRESC_TOC",
             "INITIAL_TOC_DAY" = "DAY_TOC")
  }
  else if(FIRST_LAST == "LAST"){
    DATA = DATA_RS %>%
      filter(RSTESTCD == "OVRLRESP",
             RSSCAT != "MEDICAL HISTORY" | is.na(RSSCAT)) %>%
      pivot_wider(id_cols = c(STUDYID, USUBJID),
                  names_from = RSTESTCD,
                  values_from = c(RSSTRESC, DAY),
                  values_fn = dplyr::last) %>%
      rename("FINAL_OVERALL_RESP" =  "RSSTRESC_OVRLRESP",
             "FINAL_OVERALL_RESP_DAY" = "DAY_OVRLRESP")
  }

  return(DATA)
}
