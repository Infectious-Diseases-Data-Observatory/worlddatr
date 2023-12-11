#' Prepare the RS Domain for VL outcome analysis.
#'
#' Prepare the Disease Response and Clinical Classification (RS) domain for use
#' in outcome analysis data sets studying Visceral Leishmaniasis. Takes a
#' IDDO-SDTM curated RS domain, transforms and pivots it in order to merge it
#' into an outcome analysis data set with other domains using the
#' ANALYSE_OUTCOME() function.
#'
#' @param DATA_RS The RS domain data frame, as named in the global environment.
#' @param TOC_OVRLRESP Character, choice between displaying the Test of Cure
#'   (TOC) or Overall Response (OVRLRESP). Values are thus either "TOC" or
#'   "OVRLRESP"
#' @param expand_cols Boolean option to include all RS entries for each subject.
#'   Default is FALSE, which will display the first recorded TOC or OVRLRESP
#'   event and the associated VISITNUM, VISITDY and DAY. If TRUE, all TOCs or
#'   OVRLRESPs will be listed as RPTESTCD_NUMBER_, along with the associated
#'   VISITNUM, VISITDY and DAY.
#'
#' @return Dataframe containing a row per USUBJID, with RS terms as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_RS_OUT_VL = function(DATA_RS, TOC_OVRLRESP = "TOC", expand_cols = FALSE){
  TOC_OVRLRESP = str_to_upper(TOC_OVRLRESP)

  DATA_RS = DATA_RS %>%
    convert_blanks_to_na() %>%
    filter(is.na(.data$RSSCAT) | (.data$RSSCAT != "ADDITIONAL OUTCOME PROVIDED" &
                                    .data$RSSCAT != "ADDITIONAL OUTCOMES PROVIDED" &
                                    .data$RSSCAT != "MEDICAL HISTORY")) %>%
    mutate(RSSTRES = str_to_upper(as.character(.data$RSSTRESN)),
           RSSTRESC = str_to_upper(as.character(.data$RSSTRESC)),
           RSORRES = str_to_upper(as.character(.data$RSORRES)),
           DAY = .data$RSDY)

  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "RELPASE", "RELAPSE")
  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "FAILURE - ADVERSE EVENT", "FAILURE-ADVERSE EVENT")
  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "FAILURE - RELAPSE", "FAILURE-RELAPSE")
  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "INITITAL", "INITIAL")

  DATA_RS[which(is.na(DATA_RS$RSSTRES)), "RSSTRES"] =
    DATA_RS[which(is.na(DATA_RS$RSSTRES)), "RSSTRESC"]
  DATA_RS[which(is.na(DATA_RS$RSSTRES)), "RSSTRES"] =
    DATA_RS[which(is.na(DATA_RS$RSSTRES)), "RSORRES"]

  if(expand_cols == FALSE){
    if(TOC_OVRLRESP == "TOC"){
      DATA = DATA_RS %>%
        filter(.data$RSTESTCD == "TOC") %>%
        pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                    names_from = .data$RSTESTCD,
                    values_from = c(.data$RSSTRES, .data$VISITNUM, .data$VISITDY, .data$DAY),
                    values_fn = first) %>%
        rename("INITIAL_TOC" =  "RSSTRES_TOC",
               "INITIAL_TOC_VISITNUM" =  "VISITNUM_TOC",
               "INITIAL_TOC_VISITDY" =  "VISITDY_TOC",
               "INITIAL_TOC_DAY" = "DAY_TOC")
    }

    else if(TOC_OVRLRESP == "OVRLRESP"){
      DATA = DATA_RS %>%
        filter(.data$RSTESTCD == "OVRLRESP") %>%
        pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                    names_from = .data$RSTESTCD,
                    values_from = c(.data$RSSTRES, .data$VISITNUM, .data$VISITDY, .data$DAY),
                    values_fn = first) %>%
        rename("INITIAL_OVRLRESP" =  "RSSTRES_OVRLRESP",
               "INITIAL_OVRLRESP_VISITNUM" = "VISITNUM_OVRLRESP",
               "INITIAL_OVRLRESP_VISITDY" = "VISITDY_OVRLRESP",
               "INITIAL_OVRLRESP_DAY" = "DAY_OVRLRESP")
    }
  }

  else if(expand_cols == TRUE){
    DATA = DATA_RS %>%
      group_by(.data$STUDYID, .data$USUBJID, .data$RSTESTCD) %>%
      mutate(ROWN = row_number()) %>%
      pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                  names_from = c(.data$RSTESTCD, .data$ROWN), names_glue = "{RSTESTCD}_{ROWN}_{.value}",
                  values_from = c(.data$RSSTRES, .data$VISITNUM, .data$VISITDY, .data$DAY),
                  names_vary = "slowest")

    colnames(DATA) = str_replace_all(colnames(DATA), "RSDY", "DAY")
    colnames(DATA) = str_replace_all(colnames(DATA), "_RSSTRES", "")
    colnames(DATA) = str_replace_all(colnames(DATA), "OVRLRESP", "OVERALL_RESP")

    DATA = DATA %>%
      dplyr::select("STUDYID", "USUBJID", starts_with("TOC"), starts_with("OVERALL_RESP"), everything())

  }

  return(DATA)
}
