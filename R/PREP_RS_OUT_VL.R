PREP_RS_OUT_VL = function(DATA_RS, TOC_OVRLRESP = "TOC"){
  DATA_RS = DATA_RS %>%
    convert_blanks_to_na() %>%
    filter(is.na(RSSCAT) | (RSSCAT != "ADDITIONAL OUTCOME PROVIDED" &
                              RSSCAT != "ADDITIONAL OUTCOMES PROVIDED" &
                              RSSCAT != "MEDICAL HISTORY")) %>%
    mutate(RSSTRES = str_to_upper(as.character(RSSTRESN)),
           RSSTRESC = str_to_upper(as.character(RSSTRESC)),
           RSORRES = str_to_upper(as.character(RSORRES)),
           DAY = RSDY)

  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "RELPASE", "RELAPSE")
  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "FAILURE - ADVERSE EVENT", "FAILURE-ADVERSE EVENT")
  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "FAILURE - RELAPSE", "FAILURE-RELAPSE")
  DATA_RS$RSSTRESC = str_replace_all(DATA_RS$RSSTRESC, "INITITAL", "INITIAL")

  DATA_RS[which(is.na(DATA_RS$RSSTRES)), "RSSTRES"] =
    DATA_RS[which(is.na(DATA_RS$RSSTRES)), "RSSTRESC"]
  DATA_RS[which(is.na(DATA_RS$RSSTRES)), "RSSTRES"] =
    DATA_RS[which(is.na(DATA_RS$RSSTRES)), "RSORRES"]

  if(TOC_OVRLRESP == "TOC"){
    DATA = DATA_RS %>%
      filter(RSTESTCD == "TOC") %>%
      pivot_wider(id_cols = c(STUDYID, USUBJID),
                  names_from = RSTESTCD,
                  values_from = c(RSSTRES, VISITNUM, VISITDY, DAY),
                  values_fn = first) %>% #set to be the first inital test of cure done, given the filters
      rename("INITIAL_TOC" =  "RSSTRES_TOC",
             "INITIAL_TOC_VISITNUM" =  "VISITNUM_TOC",
             "INITIAL_TOC_VISITDY" =  "VISITDY_TOC",
             "INITIAL_TOC_DAY" = "DAY_TOC")
  }

  else if(TOC_OVRLRESP == "OVRLRESP"){
    DATA = DATA_RS %>%
      filter(RSTESTCD == "OVRLRESP") %>%
      pivot_wider(id_cols = c(STUDYID, USUBJID),
                  names_from = RSTESTCD,
                  values_from = c(RSSTRES, VISITNUM, VISITDY, DAY),
                  values_fn = first) %>%
      rename("OVERALL_RESP" =  "RSSTRES_OVRLRESP",
             "OVERALL_RESP_VISITNUM" = "VISITNUM_OVRLRESP",
             "OVERALL_RESP_VISITDY" = "VISITDY_OVRLRESP",
             "OVERALL_RESP_DAY" = "DAY_OVRLRESP")
  }

  return(DATA)
}

# PREP_RS_OUT_VL(RS_PULL, TOC_OVRLRESP = "TOC") %>% View
# PREP_RS_OUT_VL(RS_PULL, TOC_OVRLRESP = "OVRLRESP") %>% View
#
# left_join(PREP_DM(DM_PULL, "VL"), PREP_RS_OUT_VL(RS_PULL, TOC_OVRLRESP = "TOC")) %>%
#   left_join(PREP_RS_OUT_VL(RS_PULL, TOC_OVRLRESP = "OVRLRESP"))
