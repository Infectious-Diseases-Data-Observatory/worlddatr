#' Prepare the MP domain for follow up analysis.
#'
#' Prepare the Morphology and Physiology (MP) domain for use in follow up
#' analysis data sets. Takes a IDDO-SDTM curated MP domain, transforms and
#' pivots it in order to merge it into a follow up analysis data set with other
#' domains using the ANALYSE_FOLLOW_UP() function.
#'
#' @param DATA_MP The MP domain data frame, as named in the global environment.
#' @param MPTEST Specify which MPTESTCD is desired in the output. Options are:
#'   "LENGTH", "WIDTH" or "BOTH". Default is Length.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for MPLOC as
#'   specified in the MP section of the 'IDDO SDTM Implementation Manual'.
#'
#' @return Dataframe containing a row per USUBJID/subject per day, with
#'   MPTESTCDs as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MP_FU = function(DATA_MP, MPTEST = "LENGTH", VARS = NULL){
  MP_VARS = c("LIVER", "SPLEEN", str_to_upper(VARS))
  MPTEST = str_to_upper(MPTEST)

  DATA_MP = DATA_MP %>%
    convert_blanks_to_na() %>%
    filter(MPLOC %in% MP_VARS) %>%
    mutate(MPSTRES = as.character(MPSTRESN),
           MPSTRESC = as.character(MPSTRESC),
           MPORRES = as.character(MPORRES),
           DAY = MPDY)

  DATA_EMPTY = DATA_MP %>%
    filter(is.na(VISITDY) & is.na(VISITNUM) & is.na(DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA = DATA_MP %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$MPSTRES)), "MPSTRES"] =
    DATA[which(is.na(DATA$MPSTRES)), "MPSTRESC"]
  DATA[which(is.na(DATA$MPSTRES)), "MPSTRES"] =
    DATA[which(is.na(DATA$MPSTRES)), "MPORRES"]

  if(MPTEST == "WIDTH"){
    DATA = DATA %>%
      filter(MPTESTCD == "WIDTH") %>%
      pivot_wider(id_cols = c(STUDYID, USUBJID, VISITDY, VISITNUM, DAY, EMPTY_TIME),
                  names_from = MPLOC, values_from = MPSTRES,
                  names_sort = T, names_vary = "slowest",
                  values_fn = first)
  }

  else if(MPTEST == "BOTH"){
    DATA = DATA %>%
      pivot_wider(id_cols = c(STUDYID, USUBJID, VISITDY, VISITNUM, DAY, EMPTY_TIME),
                  names_from = c(MPLOC, MPTESTCD), values_from = MPSTRES,
                  names_sort = T, names_vary = "slowest",
                  values_fn = first)
  }

  else{
    DATA = DATA %>%
      filter(MPTESTCD == "LENGTH") %>%
      pivot_wider(id_cols = c(STUDYID, USUBJID, VISITDY, VISITNUM, DAY, EMPTY_TIME),
                  names_from = MPLOC, values_from = MPSTRES,
                  names_sort = T, names_vary = "slowest",
                  values_fn = first)
  }

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
