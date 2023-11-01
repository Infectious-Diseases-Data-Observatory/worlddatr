#' Prepare the MP domain for baseline analysis.
#'
#' Prepare the Morphology and Physiology (MP) domain for use in baseline
#' analysis data sets. Takes a IDDO-SDTM curated MP domain, transforms and
#' pivots it in order to merge it into a baseline analysis data set with other
#' domains using the ANALYSE_BASELINE() function.
#'
#' @param DATA_MP The MP domain data frame, as named in the global environment.
#' @param MPTEST Specify which MPTESTCD is desired in the output. Options are:
#'   "LENGTH", "WIDTH" or "BOTH". Default is Length.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for MPLOC as
#'   specified in the MP section of the 'IDDO SDTM Implementation Manual'.
#'
#' @return Dataframe containing a row per USUBJID/subject, with MPTESTCDs as
#'   columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MP_BL = function(DATA_MP, MPTEST = "LENGTH", VARS = NULL){
  MP_VARS = c("LIVER", "SPLEEN", str_to_upper(VARS))
  MPTEST = str_to_upper(MPTEST)

  DATA = DATA_MP %>%
    convert_blanks_to_na() %>%
    filter(MPLOC %in% MP_VARS) %>%
    DERIVE_TIMING() %>%
    mutate(MPSTRES = as.character(MPSTRESN),
           MPSTRESC = as.character(MPSTRESC),
           MPORRES = as.character(MPORRES))

  DATA[which(is.na(DATA$MPSTRES)), "MPSTRES"] =
    DATA[which(is.na(DATA$MPSTRES)), "MPSTRESC"]
  DATA[which(is.na(DATA$MPSTRES)), "MPSTRES"] =
    DATA[which(is.na(DATA$MPSTRES)), "MPORRES"]

  if(MPTEST == "WIDTH"){
    DATA = DATA %>%
      filter(MPTESTCD == "WIDTH",
             TIMING == 1 | TIMING == "BASELINE") %>%
      pivot_wider(id_cols = c(STUDYID, USUBJID),
                  names_from = MPLOC, values_from = MPSTRES,
                  names_sort = T, names_vary = "slowest",
                  values_fn = first)
  }

  else if(MPTEST == "BOTH"){
    DATA = DATA %>%
      filter(TIMING == 1 | TIMING == "BASELINE") %>%
      pivot_wider(id_cols = c(STUDYID, USUBJID),
                  names_from = c(MPLOC, MPTESTCD), values_from = MPSTRES,
                  names_sort = T, names_vary = "slowest",
                  values_fn = first)
  }

  else{
    DATA = DATA %>%
      filter(MPTESTCD == "LENGTH",
             TIMING == 1 | TIMING == "BASELINE") %>%
      pivot_wider(id_cols = c(STUDYID, USUBJID),
                  names_from = MPLOC, values_from = MPSTRES,
                  names_sort = T, names_vary = "slowest",
                  values_fn = first)
  }

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
