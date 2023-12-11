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
    filter(.data$MPLOC %in% MP_VARS) %>%
    DERIVE_TIMING() %>%
    mutate(MPSTRES = str_to_upper(as.character(.data$MPSTRESN)),
           MPSTRESC = str_to_upper(as.character(.data$MPSTRESC)),
           MPORRES = str_to_upper(as.character(.data$MPORRES)))

  DATA[which(is.na(DATA$MPSTRES)), "MPSTRES"] =
    DATA[which(is.na(DATA$MPSTRES)), "MPSTRESC"]
  DATA[which(is.na(DATA$MPSTRES)), "MPSTRES"] =
    DATA[which(is.na(DATA$MPSTRES)), "MPORRES"]

  if(MPTEST == "WIDTH"){
    DATA = DATA %>%
      filter(.data$MPTESTCD == "WIDTH",
             .data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
      pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                  names_from = .data$MPLOC, values_from = .data$MPSTRES,
                  names_sort = T, names_vary = "slowest",
                  values_fn = first)
  }

  else if(MPTEST == "BOTH"){
    DATA = DATA %>%
      filter(.data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
      pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                  names_from = c(.data$MPLOC, .data$MPTESTCD), values_from = .data$MPSTRES,
                  names_sort = T, names_vary = "slowest",
                  values_fn = first)
  }

  else{
    DATA = DATA %>%
      filter(.data$MPTESTCD == "LENGTH",
             .data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
      pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                  names_from = .data$MPLOC, values_from = .data$MPSTRES,
                  names_sort = T, names_vary = "slowest",
                  values_fn = first)
  }

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
