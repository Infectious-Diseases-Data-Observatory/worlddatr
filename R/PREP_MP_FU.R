#' Prepare the MP domain for follow up analysis.
#'
#' Prepare the Morphology and Physiology (MP) domain for use in follow up
#' analysis data sets. Takes a IDDO-SDTM curated MP domain, transforms and
#' pivots it in order to merge it into a follow up analysis data set with other
#' domains using the ANALYSE_FOLLOW_UP() function. Default variables are: "LIVER" &
#' "SPLEEN"
#'
#' @param DATA_MP The MP domain data frame, as named in the global environment.
#' @param MPTEST Specify which MPTESTCD is desired in the output. Options are:
#'   "LENGTH", "WIDTH" or "BOTH". Default is Length.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for MPLOC as
#'   specified in the MP section of the 'IDDO SDTM Implementation Manual'.
#'
#' @return Dataframe containing a row per USUBJID/subject per day, with
#'   MPTESTCDs and the units as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MP_FU <- function(DATA_MP, MPTEST = "LENGTH", VARS = NULL) {
  MP_VARS <- c("LIVER", "SPLEEN", str_to_upper(VARS))
  MPTEST <- str_to_upper(MPTEST)

  DATA_MP <- DATA_MP %>%
    convert_blanks_to_na() %>%
    filter(.data$MPLOC %in% MP_VARS) %>%
    mutate(
      MPSTRES = str_to_upper(as.character(.data$MPSTRESN)),
      MPSTRESC = str_to_upper(as.character(.data$MPSTRESC)),
      MPORRES = str_to_upper(as.character(.data$MPORRES)),
      DAY = .data$MPDY,
      MPUNITS = as.character(NA)
    )

  DATA_EMPTY <- DATA_MP %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_MP %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$MPSTRES)), "MPSTRES"] <-
    DATA[which(is.na(DATA$MPSTRES)), "MPSTRESC"]
  DATA[which(is.na(DATA$MPSTRES)), "MPSTRES"] <-
    DATA[which(is.na(DATA$MPSTRES)), "MPORRES"]

  DATA[which(!is.na(DATA$MPSTRESC) | !is.na(DATA$MPSTRESN)), "MPUNITS"] <-
    DATA[which(!is.na(DATA$MPSTRESC) | !is.na(DATA$MPSTRESN)), "MPSTRESU"]
  DATA[which(is.na(DATA$MPSTRESC) & is.na(DATA$MPSTRESN)), "MPUNITS"] <-
    DATA[which(is.na(DATA$MPSTRESC) & is.na(DATA$MPSTRESN)), "MPORRESU"]

  if (MPTEST == "WIDTH") {
    DATA <- DATA %>%
      filter(.data$MPTESTCD == "WIDTH") %>%
      pivot_wider(
        id_cols = c(
          .data$STUDYID, .data$USUBJID, .data$VISITDY, .data$VISITNUM,
          .data$DAY, .data$EMPTY_TIME
        ),
        names_from = .data$MPLOC, values_from = c(.data$MPSTRES, .data$MPUNITS),
        names_sort = T, names_vary = "slowest", names_glue = "{MPLOC}_{.value}",
        values_fn = first
      )
  } else if (MPTEST == "BOTH") {
    DATA <- DATA %>%
      pivot_wider(
        id_cols = c(
          .data$STUDYID, .data$USUBJID, .data$VISITDY, .data$VISITNUM,
          .data$DAY, .data$EMPTY_TIME
        ),
        names_from = c(.data$MPLOC, .data$MPTESTCD), values_from = c(.data$MPSTRES, .data$MPUNITS),
        names_sort = T, names_vary = "slowest", names_glue = "{MPLOC}_{MPTESTCD}_{.value}",
        values_fn = first
      )
  } else {
    DATA <- DATA %>%
      filter(.data$MPTESTCD == "LENGTH") %>%
      pivot_wider(
        id_cols = c(
          .data$STUDYID, .data$USUBJID, .data$VISITDY, .data$VISITNUM,
          .data$DAY, .data$EMPTY_TIME
        ),
        names_from = .data$MPLOC, values_from = c(.data$MPSTRES, .data$MPUNITS),
        names_sort = T, names_vary = "slowest", names_glue = "{MPLOC}_{.value}",
        values_fn = first
      )
  }

  colnames(DATA) <- gsub("_MPSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("MPUNITS", "UNITS", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
