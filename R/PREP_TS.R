#' Prepare the TS domain for all outputs
#'
#' Prepare the Trial Summary (TS) domain for use in any analysis data sets.
#' Takes a IDDO-SDTM curated TS domain, subsets and joins it with the
#' Demographics (DM) domain, which can then be merged into an analysis data set
#' with other domains.
#'
#' @param DATA_TS The TS domain data frame, as named in the global environment.
#' @param DATA_DM The DM domain data frame, as named in the global environment.
#'
#' @return Data frame with a row per STUDYID, and ARMCD (treatment arm code),
#'   DISEASE and STUDYFU (study follow up) as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_TS <- function(DATA_TS, DATA_DM) {
  DATA_TDIGRP <- DATA_TS %>%
    filter(.data$TSPARMCD == "TDIGRP") %>%
    dplyr::select("STUDYID", "TSVAL") %>%
    mutate(TSVAL = str_to_upper(.data$TSVAL))

  DATA_STUDYFU <- DATA_TS %>%
    filter(.data$TSPARMCD == "TRGFUDUR") %>%
    dplyr::select("STUDYID", "TSVAL") %>%
    mutate(TSVAL = str_to_upper(.data$TSVAL))

  DATA <- DATA_DM %>%
    dplyr::select("STUDYID", "ARMCD") %>%
    distinct() %>%
    left_join(DATA_TDIGRP) %>%
    rename("DISEASE" = "TSVAL") %>%
    left_join(DATA_STUDYFU) %>%
    rename("STUDYFU" = "TSVAL")

  return(DATA)
}
