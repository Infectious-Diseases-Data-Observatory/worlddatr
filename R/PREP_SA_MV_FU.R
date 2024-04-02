#' Further prepare the SA domain for follow up analysis specifically for Malaria
#' and VL variables.
#'
#' Prepare the Clinical and Adverse Effects (SA) domain for use in follow up
#' analysis data sets focusing on Malaria and Visceral Leishmaniasis. Takes a
#' IDDO-SDTM curated SA domain, transforms and pivots it in order to merge it
#' into a follow up analysis data set with other domains using the
#' ANALYSE_FOLLOW_UP() function. PREP_SA_BL() and PREP_SA_MV_BL() would be merged
#' in the ANALYSE_FOLLOW_UP() function.
#'
#' @param DATA_SA The SA domain data frame, as named in the global environment.
#'
#' @return Data frame with one row per USUBJID/subject per day, with Malaria and VL
#'   specific SATERMs as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_SA_MV_FU <- function(DATA_SA) {
  DATA_SA <- DATA_SA %>%
    convert_blanks_to_na() %>%
    mutate(
      SASTRES = str_to_upper(as.character(.data$SADECOD)),
      SAMODIFY = str_to_upper(as.character(.data$SAMODIFY)),
      SATERM = str_to_upper(as.character(.data$SATERM)),
      DAY = .data$SADY,
      START_DAY = .data$SASTDY,
      END_DAY = .data$SAENDY
    ) %>%
    CLEAN_SA()

  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] <-
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SAMODIFY"]
  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] <-
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SATERM"]

  DATA_SA <- DATA_SA %>%
    filter(.data$SASTRES %in% c("MALARIA"))

  if (any(is.na(DATA_SA$SAPRESP))) {
    DATA_SA[which(is.na(DATA_SA$SAPRESP)), "SAPRESP"] <- "N"
    DATA_SA[which(DATA_SA$SAPRESP == "N"), "SAOCCUR"] <- "Y"
  }

  DATA_EMPTY <- DATA_SA %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY) &
      is.na(.data$START_DAY) & is.na(.data$END_DAY)) %>%
    DERIVE_EMPTY_TIME()

  MALARIA_SEVERE <- DATA_SA %>%
    filter(.data$SASTRES == "MALARIA" & .data$SASEV == "SEVERE") %>%
    mutate(MALARIA_SEV = "Y") %>%
    dplyr::select(.data$USUBJID, .data$MALARIA_SEV)

  DATA <- DATA_SA %>%
    left_join(DATA_EMPTY) %>%
    mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID, .data$VISITDY, .data$VISITNUM,
        .data$DAY, .data$START_DAY, .data$END_DAY, .data$EMPTY_TIME
      ),
      names_from = .data$SASTRES,
      values_from = .data$SAOCCUR,
      values_fn = first
    )

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  DATA <- DATA %>%
    left_join(MALARIA_SEVERE)

  return(DATA)
}
