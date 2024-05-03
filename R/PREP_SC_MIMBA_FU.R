#' Title
#'
#' @param DATA_SC
#' @param VARS
#'
#' @return
#' @export
#'
#' @examples
PREP_SC_MIMBA_FU <- function(DATA_SC, VARS = NULL) {
  SC_VARS <- c(str_to_upper(VARS))

  DATA_SC <- DATA_SC %>%
    convert_blanks_to_na() %>%
    filter(.data$SCTESTCD %in% SC_VARS) %>%
    mutate(
      SCSTRES = as.character(str_to_upper(.data$SCSTRESC)),
      SCORRES = as.character(str_to_upper(.data$SCORRES)),
      DAY = .data$SCDY,
      SCUNITS = as.character(str_to_upper(.data$SCSTRESN)),
      SCORRESU = as.character(str_to_upper(.data$SCORRESU))
    )

  DATA_EMPTY <- DATA_SC %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_SC %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$SCSTRES)), "SCSTRES"] <-
    DATA[which(is.na(DATA$SCSTRES)), "SCORRES"]

  DATA[which(is.na(DATA$SCUNITS)), "SCUNITS"] <-
    DATA[which(is.na(DATA$SCUNITS)), "SCORRESU"]

  if ("DSTHOSP" %in% SC_VARS) {
    DATA <- DATA %>%
      filter(.data$SCTESTCD == "DSTHOSP") %>%
      pivot_wider(
        id_cols = c(
          .data$STUDYID, .data$USUBJID
        ),
        names_from = .data$SCTESTCD, values_from = c(.data$SCSTRES, .data$SCUNITS),
        names_sort = TRUE, names_vary = "slowest",
        values_fn = first
      )
  } else {
    DATA <- DATA %>%
      filter(.data$SCTESTCD != "DSTHOSP") %>%
      pivot_wider(
        id_cols = c(
          .data$STUDYID, .data$USUBJID
        ),
        names_from = .data$SCTESTCD, values_from = .data$SCSTRES,
        names_sort = TRUE, names_vary = "slowest",
        values_fn = first
      )
  }

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
