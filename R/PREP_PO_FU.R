#' Prepare the PO domain for follow up analysis.
#'
#' Prepare the Pregnancy Outcomes (SC) domain for use in follow up analysis data
#' sets. Takes a IDDO-SDTM curated PO domain, transforms and pivots it in order
#' to merge it into a follow up analysis data set with other domains using the
#' ANALYSE_FOLLOW_UP() function.
#'
#' @param DATA_PO The PO domain data frame, as named in the global environment.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for POTERM as
#'   specified in the PO section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("ADEVAIND").
#'
#' @return Data frame with one row per USUBJID/subject per day and POTERMs as
#'   columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_PO_FU <- function(DATA_PO, VARS = NULL) {
  PO_VARS <- c(str_to_upper(VARS))

  DATA_PO <- DATA_PO %>%
    convert_blanks_to_na() %>%
    mutate(
      POSTRES = str_to_upper(as.character(.data$POMODIFY)),
      POTERM = str_to_upper(as.character(.data$POTERM)),
      DAY = .data$PODY,
      START_DAY = .data$POSTDY,
      END_DAY = .data$POENDY
    )

  DATA_PO[which(is.na(DATA_PO$POSTRES)), "POSTRES"] <-
    DATA_PO[which(is.na(DATA_PO$POSTRES)), "POTERM"]

  DATA_PO <- DATA_PO %>%
    filter(.data$POSTRES %in% PO_VARS) %>%
    mutate(
      POPRESP = str_to_upper(.data$POPRESP),
      POOCCUR = str_to_upper(.data$POOCCUR)
    )

  DATA_PO$POPRESP <- str_replace_all(DATA_PO$POPRESP, "TRUE", "Y")
  DATA_PO$POOCCUR <- str_replace_all(DATA_PO$POOCCUR, "TRUE", "Y")
  DATA_PO$POOCCUR <- str_replace_all(DATA_PO$POOCCUR, "FALSE", "N")
  DATA_PO$POOCCUR <- str_replace_all(DATA_PO$POOCCUR, "UNKNOWN", "U")

  if (any(is.na(DATA_PO$POPRESP))) {
    DATA_PO[which(is.na(DATA_PO$POPRESP)), "POPRESP"] <- "N"
    DATA_PO[which(DATA_PO$POPRESP == "N"), "POOCCUR"] <- "Y"
  }

  DATA_EMPTY <- DATA_PO %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY) &
      is.na(.data$START_DAY) & is.na(.data$END_DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_PO %>%
    left_join(DATA_EMPTY) %>%
    mutate(POOCCUR = as.factor(.data$POOCCUR)) %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID, .data$VISITDY, .data$VISITNUM,
        .data$DAY, .data$START_DAY, .data$END_DAY, .data$EMPTY_TIME
      ),
      names_from = .data$POSTRES, values_from = .data$POOCCUR,
      values_fn = first
    )

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
# "STILL BIRTH", "MISCARRIAGE, <ANOMALIES>
