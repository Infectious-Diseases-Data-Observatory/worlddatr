#' Title
#'
#' @param DATA_PO
#' @param VARS
#'
#' @return
#' @export
#'
#' @examples
PREP_PO_MIMBA_FU <- function(DATA_PO, VARS = NULL) {
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
        .data$STUDYID, .data$USUBJID,
      ),
      names_from = .data$POSTRES, values_from = c(.data$POOCCUR, .data$PODTC),
    names_vary = "slowest", names_glue = "{POSTRES}_{.value}")

  colnames(DATA) <- gsub("POOCCUR", "", colnames(DATA))
  colnames(DATA) <- gsub("PODTC", "DTC", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}

