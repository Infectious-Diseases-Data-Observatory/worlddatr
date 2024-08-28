#' Title
#'
#' @param DATA_DS
#' @param VARS
#'
#' @return
#' @export
#'
#' @examples
PREP_DS_MIMBA_FU <- function(DATA_DS, VARS = NULL) {
  DS_VARS <- c(str_to_upper(VARS))

  DATA_DS <- DATA_DS %>%
    convert_blanks_to_na() %>%
    mutate(
      DSSTRES = str_to_upper(as.character(.data$DSDECOD)),
      DSMODIFY = str_to_upper(as.character(.data$DSMODIFY)),
      DSTERM = str_to_upper(as.character(.data$DSTERM)),
      DAY = .data$DSDY,
      START_DAY = .data$DSSTDY
    )

  DATA_DS[which(is.na(DATA_DS$DSSTRES)), "DSSTRES"] <-
    DATA_DS[which(is.na(DATA_DS$DSSTRES)), "DSMODIFY"]
  DATA_DS[which(is.na(DATA_DS$DSSTRES)), "DSSTRES"] <-
    DATA_DS[which(is.na(DATA_DS$DSSTRES)), "DSTERM"]

  DATA_EMPTY <- DATA_DS %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY) &
             is.na(.data$START_DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_DS %>%
    left_join(DATA_EMPTY) %>%
    filter(DSSTRES %in% DS_VARS) %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID,
      ),
      names_from = .data$DSSTRES, values_from = c(.data$DSDTC, .data$DSDY, .data$DSSTDTC, .data$DSSTDY),
      names_vary = "slowest", names_glue = "{DSSTRES}_{.value}")

  colnames(DATA) <- gsub("DSDTC", "DTC", colnames(DATA))
  colnames(DATA) <- gsub("DSSTDTC", "STDTC", colnames(DATA))
  colnames(DATA) <- gsub("DSDY", "DAY", colnames(DATA))
  colnames(DATA) <- gsub("DSSTDY", "START_DAY", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}

