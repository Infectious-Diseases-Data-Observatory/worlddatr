#' Title
#'
#' @param DATA_DD
#' @param VARS
#'
#' @return
#' @export
#'
#' @examples
PREP_APDD_FU <- function(DATA_DD, VARS = NULL) {
  DD_VARS <- c(
    str_to_upper(VARS)
  )

  DATA_DD <- DATA_DD %>%
    convert_blanks_to_na() %>%
    filter(.data$DDTESTCD %in% DD_VARS) %>%
    mutate(
      DDSTRES = as.character(.data$DDSTRESC),
      DDORRES = as.character(.data$DDORRES),
      DAY = .data$DDDY
    )

  DATA_EMPTY <- DATA_DD %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_AP_EMPTY_TIME()

  DATA <- DATA_DD %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$DDSTRES)), "DDSTRES"] <-
    DATA[which(is.na(DATA$DDSTRES)), "DDORRES"]

  DATA <- DATA %>%
    mutate(DDSTRES = str_to_upper(.data$DDSTRES)) %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$APID, .data$RSUBJID
      ), names_from = .data$DDTESTCD,
      values_from = .data$DDSTRES,
      names_sort = TRUE, names_vary = "slowest", values_fn = first
    )

  # colnames(DATA) <- gsub("_DDSTRES", "", colnames(DATA))
  # colnames(DATA) <- gsub("LBUNITS", "UNITS", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
