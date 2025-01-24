PREP_DD_FU <- function(DATA_DD, VARS = NULL) {
  DD_VARS <- c(
    str_to_upper(VARS)
  )

  DATA_DD <- DATA_DD %>%
    convert_blanks_to_na() %>%
    filter(.data$DDTESTCD %in% DD_VARS) %>%
    mutate(
      DDSTRES = str_to_upper(as.character(.data$DDSTRESC)),
      DDORRES = str_to_upper(as.character(.data$DDORRES)),
      DAY = .data$DDDY
    )

  DATA_EMPTY <- DATA_DD %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_DD %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$DDSTRES)), "DDSTRES"] <-
    DATA[which(is.na(DATA$DDSTRES)), "DDORRES"]

  DATA <- DATA %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID, .data$VISITDY, .data$VISITNUM,
        .data$DAY, .data$EMPTY_TIME
      ), names_from = .data$DDTESTCD,
      values_from = .data$DDSTRES,
      names_sort = TRUE, names_vary = "slowest",
      values_fn = first
    )

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
