PREP_IN_MIMBA_FU <- function(DATA_IN, VARS = NULL) {
  IN_VARS <- c(str_to_upper(VARS))

  DATA_IN <- DATA_IN %>%
    convert_blanks_to_na() %>%
    mutate(
      INSTRES = str_to_upper(as.character(.data$INDECOD)),
      INMODIFY = str_to_upper(as.character(.data$INMODIFY)),
      INTRT = str_to_upper(as.character(.data$INTRT)),
      DAY = .data$INDY,
      START_DAY = .data$INSTDY,
      END_DAY = .data$INENDY
    )

  DATA_IN[which(is.na(DATA_IN$INSTRES)), "INSTRES"] <-
    DATA_IN[which(is.na(DATA_IN$INSTRES)), "INMODIFY"]
  DATA_IN[which(is.na(DATA_IN$INSTRES)), "INSTRES"] <-
    DATA_IN[which(is.na(DATA_IN$INSTRES)), "INTRT"]

  DATA_IN <- DATA_IN %>%
    filter(.data$INSTRES %in% IN_VARS) %>%
    mutate(
      INPRESP = str_to_upper(.data$INPRESP),
      INOCCUR = str_to_upper(.data$INOCCUR)
    )

  DATA_IN$INPRESP <- str_replace_all(DATA_IN$INPRESP, "TRUE", "Y")
  DATA_IN$INOCCUR <- str_replace_all(DATA_IN$INOCCUR, "TRUE", "Y")
  DATA_IN$INOCCUR <- str_replace_all(DATA_IN$INOCCUR, "FALSE", "N")
  DATA_IN$INOCCUR <- str_replace_all(DATA_IN$INOCCUR, "UNKNOWN", "U")

  if (any(is.na(DATA_IN$INPRESP) == T)) {
    DATA_IN[which(is.na(DATA_IN$INPRESP)), "INPRESP"] <- "N"
    DATA_IN[which(DATA_IN$INPRESP == "N"), "INOCCUR"] <- "Y"
  }

  DATA_EMPTY <- DATA_IN %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY) &
             is.na(.data$START_DAY) & is.na(.data$END_DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_IN %>%
    left_join(DATA_EMPTY) %>%
    mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID
      ),
      names_from = .data$INSTRES, names_glue = "{INSTRES}_{.value}",
      values_from = c(.data$INOCCUR, .data$INROUTE, .data$INDTC),
      values_fn = first, names_vary = "slowest"
    )

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
