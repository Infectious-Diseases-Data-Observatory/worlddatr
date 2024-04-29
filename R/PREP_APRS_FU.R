PREP_APRS_FU <- function(DATA_RS, VARS = NULL) {
  RS_VARS <- c(
    str_to_upper(VARS)
  )

  DATA_RS <- DATA_RS %>%
    convert_blanks_to_na() %>%
    filter(.data$RSTESTCD %in% RS_VARS) %>%
    mutate(
      RSSTRES = as.character(.data$RSSTRESC),
      RSORRES = as.character(.data$RSORRES),
      DAY = .data$RSDY
    )

  DATA_EMPTY <- DATA_RS %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_AP_EMPTY_TIME()

  DATA <- DATA_RS %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$RSSTRES)), "RSSTRES"] <-
    DATA[which(is.na(DATA$RSSTRES)), "RSORRES"]



    DATA <- DATA %>%
      mutate(RSSTRES = str_to_upper(.data$RSSTRES),
             RSTEST_TIME = paste(RSTESTCD, RSEVINTX)) %>%
      pivot_wider(
        id_cols = c(
          .data$STUDYID, .data$APID, .data$RSUBJID
        ), names_from = .data$RSTEST_TIME,
        values_from = .data$RSSTRES,
        names_sort = TRUE, names_vary = "slowest", values_fn = min
      )



    colnames(DATA) <- gsub(" 1 MINUTE POST BIRTH", "-1M", colnames(DATA))
    colnames(DATA) <- gsub(" 10 MINUTES POST BIRTH", "-10M", colnames(DATA))
    colnames(DATA) <- gsub(" 5 MINUTES POST BIRTH", "-5M", colnames(DATA))

    colnames(DATA) <- gsub(" INFANT EXAMINATION 12 MONTHS POST DELIVERY", "-M12", colnames(DATA))
    colnames(DATA) <- gsub(" INFANT EXAMINATION 6 MONTHS POST DELIVERY", "-M6", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
