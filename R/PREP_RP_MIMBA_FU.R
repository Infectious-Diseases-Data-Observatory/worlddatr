PREP_RP_MIMBA_FU <- function(DATA_RP, VARS = NULL) {
  RP_VARS <- c("PREGIND", "EGESTAGE", str_to_upper(VARS))

  DATA_RP <- DATA_RP %>%
    convert_blanks_to_na() %>%
    filter(.data$RPTESTCD %in% RP_VARS) %>%
    mutate(
      RPSTRES = as.character(.data$RPSTRESN),
      RPSTRESC = as.character(.data$RPSTRESC),
      RPORRES = as.character(.data$RPORRES),
      DAY = .data$RPDY,
      RPUNITS = as.character(NA)
    )

  DATA_RP$RPSTRESC <- str_replace_all(DATA_RP$RPSTRESC, "NEGATIVE", "N")

  DATA_EMPTY <- DATA_RP %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_RP %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$RPSTRES)), "RPSTRES"] <-
    DATA[which(is.na(DATA$RPSTRES)), "RPSTRESC"]
  DATA[which(is.na(DATA$RPSTRES)), "RPSTRES"] <-
    DATA[which(is.na(DATA$RPSTRES)), "RPORRES"]

  DATA[which(!is.na(DATA$RPSTRESC) | !is.na(DATA$RPSTRESN)), "RPUNITS"] <-
    DATA[which(!is.na(DATA$RPSTRESC) | !is.na(DATA$RPSTRESN)), "RPSTRESU"]
  DATA[which(is.na(DATA$RPSTRESC) & is.na(DATA$RPSTRESN)), "RPUNITS"] <-
    DATA[which(is.na(DATA$RPSTRESC) & is.na(DATA$RPSTRESN)), "RPORRESU"]

  if ("EGESTAGE" %in% DATA$RPTESTCD){
    DATA_exc <- DATA %>%
      filter(RPTESTCD != "EGESTAGE") %>%
      pivot_wider(
        id_cols = c(
          .data$STUDYID, .data$USUBJID
        ), names_from = .data$RPTESTCD,
        values_from = c(.data$RPSTRES, .data$RPMETHOD, .data$RPDTC, .data$RPDY), names_vary = "slowest",
        names_sort = TRUE, values_fn = first,names_glue = "{RPTESTCD}_{.value}"
      )

    DATA_inc <- DATA %>%
      filter(RPTESTCD == "EGESTAGE") %>%
      pivot_wider(
        id_cols = c(
          .data$STUDYID, .data$USUBJID
        ), names_from = c(.data$RPTESTCD, .data$VISITNUM),
        values_from = c(.data$RPSTRES, .data$RPMETHOD, .data$RPDTC, .data$RPDY), names_vary = "slowest",
        names_sort = TRUE, names_glue = "{RPTESTCD}_{VISITNUM}_{.value}"
      )

    DATA <- full_join(DATA_exc, DATA_inc)

  } else{
    DATA <- DATA %>%
      pivot_wider(
        id_cols = c(
          .data$STUDYID, .data$USUBJID, .data$RPDTC
        ), names_from = .data$RPTESTCD,
        values_from = c(.data$RPSTRES, .data$RPMETHOD, .data$RPDTC, .data$RPDY), names_vary = "slowest",
        names_sort = T, values_fn = first, names_glue = "{RPTESTCD}_{.value}"
      )
  }

  colnames(DATA) <- gsub("EGESTAGE_1", "EGESTAGE_ENROL", colnames(DATA))
  colnames(DATA) <- gsub("EGESTAGE_2", "EGESTAGE_DELIVERY", colnames(DATA))
  colnames(DATA) <- gsub("RPMETHOD", "METHOD", colnames(DATA))
  colnames(DATA) <- gsub("_RPSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("RPDTC", "DTC", colnames(DATA))
  colnames(DATA) <- gsub("RPDY", "DAY", colnames(DATA))

  # if ("EGESTAGE" %in% names(DATA)) {
  #   DATA <- DATA %>%
  #     rename(
  #       "EGA" = "EGESTAGE",
  #       "EGA_UNITS" = "EGESTAGE_UNITS"
  #     )
  # }

  return(DATA)
}
