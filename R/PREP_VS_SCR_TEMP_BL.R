PREP_VS_SCR_TEMP_BL <- function(DATA_VS) {
  DATA <- DATA_VS %>%
    convert_blanks_to_na() %>%
    filter(.data$VSTESTCD == "TEMP") %>%
    DERIVE_TIMING() %>%
    mutate(
      VSSTRES = as.character(.data$VSSTRESN),
      VSSTRESC = as.character(.data$VSSTRESC),
      VSORRES = as.character(.data$VSORRES),
      VSUNITS = as.character(NA)
    )

  DATA <- DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$VISITDY, DATA$VSDY), ]

  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] <-
    DATA[which(is.na(DATA$VSSTRES)), "VSSTRESC"]
  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] <-
    DATA[which(is.na(DATA$VSSTRES)), "VSORRES"]

  DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSUNITS"] <-
    DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSSTRESU"]
  DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSUNITS"] <-
    DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSORRESU"]

  DATA <- DATA %>%
    filter(.data$TIMING == 1 | .data$TIMING == "SCREENING") %>%
    pivot_wider(
      id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$VSTESTCD,
      values_from = c(.data$VSSTRES, .data$VSUNITS, .data$VSLOC),
      names_sort = T, names_vary = "slowest",
      values_fn = first, names_glue = "{VSTESTCD}_{.value}"
    )

  colnames(DATA) <- gsub("_VSSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("VSUNITS", "UNITS", colnames(DATA))
  colnames(DATA) <- gsub("VSLOC", "LOC", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
