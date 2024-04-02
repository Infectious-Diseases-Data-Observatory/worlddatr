PREP_MB_HR0_BL_MAL <- function(DATA_MB) {
  MB_VARS <- c(
    "PFALCIPA", "PFALCIPS", "PFALCIP",
    "PVIVAXA", "PVIVAXS", "PVIVAX",
    "PLSMDMA", "PLSMDMS", "PLSMDM",
    "PKNOWLA", "PKNOWLS", "PKNOWL",
    "PMALARA", "PMALARS", "PMALAR",
    "POVALEA", "POVALES", "POVALE"
  )

  DATA <- DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(.data$MBTESTCD %in% MB_VARS) %>%
    mutate(
      MBSTRES = as.character(.data$MBSTRESN),
      MBSTRESC = as.character(.data$MBSTRESC),
      MBMODIFY = as.character(.data$MBMODIFY),
      MBORRES = as.character(.data$MBORRES),
      MBUNITS = as.character(NA),
      TIMING = str_to_upper(as.character(.data$VISIT)),
      EPOCH = str_to_upper(as.character(.data$EPOCH))
    )

  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] <-
    DATA[which(is.na(DATA$MBSTRES)), "MBSTRESC"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] <-
    DATA[which(is.na(DATA$MBSTRES)), "MBMODIFY"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] <-
    DATA[which(is.na(DATA$MBSTRES)), "MBORRES"]

  DATA[which(!is.na(DATA$MBSTRESC) | !is.na(DATA$MBSTRESN)), "MBUNITS"] <-
    DATA[which(!is.na(DATA$MBSTRESC) | !is.na(DATA$MBSTRESN)), "MBSTRESU"]
  DATA[which(is.na(DATA$MBSTRESC) & is.na(DATA$MBSTRESN)), "MBUNITS"] <-
    DATA[which(is.na(DATA$MBSTRESC) & is.na(DATA$MBSTRESN)), "MBORRESU"]

  DATA <- DATA %>%
    mutate(MBSTRES = str_to_upper(.data$MBSTRES)) %>%
    filter(.data$TIMING == "HOUR 0") %>% # .data$TIMING == "BASELINE"
    pivot_wider(
      id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$MBTESTCD,
      names_glue = "{MBTESTCD}_{.value}", values_from = c(.data$MBSTRES, .data$MBUNITS),
      names_sort = T, names_vary = "slowest",
      values_fn = first
    )

  colnames(DATA) <- gsub("_MBSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("MBUNITS", "UNITS", colnames(DATA))

  colnames(DATA) <- gsub("PFALCIPA", "PARA_PF", colnames(DATA))
  colnames(DATA) <- gsub("PFALCIPS", "GAM_PF", colnames(DATA))

  colnames(DATA) <- gsub("PLSMDMA", "PARA_PL", colnames(DATA))
  colnames(DATA) <- gsub("PLSMDMS", "GAM_PL", colnames(DATA))
  colnames(DATA) <- gsub("PLSMDM", "UNSP_PL", colnames(DATA))

  colnames(DATA) <- gsub("PVIVAXA", "PARA_PV", colnames(DATA))
  colnames(DATA) <- gsub("PVIVAXS", "GAM_PV", colnames(DATA))

  colnames(DATA) <- gsub("PKNOWLA", "PARA_PK", colnames(DATA))
  colnames(DATA) <- gsub("PKNOWLA", "GAM_PK", colnames(DATA))

  colnames(DATA) <- gsub("PMALARA", "PARA_PM", colnames(DATA))
  colnames(DATA) <- gsub("PMALARS", "GAM_PM", colnames(DATA))

  colnames(DATA) <- gsub("POVALEA", "PARA_PO", colnames(DATA))
  colnames(DATA) <- gsub("POVALES", "GAM_PO", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
