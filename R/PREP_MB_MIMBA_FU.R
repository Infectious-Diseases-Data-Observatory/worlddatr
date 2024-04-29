PREP_MB_MIMBA_FU <- function(DATA_MB, DISEASE = "", VARS = NULL) {
  DISEASE <- str_to_upper(DISEASE)

  if (DISEASE == "VL") {
    MB_VARS <- c(
      "HIV", "AFB", "MTB", "ANCDUOD", "ANCLMTA", "ASCLUM",
      "PLSMDM", "PLSMDMA", "PLSMDMS",
      "PFALCIP", "PFALCIPA", "PFALCIPS",
      "PVIVAX", "PVIVAXA", "PVIVAXS",
      str_to_upper(VARS)
    )
  } else if (DISEASE == "EBOLA") {
    MB_VARS <- c("ZEBOV", str_to_upper(VARS))
  } else {
    MB_VARS <- c(
      "HIV", "AFB", "MTB", "ANCDUOD", "ANCLMTA", "ASCLUM",
      str_to_upper(VARS)
    )
  }

  DATA_MB <- DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(.data$MBTESTCD %in% MB_VARS) %>%
    mutate(
      MBSTRES = as.character(.data$MBSTRESN),
      MBSTRESC = as.character(.data$MBSTRESC),
      MBMODIFY = as.character(.data$MBMODIFY),
      MBORRES = as.character(.data$MBORRES),
      DAY = .data$MBDY,
      MBUNITS = as.character(NA)
    )

  DATA_EMPTY <- DATA_MB %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_MB %>%
    left_join(DATA_EMPTY)

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
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID, .data$VISITDY,
        .data$VISITNUM, .data$DAY, .data$EMPTY_TIME
      ),
      names_from = .data$MBTESTCD, values_from = c(.data$MBSTRES, .data$MBUNITS),
      names_sort = T, names_vary = "slowest",
      values_fn = first, names_glue = "{MBTESTCD}_{.value}"
    )

  colnames(DATA) <- gsub("_MBSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("MBUNITS", "UNITS", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  if ("AFB" %in% names(DATA) | "MTB" %in% names(DATA)) {
    if ("AFB" %in% names(DATA) & "MTB" %in% names(DATA)) {
      DATA <- DATA %>%
        mutate(
          TB = .data$MTB,
          TB_UNITS = .data$MTB_UNITS,
          MB_IND = NA
        )

      DATA[which(!is.na(DATA$MTB)), "MB_IND"] <- "MTB"

      DATA[which(is.na(DATA$MB_IND)), "TB"] <-
        DATA[which(is.na(DATA$MB_IND)), "AFB"]

      DATA[which(is.na(DATA$MB_IND)), "TB_UNITS"] <-
        DATA[which(is.na(DATA$MB_IND)), "AFB_UNITS"]

      DATA <- DATA %>%
        dplyr::select(
          -"AFB", -"MTB",
          -"AFB_UNITS", -"MTB_UNITS",
          -"MB_IND"
        )
    } else if ("AFB" %in% names(DATA) & "MTB" %!in% names(DATA)) {
      DATA <- DATA %>%
        rename(
          "TB" = "AFB",
          "TB_UNITS" = "AFB_UNITS"
        )
    } else if ("AFB" %!in% names(DATA) & "MTB" %in% names(DATA)) {
      DATA <- DATA %>%
        rename(
          "TB" = "MTB",
          "TB_UNITS" = "MTB_UNITS"
        )
    }
  }

  return(DATA)
}
