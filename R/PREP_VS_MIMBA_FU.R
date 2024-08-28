#' Title
#'
#' @param DATA_VS
#' @param VARS
#'
#' @return
#' @export
#'
PREP_VS_MIMBA_FU <- function(DATA_VS, VARS = NULL){
  VS_VARS <- c(str_to_upper(VARS))

  DATA_VS <- DATA_VS %>%
    convert_blanks_to_na() %>%
    filter(.data$VSTESTCD %in% VS_VARS) %>%
    mutate(
      VSSTRES = as.character(.data$VSSTRESN),
      VSSTRESC = as.character(.data$VSSTRESC),
      VSORRES = as.character(.data$VSORRES),
      VSORRESU = as.character(.data$VSORRESU),
      DAY = .data$VSDY,
      VSUNITS = as.character(NA)
    )

  DATA_EMPTY <- DATA_VS %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_VS %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] <-
    DATA[which(is.na(DATA$VSSTRES)), "VSSTRESC"]
  DATA[which(is.na(DATA$VSSTRES)), "VSSTRES"] <-
    DATA[which(is.na(DATA$VSSTRES)), "VSORRES"]

  DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSUNITS"] <-
    DATA[which(!is.na(DATA$VSSTRESC) | !is.na(DATA$VSSTRESN)), "VSSTRESU"]
  DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSUNITS"] <-
    DATA[which(is.na(DATA$VSSTRESC) & is.na(DATA$VSSTRESN)), "VSORRESU"]

  DATA <- DATA %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID,
      ), names_from = .data$VSTESTCD,
      values_from = c(.data$VSSTRES, .data$VSUNITS, .data$VSDTC, .data$VSDY), names_vary = "slowest",
      names_sort = T, values_fn = first, names_glue = "{VSTESTCD}_{.value}"
    )

  colnames(DATA) <- gsub("_VSSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("VSDTC", "DTC", colnames(DATA))
  colnames(DATA) <- gsub("VSDY", "DAY", colnames(DATA))
  colnames(DATA) <- gsub("VSUNITS", "UNITS", colnames(DATA))

  return(DATA)
}
