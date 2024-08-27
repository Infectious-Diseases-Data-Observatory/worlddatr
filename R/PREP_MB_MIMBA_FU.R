#' Title
#'
#' @param DATA_MB
#' @param DISEASE
#' @param VARS
#'
#' @return
#' @export
#'
#' @examples
PREP_MB_MIMBA_FU <- function(DATA_MB, VARS = NULL) {
  MB_VARS <- c(str_to_upper(VARS))

  DATA_MB[which(DATA_MB$MBTESTCD == "HIV" &
                  DATA_MB$MBTSTDTL == "VIRAL LOAD"), "MBSTRESN"] = NA
  DATA_MB[which(DATA_MB$MBTESTCD == "HIV" &
                  DATA_MB$MBTSTDTL == "VIRAL LOAD"), "MBSTRESC"] = "POSITIVE"

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
    filter(MBSTRES == "POSITIVE") %>%
    arrange(USUBJID, MBTESTCD, MBDTC, MBDY) %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID
      ),
      names_from = .data$MBTESTCD, values_from = c(.data$MBSTRES, .data$MBDTC, .data$MBDY),
      names_sort = T, names_vary = "slowest",
       values_fn = first, names_glue = "{MBTESTCD}_{.value}"
    )

  colnames(DATA) <- gsub("_MBSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("MBUNITS", "UNITS", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
