#' Title
#'
#' @param DATA_LB
#' @param VARS
#'
#' @return
#' @export
#'
PREP_LB_MIMBA_FU = function(DATA_LB, VARS = NULL){
  LB_VARS <- c(str_to_upper(VARS))

  DATA_LB <- DATA_LB %>%
    convert_blanks_to_na() %>%
    filter(.data$LBTESTCD %in% LB_VARS) %>%
    mutate(
      LBSTRES = as.character(.data$LBSTRESN),
      LBSTRESC = as.character(.data$LBSTRESC),
      LBORRES = as.character(.data$LBORRES),
      DAY = .data$LBDY,
      LBUNITS = as.character(NA)
    )

  DATA_EMPTY <- DATA_LB %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_LB %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$LBSTRES)), "LBSTRES"] <-
    DATA[which(is.na(DATA$LBSTRES)), "LBSTRESC"]
  DATA[which(is.na(DATA$LBSTRES)), "LBSTRES"] <-
    DATA[which(is.na(DATA$LBSTRES)), "LBORRES"]

  DATA[which(!is.na(DATA$LBSTRESC) | !is.na(DATA$LBSTRESN)), "LBUNITS"] <-
    DATA[which(!is.na(DATA$LBSTRESC) | !is.na(DATA$LBSTRESN)), "LBSTRESU"]
  DATA[which(is.na(DATA$LBSTRESC) & is.na(DATA$LBSTRESN)), "LBUNITS"] <-
    DATA[which(is.na(DATA$LBSTRESC) & is.na(DATA$LBSTRESN)), "LBORRESU"]

  DATA <- DATA %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID,
      ), names_from = .data$LBTESTCD,
      values_from = c(.data$LBSTRES, .data$LBUNITS, .data$LBDTC, .data$LBDY), names_vary = "slowest",
      names_sort = T, values_fn = first, names_glue = "{LBTESTCD}_{.value}"
    )

  colnames(DATA) <- gsub("_LBSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("LBDTC", "DTC", colnames(DATA))
  colnames(DATA) <- gsub("LBDY", "DAY", colnames(DATA))
  colnames(DATA) <- gsub("LBUNITS", "UNITS", colnames(DATA))

  return(DATA)
}
