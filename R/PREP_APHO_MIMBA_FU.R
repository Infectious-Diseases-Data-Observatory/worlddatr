#' Title
#'
#' @param DATA_APHO
#' @param VARS
#'
#' @return
#' @export
#'
PREP_APHO_MIMBA_FU <- function(DATA_APHO, VARS = NULL){
  HO_VARS = str_to_upper(c(VARS))

  DATA_APHO <- DATA_APHO %>%
    convert_blanks_to_na() %>%
    mutate(
      HOSTRES = str_to_upper(as.character(.data$HOTERM)),
  #    SATERM = str_to_upper(as.character(.data$SATERM)),
      DAY = .data$HODY
    )

  DATA_APHO <- DATA_APHO %>%
    filter(.data$HOSTRES %in% HO_VARS) %>%
    mutate(
      HOPRESP = str_to_upper(.data$HOPRESP),
      HOOCCUR = str_to_upper(.data$HOOCCUR)
      # ,      HOOCCUR_ANYTIME = NA
    )

  # DATA_APHO$HOPRESP <- str_replace_all(DATA_APHO$HOPRESP, "TRUE", "Y")
  # DATA_APHO$HOOCCUR <- str_replace_all(DATA_APHO$HOOCCUR, "TRUE", "Y")
  # DATA_APHO$HOOCCUR <- str_replace_all(DATA_APHO$HOOCCUR, "FALSE", "N")
  # DATA_APHO$HOOCCUR <- str_replace_all(DATA_APHO$HOOCCUR, "UNKNOWN", "U")
  #
  # if (any(is.na(DATA_APHO$HOPRESP))) {
  #   DATA_APHO[which(is.na(DATA_APHO$HOPRESP)), "HOPRESP"] <- "N"
  #   DATA_APHO[which(DATA_APHO$HOPRESP == "N"), "HOOCCUR"] <- "Y"
  # }

  if (any(is.na(DATA_APHO$HOINC))) {
    DATA_APHO[which(is.na(DATA_APHO$HOINC)), "HOINC"] <-
      DATA_APHO[which(is.na(DATA_APHO$HOINC)), "HOOCCUR"]
  }

  DATA <- DATA_APHO  %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$APID),
                names_from = .data$HOSTRES,
                values_from = c(HOINC),
                # names_glue = "{HOSTRES}_{.value}",
                values_fn = first, names_vary = "slowest")

  colnames(DATA) <- gsub("_HOOCCUR", "_OCCUR", colnames(DATA))
  colnames(DATA) <- gsub("HOPRESP", "PRESP", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
