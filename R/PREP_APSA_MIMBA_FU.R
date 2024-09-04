#' Title
#'
#' @param DATA_APSA
#' @param DISEASE
#' @param VARS
#'
#' @return
#' @export
#'
PREP_APSA_MIMBA_FU <- function(DATA_APSA, VARS = NULL) {
  # SA_VARS <- c(str_to_upper(VARS))

  DATA_APSA <- DATA_APSA %>%
    convert_blanks_to_na() %>%
    mutate(
      SASTRES = str_to_upper(as.character(.data$SADECOD)),
      SAMODIFY = str_to_upper(as.character(.data$SAMODIFY)),
      SATERM = str_to_upper(as.character(.data$SATERM)),
      DAY = .data$SADY,
      START_DAY = .data$SASTDY,
      END_DAY = .data$SAENDY
    )

  DATA_APSA[which(is.na(DATA_APSA$SASTRES)), "SASTRES"] <-
    DATA_APSA[which(is.na(DATA_APSA$SASTRES)), "SAMODIFY"]
  DATA_APSA[which(is.na(DATA_APSA$SASTRES)), "SASTRES"] <-
    DATA_APSA[which(is.na(DATA_APSA$SASTRES)), "SATERM"]

  DATA_APSA <- DATA_APSA %>%
    # filter(.data$SASTRES %in% SA_VARS) %>%
    mutate(
      SAPRESP = str_to_upper(.data$SAPRESP),
      SAOCCUR = str_to_upper(.data$SAOCCUR),
      SAOCCUR_ANYTIME = NA
    )

  DATA_APSA$SAPRESP <- str_replace_all(DATA_APSA$SAPRESP, "TRUE", "Y")
  DATA_APSA$SAOCCUR <- str_replace_all(DATA_APSA$SAOCCUR, "TRUE", "Y")
  DATA_APSA$SAOCCUR <- str_replace_all(DATA_APSA$SAOCCUR, "FALSE", "N")
  DATA_APSA$SAOCCUR <- str_replace_all(DATA_APSA$SAOCCUR, "UNKNOWN", "U")

  if (any(is.na(DATA_APSA$SAPRESP))) {
    DATA_APSA[which(is.na(DATA_APSA$SAPRESP)), "SAPRESP"] <- "N"
    DATA_APSA[which(DATA_APSA$SAPRESP == "N"), "SAOCCUR"] <- "Y"
  }

  # DATA_EMPTY <- DATA_APSA %>%
  #   filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY) &
  #            is.na(.data$START_DAY) & is.na(.data$END_DAY)) %>%
  #   DERIVE_AP_EMPTY_TIME()
#
#   for (i in unique(DATA_APSA$APID)) {
#     LOOP_DATA <- DATA_APSA %>%
#       filter(.data$APID == i)
#
#     for (j in unique(LOOP_DATA$SASTRES)) {
#       STRES_DATA = LOOP_DATA %>%
#         filter(.data$SASTRES == j)
#
#       if(any(STRES_DATA$SAOCCUR == "Y")){
#         DATA_APSA[which(DATA_APSA$APID == i & DATA_APSA$SASTRES == j), "SAOCCUR_ANYTIME"] = "Y"
#       } else if (!any(STRES_DATA$SAOCCUR == "Y")) {
#         DATA_APSA[which(DATA_APSA$APID == i & DATA_APSA$SASTRES == j), "SAOCCUR_ANYTIME"] = "N"
#       } else {
#         DATA_APSA[which(DATA_APSA$APID == i & DATA_APSA$SASTRES == j), "SAOCCUR_ANYTIME"] = "U"
#       }
#     }
#   }

  # DATA <- DATA_APSA  %>%
  #   pivot_wider(id_cols = c(.data$STUDYID, .data$APID),
  #               names_from = .data$SASTRES,
  #               values_from = c(.data$SAPRESP, .data$SAOCCUR_ANYTIME, .data$SACAT, .data$SASCAT),
  #               names_glue = "{SASTRES}_{.value}",
  #               values_fn = first, names_vary = "slowest")

  DATA = DATA_APSA %>%
    filter(SAOCCUR != "N") %>%
    select(STUDYID, APID, SASTRES, SACAT, SASCAT, SAPRESP, SAOCCUR) %>%
    group_by(STUDYID, APID) %>%
    summarise(
      N_TERMS = n(),
      SATERM = paste(SASTRES, collapse = " | "),
      PRESP = paste(SAPRESP, collapse = " | "),
      OCCUR = paste(SAOCCUR, collapse = " | "),
      CAT = paste(SACAT, collapse = " | ")
    ) %>%
    ungroup()

  colnames(DATA) <- gsub("_SAOCCUR", "_OCCUR", colnames(DATA))
  colnames(DATA) <- gsub("SAPRESP", "PRESP", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
