#' Further prepare the SA domain for baseline analysis specifically for Malaria
#' and VL variables.
#'
#' Prepare the Clinical and Adverse Effects (SA) domain for use in baseline
#' analysis data sets focussing on Malaria and Visceral Leishmaniasis (VL).
#' Takes a IDDO-SDTM curated SA domain, transforms and pivots it in order to
#' merge it into a baseline analysis data set with other domains using the
#' ANALYSE_BASELINE() function. PREP_SA_BL() and PREP_SA_MV_BL() would be merged
#' in the ANALYSE_BASELINE() function.
#'
#' @param DATA_SA The SA domain data frame, as named in the global environment.
#' @param inc_DUR Should the analysis dataset include the duration of the event?
#'   This is the time from the start of the event till the end. Boolean, default
#'   is FALSE.
#' @param inc_TIME Should the analysis dataset include the time since the event?
#'   This is the time since the end of the event. Boolean, default is FALSE.
#'
#' @return Data frame with one row per USUBJID/subject, with Malaria and VL
#'   specific SATERMs as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_SA_MV_BL <- function(DATA_SA, inc_DUR = FALSE, inc_TIME = FALSE) {
  DATA_SA <- DATA_SA %>%
    convert_blanks_to_na() %>%
    mutate(
      SASTRES = str_to_upper(as.character(.data$SADECOD)),
      SAMODIFY = str_to_upper(as.character(.data$SAMODIFY)),
      SATERM = str_to_upper(as.character(.data$SATERM)),
      SACAT = str_to_upper(.data$SACAT)
    ) %>%
    CLEAN_SA()

  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] <-
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SAMODIFY"]
  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] <-
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SATERM"]

  DATA_SA <- DATA_SA %>%
    filter(.data$SASTRES %in% c("MALARIA")) %>%
    DERIVE_TIMING()

  if (any(is.na(DATA_SA$SAPRESP))) {
    DATA_SA[which(is.na(DATA_SA$SAPRESP)), "SAPRESP"] <- "N"
    DATA_SA[which(DATA_SA$SAPRESP == "N"), "SAOCCUR"] <- "Y"
  }

  if (inc_DUR == FALSE & inc_TIME == FALSE) {
    if ("SACAT" %in% names(DATA_SA)) {
      DATA_HIST <- DATA_SA %>%
        filter(.data$SACAT == "MEDICAL HISTORY") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "HISTORY_{SASTRES}_{.value}",
          values_from = .data$SAOCCUR,
          values_fn = first
        )

      DATA_SAE <- DATA_SA %>%
        filter((.data$SACAT != "MEDICAL HISTORY" | is.na(.data$SACAT) == TRUE) &
          (.data$TIMING == "1" | .data$TIMING == "BASELINE")) %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
          values_from = .data$SAOCCUR,
          values_fn = first
        )

      DATA <- full_join(DATA_HIST, DATA_SAE)
    } else {
      DATA <- DATA_SA %>%
        filter(.data$TIMING == "1" | .data$TIMING == "BASELINE") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
          values_from = .data$SAOCCUR,
          values_fn = first
        )
    }
  }

  if (inc_DUR == TRUE & inc_TIME == FALSE) {
    DATA_SA <- DATA_SA %>%
      mutate(SADUR = str_to_upper(.data$SADUR))

    if ("SACAT" %in% names(DATA_SA)) {
      DATA_HIST <- DATA_SA %>%
        filter(.data$SACAT == "MEDICAL HISTORY") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "HISTORY_{SASTRES}_{.value}",
          values_from = c(.data$SAOCCUR, .data$SADUR),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA_SAE <- DATA_SA %>%
        filter((.data$SACAT != "MEDICAL HISTORY" | is.na(.data$SACAT) == TRUE) &
          (.data$TIMING == "1" | .data$TIMING == "BASELINE")) %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
          values_from = c(.data$SAOCCUR, .data$SADUR),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA <- full_join(DATA_HIST, DATA_SAE)
    } else {
      DATA <- DATA_SA %>%
        filter(.data$TIMING == "1" | .data$TIMING == "BASELINE") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
          values_from = c(.data$SAOCCUR, .data$SADUR),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )
    }
  }

  if (inc_DUR == FALSE & inc_TIME == TRUE) {
    DATA_SA <- DATA_SA %>%
      mutate(SAEVINTX = str_to_upper(.data$SAEVINTX))

    if ("SACAT" %in% names(DATA_SA)) {
      DATA_HIST <- DATA_SA %>%
        filter(.data$SACAT == "MEDICAL HISTORY") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "HISTORY_{SASTRES}_{.value}",
          values_from = c(.data$SAOCCUR, .data$SAEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA_SAE <- DATA_SA %>%
        filter((.data$SACAT != "MEDICAL HISTORY" | is.na(.data$SACAT) == TRUE) &
          (.data$TIMING == "1" | .data$TIMING == "BASELINE")) %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
          values_from = c(.data$SAOCCUR, .data$SAEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA <- full_join(DATA_HIST, DATA_SAE)
    } else {
      DATA <- DATA_SA %>%
        filter(.data$TIMING == "1" | .data$TIMING == "BASELINE") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
          values_from = c(.data$SAOCCUR, .data$SAEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )
    }
  }

  if (inc_DUR == TRUE & inc_TIME == TRUE) {
    DATA_SA <- DATA_SA %>%
      mutate(
        SADUR = str_to_upper(.data$SADUR),
        SAEVINTX = str_to_upper(.data$SAEVINTX)
      )

    if ("SACAT" %in% names(DATA_SA)) {
      DATA_HIST <- DATA_SA %>%
        filter(.data$SACAT == "MEDICAL HISTORY") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "HISTORY_{SASTRES}_{.value}",
          values_from = c(.data$SAOCCUR, .data$SADUR, .data$SAEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA_SAE <- DATA_SA %>%
        filter((.data$SACAT != "MEDICAL HISTORY" | is.na(.data$SACAT) == TRUE) &
          (.data$TIMING == "1" | .data$TIMING == "BASELINE")) %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
          values_from = c(.data$SAOCCUR, .data$SADUR, .data$SAEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA <- full_join(DATA_HIST, DATA_SAE)
    } else {
      DATA <- DATA_SA %>%
        filter(.data$TIMING == "1" | .data$TIMING == "BASELINE") %>%
        mutate(SAOCCUR = as.factor(.data$SAOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$SASTRES, names_glue = "{SASTRES}_{.value}",
          values_from = c(.data$SAOCCUR, .data$SADUR, .data$SAEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )
    }
  }

  colnames(DATA) <- str_to_upper(colnames(DATA))

  if ("VL_SAOCCUR" %in% names(DATA)) {
    DATA <- DATA %>%
      select(-starts_with("VL"))
  }

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
