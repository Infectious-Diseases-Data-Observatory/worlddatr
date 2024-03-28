#' Prepare the IN domain for baseline analysis.
#'
#' Prepare the Treatments and Interventions (IN) domain for use in baseline
#' analysis data sets. Takes a IDDO-SDTM curated IN domain, transforms and
#' pivots it in order to merge it into a baseline analysis data set with other
#' domains using the ANALYSE_BASELINE() function.
#'
#' @param DATA_IN The IN domain data frame, as named in the global environment.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for INDECOD as
#'   specified in the IN section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("PARACETAMOL").
#' @param inc_DUR Should the analysis dataset include the duration of the event?
#'   This is the time from the start of the event till the end. Boolean, default
#'   is FALSE.
#' @param inc_TIME Should the analysis dataset include the time since the event?
#'   This is the time since the end of the event. Boolean, default is FALSE.
#'
#' @return Dataframe containing a row per USUBJID, with IN terms as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_IN_BL <- function(DATA_IN, VARS = NULL, inc_DUR = FALSE, inc_TIME = FALSE) {
  IN_VARS <- str_to_upper(VARS)

  DATA_IN <- DATA_IN %>%
    convert_blanks_to_na() %>%
    mutate(
      INSTRES = str_to_upper(as.character(.data$INDECOD)),
      INMODIFY = as.character(.data$INMODIFY),
      INTRT = as.character(.data$INTRT)
    )

  DATA_IN[which(is.na(DATA_IN$INSTRES)), "INSTRES"] <-
    DATA_IN[which(is.na(DATA_IN$INSTRES)), "INMODIFY"]
  DATA_IN[which(is.na(DATA_IN$INSTRES)), "INSTRES"] <-
    DATA_IN[which(is.na(DATA_IN$INSTRES)), "INTRT"]

  DATA_IN <- DATA_IN %>%
    filter(.data$INSTRES %in% IN_VARS) %>%
    DERIVE_TIMING() %>%
    mutate(
      INPRESP = str_to_upper(.data$INPRESP),
      INOCCUR = str_to_upper(.data$INOCCUR)
    )

  DATA_IN$INPRESP <- str_replace_all(DATA_IN$INPRESP, "TRUE", "Y")
  DATA_IN$INOCCUR <- str_replace_all(DATA_IN$INOCCUR, "TRUE", "Y")
  DATA_IN$INOCCUR <- str_replace_all(DATA_IN$INOCCUR, "FALSE", "N")
  DATA_IN$INOCCUR <- str_replace_all(DATA_IN$INOCCUR, "UNKNOWN", "U")

  if (any(is.na(DATA_IN$INPRESP) == T)) {
    DATA_IN[which(is.na(DATA_IN$INPRESP)), "INPRESP"] <- "N"
    DATA_IN[which(DATA_IN$INPRESP == "N"), "INOCCUR"] <- "Y"
  }

  if (inc_DUR == FALSE & inc_TIME == FALSE) {
    if ("INCAT" %in% names(DATA_IN)) {
      DATA_HIST <- DATA_IN %>%
        filter(.data$INCAT == "MEDICAL HISTORY") %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "HISTORY_{INSTRES}_{.value}",
          values_from = .data$INOCCUR,
          values_fn = first
        )

      DATA_INT <- DATA_IN %>%
        filter((.data$INCAT != "MEDICAL HISTORY" | is.na(.data$INCAT)) &
          (.data$TIMING == 1 | .data$TIMING == "BASELINE")) %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "{INSTRES}_{.value}",
          values_from = .data$INOCCUR,
          values_fn = first
        )

      DATA <- full_join(DATA_HIST, DATA_INT)
    } else {
      DATA <- DATA_IN %>%
        filter(.data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "{INSTRES}_{.value}",
          values_from = .data$INOCCUR,
          values_fn = first
        )
    }
  }

  if (inc_DUR == TRUE & inc_TIME == FALSE) {
    DATA_IN <- DATA_IN %>%
      mutate(INDUR = str_to_upper(.data$INDUR))

    if ("INCAT" %in% names(DATA_IN)) {
      DATA_HIST <- DATA_IN %>%
        filter(.data$INCAT == "MEDICAL HISTORY") %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "HISTORY_{INSTRES}_{.value}",
          values_from = c(.data$INOCCUR, .data$INDUR),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA_INT <- DATA_IN %>%
        filter((.data$INCAT != "MEDICAL HISTORY" | is.na(.data$INCAT)) &
          (.data$TIMING == 1 | .data$TIMING == "BASELINE")) %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "{INSTRES}_{.value}",
          values_from = c(.data$INOCCUR, .data$INDUR),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA <- full_join(DATA_HIST, DATA_INT)
    } else {
      DATA <- DATA_IN %>%
        filter(.data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = c(.data$INOCCUR, .data$INDUR),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )
    }
  }

  if (inc_DUR == FALSE & inc_TIME == TRUE) {
    DATA_IN <- DATA_IN %>%
      mutate(INEVINTX = str_to_upper(.data$INEVINTX))

    if ("INCAT" %in% names(DATA_IN)) {
      DATA_HIST <- DATA_IN %>%
        filter(.data$INCAT == "MEDICAL HISTORY") %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "HISTORY_{INSTRES}_{.value}",
          values_from = c(.data$INOCCUR, .data$INEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA_INT <- DATA_IN %>%
        filter((.data$INCAT != "MEDICAL HISTORY" | is.na(.data$INCAT)) &
          (.data$TIMING == 1 | .data$TIMING == "BASELINE")) %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "{INSTRES}_{.value}",
          values_from = c(.data$INOCCUR, .data$INEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA <- full_join(DATA_HIST, DATA_INT)
    } else {
      DATA <- DATA_IN %>%
        filter(.data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "{INSTRES}_{.value}",
          values_from = c(.data$INOCCUR, .data$INEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )
    }
  }

  if (inc_DUR == TRUE & inc_TIME == TRUE) {
    DATA_IN <- DATA_IN %>%
      mutate(
        INDUR = str_to_upper(.data$INDUR),
        INEVINTX = str_to_upper(.data$INEVINTX)
      )

    if ("INCAT" %in% names(DATA_IN)) {
      DATA_HIST <- DATA_IN %>%
        filter(.data$INCAT == "MEDICAL HISTORY") %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "HISTORY_{INSTRES}_{.value}",
          values_from = c(.data$INOCCUR, .data$INDUR, .data$INEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA_INT <- DATA_IN %>%
        filter((.data$INCAT != "MEDICAL HISTORY" | is.na(.data$INCAT)) &
          (.data$TIMING == 1 | .data$TIMING == "BASELINE")) %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "{INSTRES}_{.value}",
          values_from = c(.data$INOCCUR, .data$INDUR, .data$INEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )

      DATA <- full_join(DATA_HIST, DATA_INT)
    } else {
      DATA <- DATA_IN %>%
        filter(.data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
        mutate(INOCCUR = as.factor(.data$INOCCUR)) %>%
        pivot_wider(
          id_cols = c(.data$STUDYID, .data$USUBJID),
          names_from = .data$INSTRES, names_glue = "{INSTRES}_{.value}",
          values_from = c(.data$INOCCUR, .data$INDUR, .data$INEVINTX),
          names_sort = T, names_vary = "slowest",
          values_fn = first
        )
    }
  }

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
