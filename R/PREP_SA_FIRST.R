#' Prepare the SA domain for analysis on the first occurrence of events.
#'
#' Prepare the Clinical and Adverse Effects (SA) domain for use in first occurrence
#' analysis data sets. Takes a IDDO-SDTM curated SA domain, transforms and
#' pivots it in order to merge it into a first occurrence analysis data set with other
#' domains using the ANALYSE_FIRST() function.
#'
#' @param DATA_SA The SA domain data frame, as named in the global environment.
#' @param DISEASE The name of the disease theme being analysed. Character
#'   string. Default is empty (selects base variables). Select from: "MALARIA",
#'   "VL" or "EBOLA". If selection is missing or misspelt, then the default
#'   variables will be used.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for SADECOD as
#'   specified in the SA section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CRONAVIR").
#'
#' @return Dataframe containing a row per USUBJID/subject, with SATERMs and the
#'   day of first occurrence of each as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_SA_FIRST <- function(DATA_SA, DISEASE = "", VARS = NULL) {
  DISEASE <- str_to_upper(DISEASE)

  if (DISEASE == "MALARIA") {
    SA_VARS <- c(
      "FEVER", "ANEMIA", "HIV", "ANOREXIA", "DIARRHOEA", "NAUSEA", "VOMITING",
      "ABDOMINAL PAIN", "DIZZINESS", "SHORTNESS OF BREATH", "JAUNDICE",
      "DARK URINE", "ENLARGED SPLEEN", "ENLARGED LIVER", str_to_upper(VARS)
    )
  } else if (DISEASE == "VL") {
    SA_VARS <- c("FEVER", "ANEMIA", "HIV", str_to_upper(VARS))
  } else if (DISEASE == "EBOLA") {
    SA_VARS <- c(
      "FEVER", "LOSS OF APPETITE", "VOMITING", "NAUSEA AND VOMITING", "HEADACHE",
      "DIARRHOEA", "ABDOMINAL PAIN", "BLEEDING", "DIFFICULTY SWALLOWING", "HICCOUGHS",
      "DIFFICULTY BREATHING", "PAIN IN THROAT", "FATIGUE", "MUSCLE PAIN",
      "JOINT PAIN", "GENERALIZED ACHES AND PAIN", "ERUPTION OF SKIN", str_to_upper(VARS)
    )
  } else {
    SA_VARS <- c("FEVER", "ANEMIA", "HIV", str_to_upper(VARS))
  }

  DATA <- DATA_SA %>%
    convert_blanks_to_na() %>%
    mutate(
      SATERM = str_to_upper(.data$SATERM),
      SAMODIFY = str_to_upper(.data$SAMODIFY),
      SASTRES = as.character(.data$SADECOD),
      SAPRESP = str_to_upper(.data$SAPRESP),
      SAOCCUR = str_to_upper(.data$SAOCCUR),
      DAY = .data$SADY
    ) %>%
    filter(
      (.data$SACAT != "MEDICAL HISTORY" | is.na(.data$SACAT)),
      .data$SAPRESP == "Y"
    )

  DATA[which(is.na(DATA$SASTRES)), "SASTRES"] <-
    DATA[which(is.na(DATA$SASTRES)), "SAMODIFY"]
  DATA[which(is.na(DATA$SASTRES)), "SASTRES"] <-
    DATA[which(is.na(DATA$SASTRES)), "SATERM"]

  DATA <- DATA %>%
    filter(.data$SASTRES %in% SA_VARS)

  DATA <- DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$VISITDY, DATA$DAY), ]

  DATA <- DATA %>%
    pivot_wider(
      id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$SASTRES,
      names_glue = "{SASTRES}_{.value}", values_from = c(.data$SAOCCUR, .data$DAY),
      names_sort = T, names_vary = "slowest",
      values_fn = first
    )

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  colnames(DATA) <- gsub("_SAOCCUR", "", colnames(DATA))
  colnames(DATA) <- gsub("_SAPRESP", "_PRESP", colnames(DATA))

  return(DATA)
}
