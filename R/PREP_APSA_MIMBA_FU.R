PREP_APSA_MIMBA_FU <- function(DATA_SA, DISEASE = "", VARS = NULL) {
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

  DATA_SA <- DATA_SA %>%
    convert_blanks_to_na() %>%
    mutate(
      SASTRES = str_to_upper(as.character(.data$SADECOD)),
      SAMODIFY = str_to_upper(as.character(.data$SAMODIFY)),
      SATERM = str_to_upper(as.character(.data$SATERM)),
      DAY = .data$SADY,
      START_DAY = .data$SASTDY,
      END_DAY = .data$SAENDY
    )

  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] <-
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SAMODIFY"]
  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] <-
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SATERM"]

  DATA_SA <- DATA_SA %>%
    filter(.data$SASTRES %in% SA_VARS) %>%
    mutate(
      SAPRESP = str_to_upper(.data$SAPRESP),
      SAOCCUR = str_to_upper(.data$SAOCCUR),
      SAOCCUR_ANYTIME = NA
    )

  DATA_SA$SAPRESP <- str_replace_all(DATA_SA$SAPRESP, "TRUE", "Y")
  DATA_SA$SAOCCUR <- str_replace_all(DATA_SA$SAOCCUR, "TRUE", "Y")
  DATA_SA$SAOCCUR <- str_replace_all(DATA_SA$SAOCCUR, "FALSE", "N")
  DATA_SA$SAOCCUR <- str_replace_all(DATA_SA$SAOCCUR, "UNKNOWN", "U")

  if (any(is.na(DATA_SA$SAPRESP))) {
    DATA_SA[which(is.na(DATA_SA$SAPRESP)), "SAPRESP"] <- "N"
    DATA_SA[which(DATA_SA$SAPRESP == "N"), "SAOCCUR"] <- "Y"
  }

  # DATA_EMPTY <- DATA_SA %>%
  #   filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY) &
  #            is.na(.data$START_DAY) & is.na(.data$END_DAY)) %>%
  #   DERIVE_AP_EMPTY_TIME()

  for (i in unique(DATA_SA$APID)) {
    LOOP_DATA <- DATA_SA %>%
      filter(APID == i)

    for (j in unique(LOOP_DATA$SASTRES)) {
      STRES_DATA = LOOP_DATA %>%
        filter(SASTRES == j)

      if(any(STRES_DATA$SAOCCUR == "Y")){
        DATA_SA[which(DATA_SA$APID == i & DATA_SA$SASTRES == j), "SAOCCUR_ANYTIME"] = "Y"
      } else if (!any(STRES_DATA$SAOCCUR == "Y")) {
        DATA_SA[which(DATA_SA$APID == i & DATA_SA$SASTRES == j), "SAOCCUR_ANYTIME"] = "N"
      } else {
        DATA_SA[which(DATA_SA$APID == i & DATA_SA$SASTRES == j), "SAOCCUR_ANYTIME"] = "U"
      }
    }
  }

  DATA <- DATA_SA  %>%
    pivot_wider(id_cols = c(STUDYID, APID),
                names_from = SASTRES,
                values_from = c(SAPRESP, SAOCCUR_ANYTIME, SACAT, SASCAT),
                names_glue = "{SASTRES}_{.value}",
                values_fn = first, names_vary = "slowest")

  colnames(DATA) <- gsub("_SAOCCUR", "_OCCUR", colnames(DATA))
  colnames(DATA) <- gsub("SAPRESP", "PRESP", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
