#' Prepare the SA domain for follow up analysis.
#'
#' Prepare the Clinical and Adverse Effects (SA) domain for use in follow up
#' analysis data sets. Takes a IDDO-SDTM curated SA domain, transforms and
#' pivots it in order to merge it into a follow up analysis data set with other
#' domains using the ANALYSE_FOLLOW_UP() function.
#'
#' @param DATA_SA The SA domain data frame, as named in the global environment.
#' @param DISEASE The name of the disease theme being analysed. Character
#'   string. Default is empty (selects base variables). Select from: "MALARIA",
#'   "VL" or "EBOLA". If selection is missing or misspelt, then the default
#'   variables will be used.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for SADECOD/SATERM as
#'   specified in the SA section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CRONAVIR").
#'
#' @return Data frame with one row per USUBJID/subject per day, with SATERMs as
#'   columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_SA_FU = function(DATA_SA, DISEASE = "", VARS = NULL){
  DISEASE = str_to_upper(DISEASE)

  if(DISEASE == "MALARIA"){
    SA_VARS = c("FEVER", "ANEMIA", "HIV", "ANOREXIA", "DIARRHOEA", "NAUSEA", "VOMITING",
                "ABDOMINAL PAIN", "DIZZINESS", "SHORTNESS OF BREATH", "JAUNDICE",
                "DARK URINE", "ENLARGED SPLEEN", "ENLARGED LIVER", str_to_upper(VARS))
  }

  else if(DISEASE == "VL"){
    SA_VARS = c("FEVER", "ANEMIA", "HIV", str_to_upper(VARS))
  }

  else if(DISEASE == "EBOLA"){
    SA_VARS = c("FEVER", "LOSS OF APPETITE", "VOMITING", "NAUSEA AND VOMITING","HEADACHE",
                "DIARRHOEA", "ABDOMINAL PAIN", "BLEEDING", "DIFFICULTY SWALLOWING", "HICCOUGHS",
                "DIFFICULTY BREATHING", "PAIN IN THROAT", "FATIGUE", "MUSCLE PAIN",
                "JOINT PAIN", "GENERALIZED ACHES AND PAIN", "ERUPTION OF SKIN", str_to_upper(VARS))
  }

  else{
    SA_VARS = c("FEVER", "ANEMIA", "HIV", str_to_upper(VARS))
  }

  DATA_SA = DATA_SA %>%
    convert_blanks_to_na() %>%
    mutate(SASTRES = str_to_upper(as.character(SADECOD)),
           SAMODIFY = str_to_upper(as.character(SAMODIFY)),
           SATERM = str_to_upper(as.character(SATERM)),
           DAY = SADY,
           START_DAY = SASTDY,
           END_DAY = SAENDY)

  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] =
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SAMODIFY"]
  DATA_SA[which(is.na(DATA_SA$SASTRES)), "SASTRES"] =
    DATA_SA[which(is.na(DATA_SA$SASTRES)), "SATERM"]

  DATA_SA = DATA_SA %>%
    filter(SASTRES %in% SA_VARS) %>%
    mutate(SAPRESP = str_to_upper(SAPRESP),
           SAOCCUR = str_to_upper(SAOCCUR))

  DATA_SA$SAPRESP = str_replace_all(DATA_SA$SAPRESP, "TRUE", "Y")
  DATA_SA$SAOCCUR = str_replace_all(DATA_SA$SAOCCUR, "TRUE", "Y")
  DATA_SA$SAOCCUR = str_replace_all(DATA_SA$SAOCCUR, "FALSE", "N")
  DATA_SA$SAOCCUR = str_replace_all(DATA_SA$SAOCCUR, "UNKNOWN", "U")

  if(any(is.na(DATA_SA$SAPRESP))) {
    DATA_SA[which(is.na(DATA_SA$SAPRESP)), "SAPRESP"] = "N"
    DATA_SA[which(DATA_SA$SAPRESP == "N"), "SAOCCUR"] = "Y"
  }

  DATA_EMPTY = DATA_SA %>%
    filter(is.na(VISITDY) & is.na(VISITNUM) & is.na(DAY) & is.na(START_DAY) & is.na(END_DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA = DATA_SA %>%
    left_join(DATA_EMPTY) %>%
    mutate(SAOCCUR = as.factor(SAOCCUR)) %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID, VISITDY, VISITNUM, DAY, START_DAY, END_DAY, EMPTY_TIME),
                names_from = SASTRES, values_from = c(SAOCCUR, SAPRESP),
                names_glue = "{SASTRES}_{.value}",
                values_fn = first, names_vary = "slowest")

  colnames(DATA) = gsub("_SAOCCUR", "", colnames(DATA))
  colnames(DATA) = gsub("SAPRESP", "PRESP", colnames(DATA))

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
