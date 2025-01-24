#' Prepare the LB domain for analysis on the first occurrence of events.
#'
#' Prepare the Laboratory (LB) domain for use in first occurrence
#' analysis data sets. Takes a IDDO-SDTM curated LB domain, transforms and
#' pivots it in order to merge it into a first occurrence analysis data set with other
#' domains using the ANALYSE_FIRST() function. Default variables are: "HGB", "HCT",
#' "PLAT", "WBC", "K", "ALT", "AST", "BILI", "CREAT", "ALB", "SODIUM", "HCG".
#' Disease specific features that are included by default are listed in
#' 'Details'
#'
#' Default Variables:
#'
#' Malaria: "INTLK6", "CD4", "HGB", "HGBMET", "HCT", "PLAT", "WBC", "K", "ALT",
#' "AST", "BILI", "CREAT", "ALB", "SODIUM", "HCG", "G6PD"
#'
#' VL: "INTLK6", "CD4", "HGB", "HCT", "PLAT", "WBC", "K", "ALT", "AST", "BILI",
#' "CREAT", "ALB", "SODIUM", "HCG"
#'
#' Ebola: "K", "ALT", "AST", "CREAT", "SODIUM", "UREA"
#'
#' @param DATA_LB The LB domain data frame, as named in the global environment.
#' @param DISEASE The name of the disease theme being analysed. Character
#'   string. Default is empty (selects base variables). Select from: "MALARIA",
#'   "VL" or "EBOLA". If selection is missing or misspelt, then the base
#'   variables will be used.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for LBTESTCD as
#'   specified in the LB section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CHOL").
#'
#' @return Dataframe containing a row per USUBJID/subject, with LBTESTCDs and the
#'   day of first occurrence of each as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_LB_FIRST <- function(DATA_LB, DISEASE = "", VARS = NULL) {
  DISEASE <- str_to_upper(DISEASE)

  if (DISEASE == "MALARIA") {
    LB_VARS <- c(
      "INTLK6", "CD4", "HGB", "HGBMET", "HCT", "PLAT", "WBC", "K", "ALT", "AST",
      "BILI", "CREAT", "ALB", "SODIUM", "HCG", "G6PD", str_to_upper(VARS)
    )
  } else if (DISEASE == "VL") {
    LB_VARS <- c(
      "INTLK6", "CD4", "HGB", "HCT", "PLAT", "WBC", "K", "ALT", "AST",
      "BILI", "CREAT", "ALB", "SODIUM", "HCG", str_to_upper(VARS)
    )
  } else if (DISEASE == "EBOLA") {
    LB_VARS <- c("K", "ALT", "AST", "CREAT", "SODIUM", "UREA", str_to_upper(VARS))
  } else {
    LB_VARS <- c(
      "HGB", "HCT", "PLAT", "WBC", "K", "ALT", "AST", "BILI", "CREAT",
      "ALB", "SODIUM", "HCG", str_to_upper(VARS)
    )
  }

  DATA <- DATA_LB %>%
    convert_blanks_to_na() %>%
    filter(.data$LBTESTCD %in% LB_VARS) %>%
    mutate(
      LBSTRES = str_to_upper(as.character(.data$LBSTRESN)),
      LBSTRESC = str_to_upper(as.character(.data$LBSTRESC)),
      LBORRES = str_to_upper(as.character(.data$LBORRES)),
      DAY = .data$LBDY,
      LBUNITS = as.character(NA)
    )

  DATA[which(is.na(DATA$LBSTRES)), "LBSTRES"] <-
    DATA[which(is.na(DATA$LBSTRES)), "LBSTRESC"]
  DATA[which(is.na(DATA$LBSTRES)), "LBSTRES"] <-
    DATA[which(is.na(DATA$LBSTRES)), "LBORRES"]

  DATA[which(!is.na(DATA$LBSTRESC) | !is.na(DATA$LBSTRESN)), "LBUNITS"] <-
    DATA[which(!is.na(DATA$LBSTRESC) | !is.na(DATA$LBSTRESN)), "LBSTRESU"]
  DATA[which(is.na(DATA$LBSTRESC) & is.na(DATA$LBSTRESN)), "LBUNITS"] <-
    DATA[which(is.na(DATA$LBSTRESC) & is.na(DATA$LBSTRESN)), "LBORRESU"]

  DATA <- DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$VISITDY, DATA$DAY), ]

  DATA <- DATA %>%
    pivot_wider(
      id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$LBTESTCD,
      values_from = c(.data$LBSTRES, .data$LBUNITS, .data$DAY),
      names_sort = T, names_vary = "slowest",
      values_fn = first, names_glue = "{LBTESTCD}_{.value}"
    )

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  colnames(DATA) <- gsub("_LBSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("LBUNITS", "UNITS", colnames(DATA))

  return(DATA)
}
