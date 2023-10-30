#' Prepare the LB domain for follow up analysis.
#'
#' Prepare the Laboratory Test Results (LB) domain for use in follow up analysis
#' data sets. Takes a IDDO-SDTM curated LB domain, transforms and pivots it in
#' order to merge it into a follow up analysis data set with other domains using
#' the ANALYSE_FOLLOW_UP() function.
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
#' @return Dataframe containing a row per USUBJID/subject per day, with
#'   LBTESTCDs as columns.
#'
#' @export
#'
#' @author Rhys Peploe
PREP_LB_FU = function(DATA_LB, DISEASE = "", VARS = NULL){
  DISEASE = str_to_upper(DISEASE)

  if(DISEASE == "MALARIA"){
    LB_VARS = c("INTLK6", "CD4", "HGB", "HGBMET", "HCT", "PLAT", "WBC", "K", "ALT", "AST",
                "BILI", "CREAT", "ALB", "SODIUM", "HCG", "G6PD", str_to_upper(VARS))
  }

  else if(DISEASE == "VL"){
    LB_VARS = c("INTLK6", "CD4", "HGB", "HCT", "PLAT", "WBC", "K", "ALT", "AST",
                "BILI", "CREAT", "ALB", "SODIUM", "HCG", str_to_upper(VARS))
  }

  else if(DISEASE == "EBOLA"){
    LB_VARS = c("K", "ALT", "AST", "CREAT", "SODIUM", "UREA", str_to_upper(VARS))
  }

  else{
    LB_VARS = c("HGB", "HCT", "PLAT", "WBC", "K", "ALT", "AST", "BILI", "CREAT",
                "ALB", "SODIUM", "HCG", str_to_upper(VARS))
  }

  DATA_LB = DATA_LB %>%
    convert_blanks_to_na() %>%
    filter(LBTESTCD %in% LB_VARS) %>%
    mutate(LBSTRES = as.character(LBSTRESN),
           LBSTRESC = as.character(LBSTRESC),
           LBORRES = as.character(LBORRES),
           DAY = LBDY)

  DATA_EMPTY = DATA_LB %>%
    filter(is.na(VISITDY) & is.na(VISITNUM) & is.na(DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA = DATA_LB %>%
    left_join(DATA_EMPTY)

  DATA[which(is.na(DATA$LBSTRES)), "LBSTRES"] =
    DATA[which(is.na(DATA$LBSTRES)), "LBSTRESC"]
  DATA[which(is.na(DATA$LBSTRES)), "LBSTRES"] =
    DATA[which(is.na(DATA$LBSTRES)), "LBORRES"]

  DATA = DATA %>%
    mutate(LBSTRES = str_to_upper(LBSTRES)) %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID, VISITDY, VISITNUM, DAY, EMPTY_TIME), names_from = LBTESTCD,
                values_from = LBSTRES,
                names_sort = T, names_vary = "slowest",
                values_fn = first)

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
