#' Prepare the ER domain for analysis on the first occurrence of events.
#'
#' Extract the first occurrence of terms in the Environmental Risk (ER) domain.
#' Takes a IDDO-SDTM curated ER domain, transforms and pivots it in order to
#' merge it into a first occurrence analysis data set with other domains using
#' the ANALYSE_FIRST() function.
#'
#' @param DATA_ER The ER domain data frame, as named in the global environment.
#'
#' @return Dataframe containing a row per USUBJID/subject, with ER terms and the
#'   day of first occurrence of each as columns.
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_ER_FIRST = function(DATA_ER){
  DATA = DATA_ER %>%
    convert_blanks_to_na() %>%
    mutate(ERSTRES = as.character(ERDECOD),
           ERPRESP = str_to_upper(ERPRESP),
           EROCCUR = str_to_upper(EROCCUR),
           DAY = ERDY) %>%
    filter(ERPRESP == "Y")

  DATA[which(is.na(DATA$ERSTRES)), "ERSTRES"] =
    DATA[which(is.na(DATA$ERSTRES)), "ERMODIFY"]
  DATA[which(is.na(DATA$ERSTRES)), "ERSTRES"] =
    DATA[which(is.na(DATA$ERSTRES)), "ERTERM"]

  DATA = DATA[order(DATA$USUBJID, DATA$VISITNUM, DATA$DAY), ]

  DATA = DATA %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID), names_from = ERSTRES, names_glue = "{ERSTRES}_{.value}",
                values_from = c(EROCCUR, DAY),
                names_sort = T, names_vary = "slowest",
                values_fn = first)

  DATA = DATA %>%
    clean_names(case = "all_caps")

  colnames(DATA) = gsub("_EROCCUR", "", colnames(DATA))
  colnames(DATA) = gsub("_ERPRESP", "_PRESP", colnames(DATA))

  return(DATA)
}
