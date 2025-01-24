#' Further prepare the MB domain for baseline analysis specifically for Malaria,
#' using Hour 0 as the timing filter..
#'
#' Prepare the Microbiology (MB) domain for use in baseline analysis data sets
#' with specific actions for Malaria. Instead of the typical TIMING == 1 or
#' BASELINE, this takes VISIT = HOUR 0 as the definition of baseline. Takes a
#' IDDO-SDTM curated MB domain, transforms and pivots it in order to merge it
#' into a baseline analysis data set with other domains using the
#' ANALYSE_BASELINE() function. PREP_MB_BL() and PREP_MB_MAL_BL() would be
#' merged in the ANALYSE_BASELINE() function. Default variables are: "PFALCIPA",
#' "PFALCIPS", "PFALCIP", "PVIVAXA", "PVIVAXS", "PVIVAX", "PLSMDMA", "PLSMDMS",
#' "PLSMDM", "PKNOWLA", "PKNOWLS", "PKNOWL", "PMALARA", "PMALARS", "PMALAR",
#' "POVALEA", "POVALES", "POVALE"
#'
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#'
#' @return Data frame with one row per USUBJID/subject, with Malaria specific
#'   MBTESTCDs and the units  as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MB_HR0_BL_MAL <- function(DATA_MB) {
  MB_VARS <- c(
    "PFALCIPA", "PFALCIPS", "PFALCIP",
    "PVIVAXA", "PVIVAXS", "PVIVAX",
    "PLSMDMA", "PLSMDMS", "PLSMDM",
    "PKNOWLA", "PKNOWLS", "PKNOWL",
    "PMALARA", "PMALARS", "PMALAR",
    "POVALEA", "POVALES", "POVALE"
  )

  DATA <- DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(.data$MBTESTCD %in% MB_VARS) %>%
    mutate(
      MBSTRES = str_to_upper(as.character(.data$MBSTRESN)),
      MBSTRESC = str_to_upper(as.character(.data$MBSTRESC)),
      MBMODIFY = str_to_upper(as.character(.data$MBMODIFY)),
      MBORRES = str_to_upper(as.character(.data$MBORRES)),
      MBUNITS = as.character(NA),
      TIMING = str_to_upper(as.character(.data$VISIT))
    )

  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] <-
    DATA[which(is.na(DATA$MBSTRES)), "MBSTRESC"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] <-
    DATA[which(is.na(DATA$MBSTRES)), "MBMODIFY"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] <-
    DATA[which(is.na(DATA$MBSTRES)), "MBORRES"]

  DATA[which(!is.na(DATA$MBSTRESC) | !is.na(DATA$MBSTRESN)), "MBUNITS"] <-
    DATA[which(!is.na(DATA$MBSTRESC) | !is.na(DATA$MBSTRESN)), "MBSTRESU"]
  DATA[which(is.na(DATA$MBSTRESC) & is.na(DATA$MBSTRESN)), "MBUNITS"] <-
    DATA[which(is.na(DATA$MBSTRESC) & is.na(DATA$MBSTRESN)), "MBORRESU"]

  DATA <- DATA %>%
    filter(.data$TIMING == "HOUR 0") %>% # .data$TIMING == "BASELINE"
    pivot_wider(
      id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$MBTESTCD,
      names_glue = "{MBTESTCD}_{.value}", values_from = c(.data$MBSTRES, .data$MBUNITS),
      names_sort = T, names_vary = "slowest",
      values_fn = first
    )

  colnames(DATA) <- gsub("_MBSTRES", "", colnames(DATA))
  colnames(DATA) <- gsub("MBUNITS", "UNITS", colnames(DATA))

  colnames(DATA) <- gsub("PFALCIPA", "PARA_PF", colnames(DATA))
  colnames(DATA) <- gsub("PFALCIPS", "GAM_PF", colnames(DATA))

  colnames(DATA) <- gsub("PLSMDMA", "PARA_PL", colnames(DATA))
  colnames(DATA) <- gsub("PLSMDMS", "GAM_PL", colnames(DATA))
  colnames(DATA) <- gsub("PLSMDM", "UNSP_PL", colnames(DATA))

  colnames(DATA) <- gsub("PVIVAXA", "PARA_PV", colnames(DATA))
  colnames(DATA) <- gsub("PVIVAXS", "GAM_PV", colnames(DATA))

  colnames(DATA) <- gsub("PKNOWLA", "PARA_PK", colnames(DATA))
  colnames(DATA) <- gsub("PKNOWLA", "GAM_PK", colnames(DATA))

  colnames(DATA) <- gsub("PMALARA", "PARA_PM", colnames(DATA))
  colnames(DATA) <- gsub("PMALARS", "GAM_PM", colnames(DATA))

  colnames(DATA) <- gsub("POVALEA", "PARA_PO", colnames(DATA))
  colnames(DATA) <- gsub("POVALES", "GAM_PO", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
