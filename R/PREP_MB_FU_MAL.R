#' Further prepare the MB domain for follow up analysis specifically for
#' Malaria.
#'
#' Prepare the Microbiology (MB) domain for use in follow up analysis data sets
#' with specific actions for Malaria. Takes a IDDO-SDTM curated MB domain,
#' transforms and pivots it in order to merge it into a follow up analysis data
#' set with other domains using the ANALYSE_FOLLOW_UP() function. PREP_MB_FU()
#' and PREP_MB_MAL_FU() would be merged in the ANALYSE_FOLLOW_UP() function.
#' Default variables are: "PFALCIPA", "PFALCIPS", "PFALCIP", "PVIVAXA",
#' "PVIVAXS", "PVIVAX", "PLSMDMA", "PLSMDMS", "PLSMDM", "PKNOWLA", "PKNOWLS",
#' "PKNOWL", "PMALARA", "PMALARS", "PMALAR", "POVALEA", "POVALES", "POVALE"
#'
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#'
#' @return Data frame with one row per USUBJID/subject per day, with Malaria
#'   specific MBTESTCDs  and the units as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MB_FU_MAL <- function(DATA_MB) {
  MB_VARS <- c(
    "PFALCIPA", "PFALCIPS", "PFALCIP",
    "PVIVAXA", "PVIVAXS", "PVIVAX",
    "PLSMDMA", "PLSMDMS", "PLSMDM",
    "PKNOWLA", "PKNOWLS", "PKNOWL",
    "PMALARA", "PMALARS", "PMALAR",
    "POVALEA", "POVALES", "POVALE"
  )

  DATA_MB <- DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(.data$MBTESTCD %in% MB_VARS) %>%
    mutate(
      MBSTRES = str_to_upper(as.character(.data$MBSTRESN)),
      MBSTRESC = str_to_upper(as.character(.data$MBSTRESC)),
      MBMODIFY = str_to_upper(as.character(.data$MBMODIFY)),
      MBORRES = str_to_upper(as.character(.data$MBORRES)),
      DAY = .data$MBDY,
      MBUNITS = as.character(NA)
    )

  DATA_EMPTY <- DATA_MB %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_MB %>%
    left_join(DATA_EMPTY)

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
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID, .data$VISITDY, .data$VISITNUM,
        .data$DAY, .data$EMPTY_TIME
      ),
      names_from = .data$MBTESTCD, values_from = c(.data$MBSTRES, .data$MBUNITS),
      names_sort = T, names_vary = "slowest", names_glue = "{MBTESTCD}_{.value}",
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
