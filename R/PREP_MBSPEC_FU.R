#' Prepare the Specimen Material Type (SPEC) and Specimen Collection Location (LOC) of follow up MB Tests.
#'
#' Prepare the Microbiology (MB) variables MBSPEC and MBLOC for use in follow up
#' analysis data sets. Takes a IDDO-SDTM curated MB domain, transforms and
#' pivots it in order to merge it into a follow up analysis data set with other
#' domains using the ANALYSE_FOLLOW_UP() function.
#'
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#' @param DISEASE The name of the disease theme being analysed. Character
#'   string. Default is empty (selects base variables). Select from: "MALARIA" or
#'   "VL". If selection is missing or misspelt, then the default
#'   variables will be used.
#' @param VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use controlled terminology for MBTESTCD as
#'   specified in the LB section of the 'IDDO SDTM Implementation Manual'. i.e.
#'   c("CRONAVIR").
#'
#' @return Data frame with row per USUBJID/subject per day and MBTESTCDs, with MBSPEC
#'   and MBLOC, as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MBSPEC_FU <- function(DATA_MB, DISEASE = "", VARS = NULL) {
  DISEASE <- str_to_upper(DISEASE)

  if (DISEASE == "MALARIA") {
    MB_VARS <- c("HIV", "AFB", "MTB", str_to_upper(VARS))
  } else if (DISEASE == "VL") {
    MB_VARS <- c("AFB", "HIV", "MTB", "PLSMDM", "PVIVAX", str_to_upper(VARS))
  } else {
    MB_VARS <- c("HIV", "AFB", "MTB", str_to_upper(VARS))
  }

  DATA_MB <- DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(.data$MBTESTCD %in% MB_VARS) %>%
    mutate(DAY = .data$MBDY)

  DATA_EMPTY <- DATA_MB %>%
    filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM) & is.na(.data$DAY)) %>%
    DERIVE_EMPTY_TIME()

  DATA <- DATA_MB %>%
    left_join(DATA_EMPTY)

  DATA <- DATA %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID, .data$VISITDY,
        .data$VISITNUM, .data$DAY, .data$EMPTY_TIME
      ),
      names_from = .data$MBTESTCD, values_from = c(.data$MBLOC, .data$MBSPEC),
      names_sort = TRUE, names_vary = "slowest",
      values_fn = first
    )

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  colnames(DATA) <- gsub("MBLOC", "LOC", colnames(DATA))
  colnames(DATA) <- gsub("MBSPEC", "SPEC", colnames(DATA))

  if ("AFB_LOC" %in% names(DATA) | "MTB_LOC" %in% names(DATA)) {
    if ("AFB_LOC" %in% names(DATA) & "MTB_LOC" %in% names(DATA)) {
      DATA <- DATA %>%
        mutate(
          TB_LOC = as.character(.data$MTB_LOC),
          TB_SPEC = as.character(.data$MTB_SPEC),
          AFB_LOC = as.character(.data$AFB_LOC),
          AFB_SPEC = as.character(.data$AFB_SPEC),
          MB_IND = NA
        )

      DATA[which(!is.na(DATA$MTB_LOC) | !is.na(DATA$MTB_SPEC)), "MB_IND"] <- "MTB"

      DATA[which(is.na(DATA$MB_IND)), "TB_LOC"] <-
        DATA[which(is.na(DATA$MB_IND)), "AFB_LOC"]

      DATA[which(is.na(DATA$MB_IND)), "TB_SPEC"] <-
        DATA[which(is.na(DATA$MB_IND)), "AFB_SPEC"]

      DATA <- DATA %>%
        dplyr::select(
          -"AFB_LOC", -"MTB_LOC", -"MB_IND",
          -"AFB_SPEC", -"MTB_SPEC"
        )
    } else if ("AFB_LOC" %in% names(DATA) & "MTB_LOC" %!in% names(DATA)) {
      DATA <- DATA %>%
        rename(
          "TB_LOC" = "AFB_LOC",
          "TB_SPEC" = "AFB_SPEC"
        )
    } else if ("AFB_LOC" %!in% names(DATA) & "MTB_LOC" %in% names(DATA)) {
      DATA <- DATA %>%
        rename(
          "TB_LOC" = "MTB_LOC",
          "TB_SPEC" = "MTB_SPEC"
        )
    }
  }

  return(DATA)
}
