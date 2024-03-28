#' Create outcome analysis dataset.
#'
#' Outcome analysis datasets include information about the initial and final
#' outcomes of studies. This function prepares, cleans, filters and pivots
#' multiple IDDO-SDTM domains and finally merges them into a single dataset,
#' which can be used for research and analysis. The choice of DISEASE preselects
#' a number of variables which have been chosen with input from Subject Matter
#' Experts. The DM, Ds and RS Domains are required, even if empty.
#'
#' If issues are found with the code, please log them at:
#' https://github.com/RhysPeploe/iddoverse/issues
#'
#' @param DATA_DM The DM domain data frame, as named in the global environment.
#'   Required.
#' @param DATA_DS The DS domain data frame, as named in the global environment.
#' @param DATA_RS The RS domain data frame, as named in the global environment.
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#' @param DM_VARS Specify additional variables to be included in the output
#'   dataset. Character string. Use column names as specified in the DM section
#'   of the 'IDDO SDTM Implementation Manual'. i.e. c("AGE").
#' @param expand_cols Boolean option to include all DS and RS entries for each
#'   subject. Default is FALSE, which will display the first recorded RS events
#'   and last DS event, along with the associated VISITNUM, VISITDY and DAY. If
#'   TRUE, all RS and DS events will be provided, along with the associated
#'   VISITNUM, VISITDY and DAY.
#'
#' @return A dataset with one row per subject, and variables from each of the
#'   domains listed in the parameters above.
#'
#' @export
#'
#' @author Rhys Peploe
#'
ANALYSE_OUTCOME_VL <- function(DATA_DM, DATA_DS = NULL, DATA_RS = NULL, DATA_MB = NULL,
                               DM_VARS = NULL, expand_cols = FALSE) {
  if (is.null(DATA_DS) == FALSE) {
    OUT <- PREP_DM(DATA_DM, DISEASE = "VL", VARS = c("DTHFL", "DTHDTC", str_to_upper(DM_VARS))) %>%
      left_join(PREP_DS_OUT_VL(DATA_DS, expand_cols = FALSE))
  } else if (is.null(DATA_DS)) {
    OUT <- PREP_DM(DATA_DM, DISEASE = "VL", VARS = c("DTHFL", "DTHDTC", str_to_upper(DM_VARS)))
  }

  if (is.null(DATA_RS) == FALSE) {
    TOC <- PREP_RS_OUT_VL(DATA_RS, TOC_OVRLRESP = "TOC", expand_cols = FALSE)

    OVRLRESP <- PREP_RS_OUT_VL(DATA_RS, TOC_OVRLRESP = "OVRLRESP", expand_cols = FALSE)

    if (is.null(DATA_MB) == FALSE) {
      TOC <- TOC %>%
        left_join(PREP_MB_FU_VL(DATA_MB) %>%
          rename(
            "INITIAL_TOC_VISITDY" = "VISITDY",
            "INITIAL_TOC_VISITNUM" = "VISITNUM",
            "INITIAL_TOC_DAY" = "DAY"
          ) %>%
          dplyr::select(-"EMPTY_TIME") %>%
          dplyr::rename_with(
            .fn = function(.x) {
              paste0("INITIAL_TOC_", .x)
            },
            .cols = c(
              starts_with("LDONOV"),
              starts_with("LSHMANIA")
            )
          ))

      OVRLRESP <- OVRLRESP %>%
        left_join(PREP_MB_FU_VL(DATA_MB) %>%
          rename(
            "INITIAL_OVRLRESP_VISITDY" = "VISITDY",
            "INITIAL_OVRLRESP_VISITNUM" = "VISITNUM",
            "INITIAL_OVRLRESP_DAY" = "DAY"
          ) %>%
          dplyr::select(-"EMPTY_TIME") %>%
          dplyr::rename_with(
            .fn = function(.x) {
              paste0("INITIAL_OVRLRESP_", .x)
            },
            .cols = c(
              starts_with("LDONOV"),
              starts_with("LSHMANIA")
            )
          ))
    }

    OUT <- OUT %>%
      left_join(TOC) %>%
      left_join(OVRLRESP)
  }

  if (expand_cols == TRUE) {
    if (is.null(DATA_DS) == FALSE) {
      OUT <- OUT %>%
        left_join(PREP_DS_OUT_VL(DATA_DS, expand_cols = expand_cols))
    }

    if (is.null(DATA_RS) == FALSE) {
      OUT <- OUT %>%
        left_join(PREP_RS_OUT_VL(DATA_RS, expand_cols = expand_cols))
    }
  }

  return(OUT)
}
