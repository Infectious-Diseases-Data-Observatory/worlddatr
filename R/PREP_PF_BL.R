#' Title
#'
#' @param DATA_PF
#' @param VARS The options under PFGENTRG as characters
#'
#' @return
#' @export
#'
#' @author Rhys Peploe
#'
PREP_PF_BL = function(DATA_PF, VARS = NULL){
  PF_VARS = c(str_to_upper(VARS))

  DATA = DATA_PF %>%
    convert_blanks_to_na() %>%
    filter(.data$PFGENTRG %in% PF_VARS) %>%
    mutate(
      # PFSTRES = as.character(.data$PFSTRESN),
      PFSTRES = as.character(.data$PFSTRESC),
      PFORRES = as.character(.data$PFORRES)
      # DAY = .data$PFDY,
      # PFUNITS = as.character(NA)
    )

  # DATA[which(is.na(DATA$PFSTRES)), "PFSTRES"] <-
  #   DATA[which(is.na(DATA$PFSTRES)), "PFSTRESC"]
  DATA[which(is.na(DATA$PFSTRES)), "PFSTRES"] <-
    DATA[which(is.na(DATA$PFSTRES)), "PFORRES"]

  # DATA[which(!is.na(DATA$PFSTRESC)), "PFUNITS"] <-
  #   DATA[which(!is.na(DATA$PFSTRESC)), "PFSTRESU"]   #| !is.na(DATA$PFSTRESN)
  # DATA[which(is.na(DATA$PFSTRESC)), "PFUNITS"] <-
  #   DATA[which(is.na(DATA$PFSTRESC)), "PFORRESU"]

  DATA = DATA %>%
    group_by(USUBJID, PFGENTRG) %>%
    arrange(USUBJID, PFSTRESC) %>%
    # mutate(RESULT = str_c(unique(PFSTRES),collapse = "|"),
    #        STRESULT = NA) %>%
    mutate(ROWN = row_number()) %>%
    ungroup()

  # DATA[which(DATA$RESULT == "MUTATION|WILD TYPE" & is.na(DATA$STRESULT)), "STRESULT"] = "MUTATION"
  # DATA[which(is.na(DATA$STRESULT)), "STRESULT"] = DATA[which(is.na(DATA$STRESULT)), "RESULT"]

  DATA <- DATA %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID),    # .data$DAY,
      names_from = c(PFGENRI, .data$PFGENTRG, ROWN),
      values_from = c(.data$PFSTRES),
      values_fn = first,
      names_glue = "{PFGENRI}_{PFGENTRG}_{ROWN}",
      names_sort = TRUE
    )

  colnames(DATA) <- gsub("KELCH 13_KELCH 13", "K13", colnames(DATA))
  colnames(DATA) <- gsub("KELCH 13_", "K13_", colnames(DATA))
  # colnames(DATA) <- gsub("PFUNITS", "UNITS", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
