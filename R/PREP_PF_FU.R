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
PREP_PF_FU = function(DATA_PF, VARS = NULL){
  PF_VARS = c(str_to_upper(VARS))

  DATA = DATA_PF %>%
    convert_blanks_to_na() %>%
    filter(.data$PFGENTRG %in% PF_VARS) %>%
    mutate(
      # PFSTRES = as.character(.data$PFSTRESN),
      PFSTRES = str_to_upper(as.character(.data$PFSTRESC)),
      PFORRES = str_to_upper(as.character(.data$PFORRES))
      # DAY = .data$PFDY,
      # PFUNITS = as.character(NA)
    )

  # DATA_EMPTY <- DATA_PF %>%
  #   filter(is.na(.data$VISITDY) & is.na(.data$VISITNUM)) %>% # & is.na(.data$DAY)
  #   DERIVE_EMPTY_TIME()
  #
  # DATA <- DATA_PF %>%
  #   left_join(DATA_EMPTY)

  # DATA[which(is.na(DATA$PFSTRES)), "PFSTRES"] <-
  #   DATA[which(is.na(DATA$PFSTRES)), "PFSTRESC"]
  DATA[which(is.na(DATA$PFSTRES)), "PFSTRES"] <-
    DATA[which(is.na(DATA$PFSTRES)), "PFORRES"]

  # DATA[which(!is.na(DATA$PFSTRESC)), "PFUNITS"] <-
  #   DATA[which(!is.na(DATA$PFSTRESC)), "PFSTRESU"]   #| !is.na(DATA$PFSTRESN)
  # DATA[which(is.na(DATA$PFSTRESC)), "PFUNITS"] <-
  #   DATA[which(is.na(DATA$PFSTRESC)), "PFORRESU"]

  # DATA = DATA %>%
  #   group_by(USUBJID, PFGENTRG, VISITNUM, VISITDY, EMPTY_TIME) %>%
  #   arrange(USUBJID, PFSTRESC) %>%
  #   mutate(RESULT = str_c(unique(PFSTRES),collapse = "|"),
  #          STRESULT = NA) %>%
  #   ungroup()

  # DATA[which(DATA$RESULT == "MUTATION|WILD TYPE" & is.na(DATA$STRESULT)), "STRESULT"] = "MUTATION"
  # DATA[which(is.na(DATA$STRESULT)), "STRESULT"] = DATA[which(is.na(DATA$STRESULT)), "RESULT"]

  DATA <- DATA %>%
    pivot_wider(
      id_cols = c(
        .data$STUDYID, .data$USUBJID, .data$PARASITE),    # .data$DAY,
      names_from = c(PFGENRI, .data$PFGENTRG),
      values_from = c(.data$PFSTRES, .data$PFRESCAT),
      values_fn = first,
      names_glue = "{PFGENRI}_{PFGENTRG}_{.value}",
      names_sort = TRUE
    )

  colnames(DATA) <- gsub("KELCH 13_KELCH 13", "KELCH_13", colnames(DATA))
  colnames(DATA) <- gsub("KELCH 13_", "K13_", colnames(DATA))
  colnames(DATA) <- gsub("PFSTRES", "", colnames(DATA))

  DATA <- DATA %>%
    clean_names(case = "all_caps") %>%
    select(-c((ends_with("PFRESCAT") & starts_with("K13"))))

  return(DATA)
}
