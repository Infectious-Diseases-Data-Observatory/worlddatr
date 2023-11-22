#' Join multiple Malaria and VL variables into one for analysis.
#'
#' Medical History of Malaria and VL can be represented as a Clinical and
#' Adverse Effect (SA Domain) or as a Intervention (IN Domain). This takes
#' SAOCCUR and where NAs exist, they are populated with INOCCUR and renamed.
#' This is conducted for SADUR and INDUR, as well as SAEVINTX and INEVINTX, for
#' both Malaria and VL separately
#'
#' @param DATA Data frame including --OCCUR, --DUR or --EVINTX
#'
#' @return Data frame with combined columns, with the constituent columns
#'   removed
#'
#' @author Rhys Peploe
#'
JOIN_MV = function(DATA){
  ### Joining IN/SA Malaria and VL terms, OCCUR
  if(("HISTORY_MALARIA_SAOCCUR" %in% names(DATA)) & ("HISTORY_MALARIA_INOCCUR" %in% names(DATA))){
    DATA = DATA %>%
      mutate(HISTORY_MALARIA = as.character(.data$HISTORY_MALARIA_SAOCCUR),
             HISTORY_MALARIA_INOCCUR = as.character(.data$HISTORY_MALARIA_INOCCUR))

    DATA[which(is.na(DATA$HISTORY_MALARIA)), "HISTORY_MALARIA"] =
      DATA[which(is.na(DATA$HISTORY_MALARIA)), "HISTORY_MALARIA_INOCCUR"]

    DATA = DATA %>%
      relocate("HISTORY_MALARIA", .after = "HISTORY_MALARIA_SAOCCUR") %>%
      dplyr::select(-"HISTORY_MALARIA_SAOCCUR", -"HISTORY_MALARIA_INOCCUR")

  }

  if(("HISTORY_VL_SAOCCUR" %in% names(DATA)) & ("HISTORY_VL_INOCCUR" %in% names(DATA))){
    DATA = DATA %>%
      mutate(HISTORY_VL = as.character(.data$HISTORY_VL_SAOCCUR),
             HISTORY_VL_INOCCUR = as.character(.data$HISTORY_VL_INOCCUR))

    DATA[which(is.na(DATA$HISTORY_VL)), "HISTORY_VL"] =
      DATA[which(is.na(DATA$HISTORY_VL)), "HISTORY_VL_INOCCUR"]

    DATA = DATA %>%
      relocate("HISTORY_VL", .after = "HISTORY_VL_SAOCCUR") %>%
      dplyr::select(-"HISTORY_VL_SAOCCUR", -"HISTORY_VL_INOCCUR")
  }

  ### Joining IN/SA Malaria and VL terms, DUR
  if(("HISTORY_MALARIA_SADUR" %in% names(DATA)) & ("HISTORY_MALARIA_INDUR" %in% names(DATA))){
    DATA = DATA %>%
      mutate(HISTORY_MALARIA_DUR = .data$HISTORY_MALARIA_SADUR)

    DATA[which(is.na(DATA$HISTORY_MALARIA_DUR)), "HISTORY_MALARIA_DUR"] =
      DATA[which(is.na(DATA$HISTORY_MALARIA_DUR)), "HISTORY_MALARIA_INDUR"]

    DATA = DATA %>%
      relocate("HISTORY_MALARIA_DUR", .after = "HISTORY_MALARIA_SADUR") %>%
      dplyr::select(-"HISTORY_MALARIA_SADUR", -"HISTORY_MALARIA_INDUR")
  }

  if(("HISTORY_VL_SADUR" %in% names(DATA)) & ("HISTORY_VL_INDUR" %in% names(DATA))){
    DATA = DATA %>%
      mutate(HISTORY_VL_DUR = .data$HISTORY_VL_SADUR)

    DATA[which(is.na(DATA$HISTORY_VL_DUR)), "HISTORY_VL_DUR"] =
      DATA[which(is.na(DATA$HISTORY_VL_DUR)), "HISTORY_VL_INDUR"]

    DATA = DATA %>%
      relocate("HISTORY_VL_DUR", .after = "HISTORY_VL_SADUR") %>%
      dplyr::select(-"HISTORY_VL_SADUR", -"HISTORY_VL_INDUR")
  }

  ### Joining IN/SA Malaria and VL terms, EVINTX
  if(("HISTORY_MALARIA_SAEVINTX" %in% names(DATA)) & ("HISTORY_MALARIA_INEVINTX" %in% names(DATA))){
    DATA = DATA %>%
      mutate(HISTORY_MALARIA_TIME = .data$HISTORY_MALARIA_SAEVINTX)

    DATA[which(is.na(DATA$HISTORY_MALARIA_TIME)), "HISTORY_MALARIA_TIME"] =
      DATA[which(is.na(DATA$HISTORY_MALARIA_TIME)), "HISTORY_MALARIA_INEVINTX"]

    DATA = DATA %>%
      relocate("HISTORY_MALARIA_TIME", .after = "HISTORY_MALARIA_SAEVINTX") %>%
      dplyr::select(-"HISTORY_MALARIA_SAEVINTX", -"HISTORY_MALARIA_INEVINTX")
  }

  if(("HISTORY_VL_SAEVINTX" %in% names(DATA)) & ("HISTORY_VL_INEVINTX" %in% names(DATA))){
    DATA = DATA %>%
      mutate(HISTORY_VL_TIME = .data$HISTORY_VL_SAEVINTX)

    DATA[which(is.na(DATA$HISTORY_VL_TIME)), "HISTORY_VL_TIME"] =
      DATA[which(is.na(DATA$HISTORY_VL_TIME)), "HISTORY_VL_INEVINTX"]

    DATA = DATA %>%
      relocate("HISTORY_VL_TIME", .after = "HISTORY_VL_SAEVINTX") %>%
      dplyr::select(-"HISTORY_VL_SAEVINTX", -"HISTORY_VL_INEVINTX")
  }

  return(DATA)
}
