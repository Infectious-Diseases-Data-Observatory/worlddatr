#' Title
#'
#' @param DATA_IN
#'
#' @return
#' @export
#'
PREP_IN_MIMBA_FU = function(DATA_IN){
  IN_VARS <- str_to_upper(c(
    "Amodiaquine-artesunate",
    "Artemether",
    "Artemether-Lumefantrine",
    "Artesunate",
    "DHA-Piperaquine",
    "Pyronaridine-Artesunate",
    "Quinine"
    ))

  DATA_IN[which(DATA_IN$INTRT == "ARTESUN"), "INTRT"] = "Artesunate"
  DATA_IN[which(DATA_IN$INTRT == "Artemether/Lumefantrine Paracetamol"), "INTRT"] = "Artemether-Lumefantrine"
  DATA_IN[which(DATA_IN$INTRT == "Artemether/Lumefantrine Phloroglucinol tablet"), "INTRT"] = "Artemether-Lumefantrine"
  DATA_IN[which(DATA_IN$INTRT == "Artemether/Lumefantrine"), "INTRT"] = "Artemether-Lumefantrine"
  DATA_IN[which(DATA_IN$INTRT == "Quinine (oral)"), "INTRT"] = "Quinine"
  DATA_IN[which(DATA_IN$INTRT == "Quinine Paracetamol"), "INTRT"] = "Quinine"
  DATA_IN[which(DATA_IN$INTRT == "Paracetamol Quinine"), "INTRT"] = "Quinine"
  DATA_IN[which(DATA_IN$INTRT == "Phloroglucinol QUININE"), "INTRT"] = "Quinine"
  DATA_IN[which(DATA_IN$INTRT == "Paracetamol Phloroglucinol Artemether"), "INTRT"] = "Artemether"
  DATA_IN[which(DATA_IN$INTRT == "PYRAMAX"), "INTRT"] = "Pyronaridine-Artesunate"
  DATA_IN[which(DATA_IN$INTRT == "Artusunate"), "INTRT"] = "Artesunate"
  DATA_IN[which(DATA_IN$INTRT == "BIMALARIL"), "INTRT"] = "Artemether-Lumefantrine"
  DATA_IN[which(DATA_IN$INTRT == "Coartem"), "INTRT"] = "Artemether-Lumefantrine"
  DATA_IN[which(DATA_IN$INTRT == "EURARTESIM"), "INTRT"] = "DHA-Piperaquine"
  DATA_IN[which(DATA_IN$INTRT == "Iv artesunate"), "INTRT"] = "Artesunate"
  DATA_IN[which(DATA_IN$INTRT == "AL"), "INTRT"] = "Artemether-Lumefantrine"
  DATA_IN[which(DATA_IN$INTRT == "Artesunate/Amodiaquine"), "INTRT"] = "Amodiaquine-artesunate"
  DATA_IN[which(DATA_IN$INTRT == "Quinine (IV)"), "INTRT"] = "Quinine"
  DATA_IN[which(DATA_IN$INTRT == "COMBIMAL"), "INTRT"] = "DHA-Piperaquine"
  DATA_IN[which(DATA_IN$INTRT == "MALACUR"), "INTRT"] = "DHA-Piperaquine"
  DATA_IN[which(DATA_IN$INTRT == "IV QnSo4"), "INTRT"] = "Quinine"


  DATA_IN <- DATA_IN %>%
    convert_blanks_to_na() %>%
    mutate(
      INPRESP = str_to_upper(.data$INPRESP),
      INOCCUR = str_to_upper(.data$INOCCUR),
      INTRT = str_to_upper(.data$INTRT)
    )

  DATA_IN$INPRESP <- str_replace_all(DATA_IN$INPRESP, "TRUE", "Y")
  DATA_IN$INOCCUR <- str_replace_all(DATA_IN$INOCCUR, "TRUE", "Y")
  DATA_IN$INOCCUR <- str_replace_all(DATA_IN$INOCCUR, "FALSE", "N")
  DATA_IN$INOCCUR <- str_replace_all(DATA_IN$INOCCUR, "UNKNOWN", "U")

  if (any(is.na(DATA_IN$INPRESP) == TRUE)) {
    DATA_IN[which(is.na(DATA_IN$INPRESP)), "INPRESP"] <- "N"
    DATA_IN[which(DATA_IN$INPRESP == "N"), "INOCCUR"] <- "Y"
  }

  DATA <- DATA_IN %>%
    filter(INTRT %in% IN_VARS) %>%
    group_by(.data$STUDYID, .data$USUBJID) %>%
    mutate(ANTIMAL_SEQ = row_number()) %>%
    dplyr::ungroup() %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                names_from = ANTIMAL_SEQ,
                values_from = c(.data$INTRT, .data$INROUTE, .data$INEVINTX,
                                .data$INSTDTC, .data$INDUR),
                names_vary = "slowest",
                names_glue = "ANTIMALARIAL_{ANTIMAL_SEQ}_{.value}")

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
