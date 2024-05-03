#' Title
#'
#' @param DATA_IN
#'
#' @return
#' @export
#'
#' @examples
PREP_IN_MIMBA_FU = function(DATA_IN){
  IN_VARS <- str_to_upper(c(
    "Amodiaquine-artesunate",
    "Artemether",
    "Artemether-Lumefantrine",
    "Artemether/Lumefantrine Paracetamol",
    "Artemether/Lumefantrine Phloroglucinol tablet",
    "DHA-Piperaquine",
    "Pyronaridine-Artesunate",
    "Quinine",
    "Quinine (Oral)",
    "QUININE (PARENTERAL/IV)",
    "Quinine Paracetamol",
    "Phloroglucinol Quinine",
    "Paracetamol Quinine",
    "Paracetamol Phloroglucinol Artemether"
    ))

  DATA_IN$INTRT <- str_replace_all(DATA_IN$INTRT, "Artemether/Lumefantrine Paracetamol", "Artemether-Lumefantrine")
  DATA_IN$INTRT <- str_replace_all(DATA_IN$INTRT, "Artemether/Lumefantrine Phloroglucinol tablet", "Artemether-Lumefantrine")
  DATA_IN$INTRT <- str_replace_all(DATA_IN$INTRT, "Quinine \\(oral\\)", "Quinine")
  DATA_IN$INTRT <- str_replace_all(DATA_IN$INTRT, "Quinine Paracetamol", "Quinine")
  DATA_IN$INTRT <- str_replace_all(DATA_IN$INTRT, "Paracetamol Quinine", "Quinine")
  DATA_IN$INTRT <- str_replace_all(DATA_IN$INTRT, "Phloroglucinol QUININE", "Quinine")
  DATA_IN$INTRT <- str_replace_all(DATA_IN$INTRT, "Paracetamol Phloroglucinol Artemether", "Artemether")

  DATA_IN <- DATA_IN %>%
    convert_blanks_to_na() %>%
    mutate(
      INPRESP = str_to_upper(.data$INPRESP),
      INOCCUR = str_to_upper(.data$INOCCUR),
      INTRT = str_to_upper(INTRT)
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
    group_by(STUDYID, USUBJID) %>%
    mutate(ANTIMAL_SEQ = row_number()) %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID),
                names_from = ANTIMAL_SEQ,
                values_from = c(INTRT, INROUTE, INEVINTX, INSTDTC, INDUR),
                names_vary = "slowest",
                names_glue = "ANTIMALARIAL_{ANTIMAL_SEQ}_{.value}")

  DATA <- DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}
