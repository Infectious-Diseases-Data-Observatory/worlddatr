#' Clean IN variables before preparing the domain.
#'
#' Replaces certain character strings to standardise the INTRT column in the
#' Treatments and Interventions (IN) Domain.
#'
#' @param DATA_IN The IN domain data frame, as named in the global environment.
#'
#' @return Data frame which has been standardised in columns: INTRT, INPRESP,
#'   INOCCUR
#'
#' @export
#'
#' @author Rhys Peploe
#'
CLEAN_IN = function(DATA_IN){
  DATA = DATA_IN %>%
    mutate("INTRT" = str_to_upper("INTRT"),
           "INPRESP" = str_to_upper("INPRESP"),
           "INOCCUR" = str_to_upper("INOCCUR"))

  DATA$INTRT = str_replace_all(DATA$INTRT, "PACKED RED BLOOD CELLS", "BLOOD_TRANS")
  DATA$INTRT = str_replace_all(DATA$INTRT, "1 UNIT OF WHOLE BLOOD \\(450ML\\)", "BLOOD_TRANS")
  DATA$INTRT = str_replace_all(DATA$INTRT, "BLOOD \\(WHOLE\\)", "BLOOD_TRANS")
  DATA$INTRT = str_replace_all(DATA$INTRT, "WHOLE BLOOD 450ML", "BLOOD_TRANS")
  DATA$INTRT = str_replace_all(DATA$INTRT, "WHOLE BLOOD", "BLOOD_TRANS")
  DATA$INTRT = str_replace_all(DATA$INTRT, "FRESH BLOOD TRANSFUSION", "BLOOD_TRANS")
  DATA$INTRT = str_replace_all(DATA$INTRT, "BLOOD TRANSFUSION", "BLOOD_TRANS")
  DATA$INTRT = str_replace_all(DATA$INTRT, "TRANSFUSION", "BLOOD_TRANS")
  DATA$INTRT = str_replace_all(DATA$INTRT, "BLOOD_TRANS", "BLOOD_TRANSFUSION")

  # DATA$INTRT = str_replace_all(DATA$INTRT, "PREVIOUS MALARIA TREATMENT", "MALARIA")
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMODIAQUINE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTEMETHER LUMEFANTRINE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'COMBIART\\(ARTEMETHER/LUMEFANTRINE\\)', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTEMETHER', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTEQUIN', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTESUNATE + FANSIDAR TABS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTESUNATE 50MG TABS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTESUNATE TABS 50MG', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTESUNATE TABLETS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTESUNATE TABS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTESUNATE LABS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTESUNATE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTISUNATE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'ARTHEMISNIN + LUMEFANTRINE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'CHLOROQUINE PHOSPHATE INJECTION', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'CHLOROQINE SYP', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'CHLOROQUINE SUSP.', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'CHLOROQUINE TABS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'CHLOROQUINE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'COARTEM 2 TABS PO BID FOR 3 DAYS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'CO-ARTEM CARTEMETHER/LUMEFANTRINE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'TABS COARTEM', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'TAB COARTEM', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'COARTEM', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'FANSIDAR \\(SULPHUR DOXIN PYRIMETHAMINE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'FANSIDAR TABS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'FANSIDAR', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'MEFLOQUINE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'PRYIMETHAMINE - SULFADOXINE TABLETS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'PYREMETHAMINE TABS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'PYREMETHAMINE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'PYREMETHAMIN', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'QUININE INFUSION', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'QUININE INJ & TAB', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'QUININE TABS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'QUININE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'SULFADOXINE TABS', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'SULFADOXINE', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'SULFADOXIN', 'MALARIA')
  # DATA$INTRT = str_replace_all(DATA$INTRT, "ANY ANTIMALARIAL TREATMENT", "MALARIA")
  # DATA$INTRT = str_replace_all(DATA$INTRT, "ANTIMALARIALS", "MALARIA")
  #
  # DATA$INTRT = str_replace_all(DATA$INTRT, "HISTORY OF ANTILEISHMANIALS", "VL")
  # DATA$INTRT = str_replace_all(DATA$INTRT, "PREVIOUS KALA-AZAR TREATMENT", "VL")
  # DATA$INTRT = str_replace_all(DATA$INTRT, "ANTILEISHMANIALS", "VL")
  # DATA$INTRT = str_replace_all(DATA$INTRT, "ANTILEISHMANIAL", "VL")
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMBI+MILT', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMBISOME', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMPHOLIP', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMPHOMUL', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMPHOTERICIN B DEOXYCHOLATE', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMPHOTERICIN B LIPID EMULSION', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'LIPOSOMAL AMPHOTERICIN B', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMPHOTERICIN-B', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMPHOTERICIN B', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMPHOTERICIN', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'INJ AMPHO', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'INJ AMPHO B', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMPHO B', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMPHO-B', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMP B', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'AMP-B', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'FUNGISOME', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'FUNGIZONE', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'FUNGI', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'SODIUM STIBOGLUCONATE ANHYDROUS', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'GENERIC SSG', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'SSG', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'GLUCANTIME', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'HISTORY OF LAST VL EPISODE', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'IMPAVIDO', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'IMPOVIDO', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'MILTEFOSINE', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'MILTOFOC', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'MILTOFOS', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'SAG, MILT', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'INJ SAG', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'MILT', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'PAROMOMYCIN SULFATE', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'PAROMOMYCIN', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'PAROMOMY', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'PARMOMYC', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'PENTOSTAM', 'VL')
  # DATA$INTRT = str_replace_all(DATA$INTRT, 'SAG', 'VL')

  DATA$INPRESP = str_replace_all(DATA$INPRESP, "TRUE", "Y")

  DATA$INOCCUR = str_replace_all(DATA$INOCCUR, "TRUE", "Y")
  DATA$INOCCUR = str_replace_all(DATA$INOCCUR, "FALSE", "N")
  DATA$INOCCUR = str_replace_all(DATA$INOCCUR, "UNKNOWN", "U")

  return(DATA)
}
