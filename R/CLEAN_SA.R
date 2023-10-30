#' Clean SA variables before preparing the domain.
#'
#' Replaces certain character strings to standardise the SATERM column in the
#' Clinical and Adverse Effects (SA) Domain.
#'
#' @param DATA_SA The SA domain data frame, as named in the global environment.
#'
#' @return
#'
#' @export
#'
#' @author Rhys Peploe
#'
CLEAN_SA = function(DATA_SA){
  DATA = DATA_SA %>%
    mutate(SATERM = str_to_upper(SATERM),
           SAPRESP = str_to_upper(SAPRESP),
           SAOCCUR = str_to_upper(SAOCCUR))

  DATA$SATERM = str_replace_all(DATA$SATERM, "PLASMODIUM FALCIPARUM MALARIA", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "FALCIPARUM MALARIA DAY 17", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "FALCIPARUM MALARIA", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "PLASMODIUM MALARIE", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "PLASMODIUM VIRAL MALARIA", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "DAY 15.MALARIA EPISODE TREATED", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "MALARIA EPISODE 23 DAY", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "HISTORY OF MALARIA", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "MALARIA \\(P. VIVAX\\)", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "MALARIA/CERT", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "MALARIA-P/F", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "P.F MALARIA", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "P/F-MALARIA", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "CLINICAL MALARIA", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "UNCOMPLICATED MALARIA", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "VIKAT MALARIA", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "SEVERE MALARIA ENDED IN DEATH", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "SEVERE MALARIA", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "ANY ANTIMALARIAL TREATMENT", "MALARIA")
  DATA$SATERM = str_replace_all(DATA$SATERM, "ANTIMALARIALs", "MALARIA")

  DATA$SATERM = str_replace_all(DATA$SATERM, "PAST HISTORY OF KALA-AZAR", "VL")
  DATA$SATERM = str_replace_all(DATA$SATERM, "PREVIOUS HISTORY OF KALA AZAR", "VL")
  DATA$SATERM = str_replace_all(DATA$SATERM, "HISTORY OF KALA AZAR", "VL")
  DATA$SATERM = str_replace_all(DATA$SATERM, "PREVIOUS HISTORY OF VL", "VL")
  DATA$SATERM = str_replace_all(DATA$SATERM, "HISTORY OF VL", "VL")
  DATA$SATERM = str_replace_all(DATA$SATERM, "PATIENT TREATED WITH AMPHOTERICIN-B FOR VL ONE YEAR BACK", "VL")

  DATA$SAPRESP = str_replace_all(DATA$SAPRESP, "TRUE", "Y")

  DATA$SAOCCUR = str_replace_all(DATA$SAOCCUR, "TRUE", "Y")
  DATA$SAOCCUR = str_replace_all(DATA$SAOCCUR, "FALSE", "N")
  DATA$SAOCCUR = str_replace_all(DATA$SAOCCUR, "UNKNOWN", "U")

  return(DATA)
}
