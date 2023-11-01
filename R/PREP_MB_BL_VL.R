#' Further prepare the MB domain for baseline analysis specifically for VL.
#'
#' Prepare the Microbiology (MB) domain for use in baseline analysis data sets
#' with specific actions for Visceral Leishmaniasis (VL). Takes a IDDO-SDTM
#' curated MB domain, transforms and pivots it in order to merge it into a
#' baseline analysis data set with other domains using the ANALYSE_BASELINE()
#' function. PREP_MB_BL() and PREP_MB_VL_BL() would be merged in the
#' ANALYSE_BASELINE() function.
#'
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#'
#' @return Data frame with one row per USUBJID/subject, with VL specific
#'   MBTESTCDs as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MB_VL_BL = function(DATA_MB){
  MB_VARS = c("LSHMANIA", "LDONOV", "LMAJOR")

  DATA = DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(MBTESTCD %in% MB_VARS) %>%
    DERIVE_TIMING() %>%
    CLEAN_MB_VL() %>%
    mutate(MBSTRES = as.character(MBSTRESN),
           MBUNITS = as.character(MBSTRESU),
           MBSTRESC = as.character(MBSTRESC),
           MBMODIFY = as.character(MBMODIFY),
           MBORRES = as.character(MBORRES))

  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBSTRESC"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBMODIFY"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBORRES"]

  DATA[which(is.na(DATA$MBUNITS)), "MBUNITS"] =
    DATA[which(is.na(DATA$MBUNITS)), "MBORRESU"]

  DATA$MBSTRES = str_replace_all(DATA$MBSTRES, "01-Oct", "1-10")
  DATA$MBSTRES = str_replace_all(DATA$MBSTRES, "01-OCT", "1-10")

  DATA = DATA %>%
    filter(TIMING == 1 | TIMING == "BASELINE") %>%
    pivot_wider(id_cols = c(STUDYID, USUBJID), names_from = MBTESTCD, names_glue = "{MBTESTCD}_{.value}",
                values_from = c(MBSTRES, MBUNITS, MBLOC, MBSPEC),
                names_sort = T, names_vary = "slowest",
                values_fn = first) %>%
    mutate(DATA_LSHMANIA = NA,
           DATA_MAJOR = NA,
           DATA_LDONOV = NA)

  colnames(DATA) = gsub("_MBSTRES", "", colnames(DATA))
  colnames(DATA) = gsub("_MBUNITS", "_UNITS", colnames(DATA))
  colnames(DATA) = gsub("MBLOC", "LOC", colnames(DATA))
  colnames(DATA) = gsub("MBSPEC", "SPEC", colnames(DATA))

  for(i in 1:nrow(DATA)){
    if("LSHMANIA" %in% names(DATA)){
      if(!is.na(DATA$LSHMANIA[i]) | !is.na(DATA$LSHMANIA_UNITS[i]) |
         !is.na(DATA$LSHMANIA_SPEC[i]) | !is.na(DATA$LSHMANIA_LOC[i])){
        DATA$DATA_LSHMANIA[i] = "LSHM"
      }
    }
    if("LDONOV" %in% names(DATA)){
      if(!is.na(DATA$LDONOV[i]) | !is.na(DATA$LDONOV_UNITS[i]) |
         !is.na(DATA$LDONOV_SPEC[i]) | !is.na(DATA$LDONOV_LOC[i])){
        DATA$DATA_LDONOV[i] = "LDON"
      }
    }
    if("LMAJOR" %in% names(DATA)){
      if(!is.na(DATA$LMAJOR[i]) | !is.na(DATA$LMAJOR_UNITS[i]) |
         !is.na(DATA$LMAJOR_SPEC[i]) | !is.na(DATA$LMAJOR_LOC[i])){
        DATA$DATA_MAJOR[i] = "LMAJ"
      }
    }
  }

  DATA = DATA %>%
    unite(DATA_LSHMANIA, DATA_LDONOV, DATA_MAJOR, col = "SPECIES", na.rm = TRUE, remove = TRUE, sep = " + ") %>%
    relocate(SPECIES, .after = USUBJID) %>%
    mutate(SPECIES = convert_blanks_to_na(SPECIES)) %>%
    clean_names(case = "all_caps")

  return(DATA)
}
