#' Further prepare the MB domain for baseline analysis specifically for Malaria.
#'
#' Prepare the Microbiology (MB) domain for use in baseline analysis data sets
#' with specific actions for Malaria. Takes a IDDO-SDTM curated
#' MB domain, transforms and pivots it in order to merge it into a baseline
#' analysis data set with other domains using the ANALYSE_BASELINE() function.
#' PREP_MB_BL() and PREP_MB_MAL_BL() would be merged in the ANALYSE_BASELINE()
#' function.
#'
#' @param DATA_MB The MB domain data frame, as named in the global environment.
#'
#' @return Data frame with one row per USUBJID/subject, with Malaria
#'   specific MBTESTCDs as columns
#'
#' @export
#'
#' @author Rhys Peploe
#'
PREP_MB_BL_MAL = function(DATA_MB){
  MB_VARS = c("PFALCIPA", "PFALCIPS", "PFALCIP",
              "PVIVAXA", "PVIVAXS", "PVIVAX",
              "PLSMDMA", "PLSMDMS", "PLSMDM",
              "PKNOWLA", "PKNOWLS", "PKNOWL",
              "PMALARA", "PMALARS", "PMALAR",
              "POVALEA", "POVALES", "POVALE")

  DATA = DATA_MB %>%
    convert_blanks_to_na() %>%
    filter(.data$MBTESTCD %in% MB_VARS) %>%
    DERIVE_TIMING() %>%
    mutate(MBSTRES = as.character(.data$MBSTRESN),
           MBSTRESC = as.character(.data$MBSTRESC),
           MBMODIFY = as.character(.data$MBMODIFY),
           MBORRES = as.character(.data$MBORRES),
           MBUNITS = as.character(NA))

  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBSTRESC"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBMODIFY"]
  DATA[which(is.na(DATA$MBSTRES)), "MBSTRES"] =
    DATA[which(is.na(DATA$MBSTRES)), "MBORRES"]

  DATA[which(!is.na(DATA$MBSTRESC) | !is.na(DATA$MBSTRESN)), "MBUNITS"] =
    DATA[which(!is.na(DATA$MBSTRESC) | !is.na(DATA$MBSTRESN)), "MBSTRESU"]
  DATA[which(is.na(DATA$MBSTRESC) & is.na(DATA$MBSTRESN)), "MBUNITS"] =
    DATA[which(is.na(DATA$MBSTRESC) & is.na(DATA$MBSTRESN)), "MBORRESU"]

  DATA = DATA %>%
    mutate(MBSTRES = str_to_upper(.data$MBSTRES),
           MBUNITS = str_to_upper(.data$MBUNITS)) %>%
    filter(.data$TIMING == 1 | .data$TIMING == "BASELINE") %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$MBTESTCD,
                names_glue = "{MBTESTCD}_{.value}", values_from = c(.data$MBSTRES, .data$MBUNITS),
                names_sort = T, names_vary = "slowest",
                values_fn = first) #%>%
    # mutate(DATA_PF = NA,
    #        DATA_PV = NA,
    #        DATA_PL = NA,
    #        DATA_PK = NA,
    #        DATA_PM = NA,
    #        DATA_PO = NA)

  # %>%
  #   rename("PARA_PF" = "PFALCIPA",
  #          "GAM_PF",
  #          )

  colnames(DATA) = gsub("_MBSTRES", "", colnames(DATA))

  colnames(DATA) = gsub("PFALCIPA", "PARA_PF", colnames(DATA))
  colnames(DATA) = gsub("PFALCIPS", "GAM_PF", colnames(DATA))

  colnames(DATA) = gsub("PLSMDMA", "PARA_PL", colnames(DATA))
  colnames(DATA) = gsub("PLSMDMS", "GAM_PL", colnames(DATA))
  colnames(DATA) = gsub("PLSMDM", "UNSP_PL", colnames(DATA))

  colnames(DATA) = gsub("PVIVAXA", "PARA_PV", colnames(DATA))
  colnames(DATA) = gsub("PVIVAXS", "GAM_PV", colnames(DATA))

  colnames(DATA) = gsub("PKNOWLA", "PARA_PK", colnames(DATA))
  colnames(DATA) = gsub("PKNOWLA", "GAM_PK", colnames(DATA))

  colnames(DATA) = gsub("PMALARA", "PARA_PM", colnames(DATA))
  colnames(DATA) = gsub("PMALARS", "GAM_PM", colnames(DATA))

  colnames(DATA) = gsub("POVALEA", "PARA_PO", colnames(DATA))
  colnames(DATA) = gsub("POVALES", "GAM_PO", colnames(DATA))


  # for(i in 1:nrow(DATA)){
  #   if("PARA_PF" %in% names(DATA)){
  #     if(!is.na(DATA$PARA_PF[i]) | !is.na(DATA$GAM_PF[i])){
  #       DATA$DATA_PF[i] = "PF"
  #     }
  #   }
  #   if("PARA_PV" %in% names(DATA)){
  #     if(!is.na(DATA$PARA_PV[i]) | !is.na(DATA$GAM_PV[i])){
  #       DATA$DATA_PV[i] = "PV"
  #     }
  #   }
  #   if("PARA_PL" %in% names(DATA)){
  #     if(!is.na(DATA$PARA_PL[i]) | !is.na(DATA$GAM_PL[i])){
  #       DATA$DATA_PL[i] = "PL"
  #     }
  #   }
  #   if("PARA_PK" %in% names(DATA)){
  #     if(!is.na(DATA$PARA_PK[i]) | !is.na(DATA$GAM_PK[i])){
  #       DATA$DATA_PK[i] = "PK"
  #     }
  #   }
  #   if("PARA_PM" %in% names(DATA)){
  #     if(!is.na(DATA$PARA_PM[i]) | !is.na(DATA$GAM_PM[i])){
  #       DATA$DATA_PM[i] = "PM"
  #     }
  #   }
  #   if("PARA_PO" %in% names(DATA)){
  #     if(!is.na(DATA$PARA_PO[i]) | !is.na(DATA$GAM_PO[i])){
  #       DATA$DATA_PO[i] = "PO"
  #     }
  #   }
  # }

  # DATA = DATA %>%
  #   unite(.data$DATA_PF, .data$DATA_PV, .data$DATA_PK, .data$DATA_PM, .data$DATA_PO,
  #         .data$DATA_PL, col = "SPECIES", na.rm = TRUE, remove = TRUE, sep = " + ")

  DATA = DATA %>%
    # relocate(.data$SPECIES, .after = .data$USUBJID) %>%
    # mutate(SPECIES = convert_blanks_to_na(.data$SPECIES)) %>%
    clean_names(case = "all_caps")

  return(DATA)
}
