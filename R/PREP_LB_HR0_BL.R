PREP_LB_HR0_BL = function(DATA_LB, DISEASE = "", VARS = NULL){
  DISEASE = str_to_upper(DISEASE)

  if(DISEASE == "MALARIA"){
    LB_VARS = c("INTLK6", "CD4", "HGB", "HGBMET", "HCT", "PLAT", "WBC", "K", "ALT", "AST",
                "BILI", "CREAT", "ALB", "SODIUM", "HCG", "G6PD", str_to_upper(VARS))
  }

  else if(DISEASE == "VL"){
    LB_VARS = c("INTLK6", "CD4", "HGB", "HCT", "PLAT", "WBC", "K", "ALT", "AST",
                "BILI", "CREAT", "ALB", "SODIUM", "HCG", str_to_upper(VARS))
  }

  else if(DISEASE == "EBOLA"){
    LB_VARS = c("K", "ALT", "AST", "CREAT", "SODIUM", "UREA", str_to_upper(VARS))
  }

  else{
    LB_VARS = c("HGB", "HCT", "PLAT", "WBC", "K", "ALT", "AST", "BILI", "CREAT",
                "ALB", "SODIUM", "HCG", str_to_upper(VARS))
  }

  DATA = DATA_LB %>%
    convert_blanks_to_na() %>%
    filter(.data$LBTESTCD %in% LB_VARS) %>%
    mutate(LBSTRES = str_to_upper(as.character(.data$LBSTRESN)),
           LBSTRESC = str_to_upper(as.character(.data$LBSTRESC)),
           LBORRES = str_to_upper(as.character(.data$LBORRES)),
           TIMING = str_to_upper(as.character(.data$VISIT)),
           # EPOCH = str_to_upper(as.character(.data$EPOCH)),
           LBUNITS = as.character(NA))

  # DATA[which(is.na(DATA$TIMING)), "TIMING"] =
  #   DATA[which(is.na(DATA$TIMING)), "EPOCH"]

  DATA[which(is.na(DATA$LBSTRES)), "LBSTRES"] =
    DATA[which(is.na(DATA$LBSTRES)), "LBSTRESC"]
  DATA[which(is.na(DATA$LBSTRES)), "LBSTRES"] =
    DATA[which(is.na(DATA$LBSTRES)), "LBORRES"]

  DATA[which(!is.na(DATA$LBSTRESC) | !is.na(DATA$LBSTRESN)), "LBUNITS"] =
    DATA[which(!is.na(DATA$LBSTRESC) | !is.na(DATA$LBSTRESN)), "LBSTRESU"]
  DATA[which(is.na(DATA$LBSTRESC) & is.na(DATA$LBSTRESN)), "LBUNITS"] =
    DATA[which(is.na(DATA$LBSTRESC) & is.na(DATA$LBSTRESN)), "LBORRESU"]

  DATA = DATA %>%
    mutate(LBSTRES = str_to_upper(.data$LBSTRES)) %>%
    filter(.data$TIMING == "HOUR 0") %>%   # .data$TIMING == "BASELINE"
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID), names_from = .data$LBTESTCD,
                values_from = c(.data$LBSTRES, .data$LBUNITS),
                names_sort = T, names_vary = "slowest",
                values_fn = first, names_glue = "{LBTESTCD}_{.value}")

  colnames(DATA) = gsub("_LBSTRES", "", colnames(DATA))
  colnames(DATA) = gsub("LBUNITS", "UNITS", colnames(DATA))

  DATA = DATA %>%
    clean_names(case = "all_caps")

  return(DATA)
}

