PREP_DS_OUT_EVD = function(DATA_DS, expand_cols = FALSE){
  DATA_DS = DATA_DS %>%
    convert_blanks_to_na() %>%
    mutate(DSSTRES = str_to_upper(as.character(.data$DSDECOD)),
           DSMODIFY = str_to_upper(as.character(.data$DSMODIFY)),
           DSTERM = str_to_upper(as.character(.data$DSTERM)),
           DAY = .data$DSDY,
           START_DAY = .data$DSSTDY)

  DATA_DS[which(is.na(DATA_DS$DSSTRES)), "DSSTRES"] =
    DATA_DS[which(is.na(DATA_DS$DSSTRES)), "DSMODIFY"]
  DATA_DS[which(is.na(DATA_DS$DSSTRES)), "DSSTRES"] =
    DATA_DS[which(is.na(DATA_DS$DSSTRES)), "DSTERM"]

  DATA = DATA_DS %>%
    pivot_wider(id_cols = c(.data$STUDYID, .data$USUBJID),
                names_from = .data$DOMAIN,
                values_from = c(.data$DSSTRES, .data$VISITNUM, .data$VISITDY, .data$DAY, .data$START_DAY),
                values_fn = last) %>%
    rename("FINAL_DISP" = "DSSTRES_DS",
           "FINAL_DISP_VISITDY" = "VISITDY_DS",
           "FINAL_DISP_VISITNUM" = "VISITNUM_DS",
           "FINAL_DISP_DAY" = "DAY_DS",
           "FINAL_DISP_START_DAY" = "START_DAY_DS")

  return(DATA)
}
