#' Derive BMI and replace contributor provided result if HEIGHT and WEIGHT are given.
#'
#' Calculates the Body Mass Index of subjects if WEIGHT and HEIGHT are in the data frame. Initially creates a new variable which calculates the BMI of those with valid WEIGHT and HEIGHT variables, then where NAs exist, the variable is populated with existing BMI results. This new variable then replaces the existing BMI column.
#'
#' @param DATA Data frame containing HEIGHT and WEIGHT variables, typically the Vital Signs (VS) domain.
#'
#' @return
#'
#' @export
#'
#' @author Rhys Peploe
#'
DERIVE_BMI = function(DATA){
  if(("HEIGHT" %in% names(DATA)) & ("WEIGHT" %in% names(DATA))){
    DATA = DATA %>%
      mutate(HEIGHT = as.numeric(HEIGHT),
             WEIGHT = as.numeric(WEIGHT))

    DATA_BMI = DATA %>%
      filter(AGE >= 18) %>%
      filter(is.na(HEIGHT) == FALSE & is.na(WEIGHT) == FALSE) %>%
      mutate(BMI_c = round(compute_bmi(height = HEIGHT, weight = WEIGHT), 2))

    DATA = left_join(DATA, DATA_BMI) %>%
      mutate(BMI_c = as.character(BMI_c))

    if("BMI" %in% names(DATA)){
      DATA[which(is.na(DATA$BMI_c)), "BMI_c"] =
        DATA[which(is.na(DATA$BMI_c)), "BMI"]

      DATA = DATA %>%
        dplyr::select(-BMI)
    }

    DATA = DATA %>%
      rename("BMI" = "BMI_c")
  }

  return(DATA)
}
