#' Derive BMI and replace contributor provided result if HEIGHT and WEIGHT are
#' given.
#'
#' Calculates the Body Mass Index of subjects if WEIGHT and HEIGHT are in the
#' data frame. Initially creates a new variable which calculates the BMI of
#' those with valid WEIGHT and HEIGHT variables, then where NAs exist, the
#' variable is populated with existing BMI results. This new variable then
#' replaces the existing BMI column.
#'
#' @param DATA Data frame containing HEIGHT and WEIGHT variables, typically the
#'   Vital Signs (VS) domain.
#'
#' @return Data frame with additional column for BMI, if BMI did not previously
#'   exist. If BMI existed previously, the BMI is recalculated using the WEIGHT
#'   and HEIGHT provided. The existing value will only be used if there is an NA
#'   value in the recalculated column, the existing column is then discarded.
#'
#' @export
#'
#' @author Rhys Peploe
#'
#' @importFrom admiral compute_bmi
#'
#' @examples
#' df <- data.frame(WEIGHT = c(30), HEIGHT = c(150), AGE = c(34))
#' DERIVE_BMI(df)
#'
#'
DERIVE_BMI = function(DATA){
  if(("HEIGHT" %in% names(DATA)) & ("WEIGHT" %in% names(DATA))){
    DATA = DATA %>%
      mutate(HEIGHT = as.numeric(DATA$HEIGHT),
             WEIGHT = as.numeric(DATA$WEIGHT))

    DATA_BMI = DATA %>%
      filter(.data$AGE >= 18,
             is.na(.data$HEIGHT) == FALSE & is.na(.data$WEIGHT) == FALSE) %>%
      mutate(BMI_c = round(compute_bmi(height = .data$HEIGHT,
                                       weight = .data$WEIGHT), 2))

    DATA = left_join(DATA, DATA_BMI)

    if("BMI" %in% names(DATA)){
      DATA$BMI = ifelse(DATA$AGE < 18, NA, DATA$BMI)

      DATA[which(is.na(DATA$BMI_c)), "BMI_c"] =
        DATA[which(is.na(DATA$BMI_c)), "BMI"]

      DATA = DATA %>%
        dplyr::select(-"BMI")
    }

    DATA = DATA %>%
      rename("BMI" = "BMI_c") %>%
      relocate("BMI", .after = "WEIGHT")
  }

  return(DATA)
}
