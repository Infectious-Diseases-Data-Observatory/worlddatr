#' Create EMPTY_TIME variable which is populated only for rows without core
#' timing variables.
#'
#' EMPTY_TIME is a dummy variable which is used to pivot the data one when rows
#' do not have any of VISITNUM, VISITDY, DAY (and --STDY & --ENDY in appropriate
#' domains). Every row before the pivot will have either some timing variables
#' populated or EMPTY_TIME. When pivoting, EMPTY_TIME is included so that each
#' row is pivoted and no informative row is dropped during the transformation.
#'
#' This is used in creating follow up analysis datasets.
#'
#' @param DATA Data frame which contains a SDTM domain.
#'
#' @return Data frame with additional column for EMPTY_TIME
#'
#' @export
#'
#' @author Rhys Peploe
#'
DERIVE_EMPTY_TIME = function(DATA){
  DATA = DATA %>%
    group_by(USUBJID) %>%
    mutate(EMPTY_SEQ = row_number(),
           EMPTY_TIME = paste(DOMAIN, "_", EMPTY_SEQ))

  DATA$EMPTY_TIME = gsub(" ", "", DATA$EMPTY_TIME)

  return(DATA)
}
