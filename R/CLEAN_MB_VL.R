#' Clean MB variables before preparing the domain.
#'
#' Replaces certain character strings to standardise the MBORRESU and MBSTRESC columns in the
#' Microbiology (MB) Domain.
#'
#' @param DATA_MB The SA domain data frame, as named in the global environment.
#'
#' @return Data frame which has been standardised in MBSTRESC and MBORRESU
#'
#' @export
#'
#' @author Rhys Peploe
#'
CLEAN_MB_VL = function(DATA_MB){

  DATA_MB[which(DATA_MB$MBSTRESC == "0 parasites per 1000 fields"), "MBORRESU"] = "/1000 HPFs"
  DATA_MB[which(DATA_MB$MBSTRESC == "0 parasites per 1000 fields"), "MBSTRESC"] = "0"

  return(DATA_MB)
}
