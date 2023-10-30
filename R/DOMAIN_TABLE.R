#' @title Tabulate function to display variables in a given domain
#'
#' @description Uses table() to display the variables contained within an SDTM formatted data frame.
#' Additionally can be split by STUDYID to display the options across multiple studies.
#'
#' @param DOMAIN_CODE The two letter code for the domain which matches the DOMAIN_FILE.
#'   Character string. Domains included: "DM", "LB", "RP", "MB", "MP", "SA", "IN", "VS", "DS", "RS".
#' @param DOMAIN_FILE The name of the SDTM domain dataset in the global environment.
#' @param by_STUDYID Split by STUDYID if TRUE. Default is FALSE.
#'
#' @return For Demographics (DM) domain, a character list with the column names.
#' For all other domains, a table class object listing the variables under --TESTCD
#' or INTRT, MPLOC, DSDECOD; where -- is the two letter domain name.
#'
#' @export
#'
#' @author Rhys Peploe
#'
#'
DOMAIN_TABLE = function(DOMAIN_CODE, DOMAIN_FILE, by_STUDYID = FALSE){
  DOMAIN_CODE = str_to_upper(DOMAIN_CODE)

  if(by_STUDYID == FALSE){
    if(DOMAIN_CODE == "DM"){
      return(colnames(DOMAIN_FILE))
    }
    else if(DOMAIN_CODE == "LB"){
      return(table(DOMAIN_FILE$LBTESTCD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "RP"){
      return(table(DOMAIN_FILE$RPTESTCD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "MB"){
      return(table(DOMAIN_FILE$MBTESTCD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "MP"){
      return(table(DOMAIN_FILE$MPLOC, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "SA"){
      return(table(str_to_upper(DOMAIN_FILE$SATERM), useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "IN"){
      return(table(str_to_upper(DOMAIN_FILE$INTRT), useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "VS"){
      return(table(DOMAIN_FILE$VSTESTCD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "DS"){
      return(table(DOMAIN_FILE$DSDECOD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "RS"){
      return(table(DOMAIN_FILE$RSTESTCD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "PO"){
      return(table(str_to_upper(DOMAIN_FILE$POTERM), useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "SC"){
      return(table(DOMAIN_FILE$SCTESTCD, useNA = 'ifany'))
    }
  }

  else{
    if(DOMAIN_CODE == "DM"){
      return(colnames(DOMAIN_FILE))
    }
    else if(DOMAIN_CODE == "LB"){
      return(table(DOMAIN_FILE$STUDYID, DOMAIN_FILE$LBTESTCD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "RP"){
      return(table(DOMAIN_FILE$STUDYID, DOMAIN_FILE$RPTESTCD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "MB"){
      return(table(DOMAIN_FILE$STUDYID, DOMAIN_FILE$MBTESTCD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "MP"){
      return(table(DOMAIN_FILE$STUDYID, DOMAIN_FILE$MPLOC, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "SA"){
      return(table(DOMAIN_FILE$STUDYID, str_to_upper(DOMAIN_FILE$SATERM), useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "IN"){
      return(table(DOMAIN_FILE$STUDYID, str_to_upper(DOMAIN_FILE$INTRT), useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "VS"){
      return(table(DOMAIN_FILE$STUDYID, DOMAIN_FILE$VSTESTCD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "DS"){
      return(table(DOMAIN_FILE$STUDYID, DOMAIN_FILE$DSDECOD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "RS"){
      return(table(DOMAIN_FILE$STUDYID, DOMAIN_FILE$RSTESTCD, useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "PO"){
      return(table(DOMAIN_FILE$STUDYID, str_to_upper(DOMAIN_FILE$POTERM), useNA = 'ifany'))
    }
    else if(DOMAIN_CODE == "SC"){
      return(table(DOMAIN_FILE$STUDYID, DOMAIN_FILE$SCTESTCD, useNA = 'ifany'))
    }
  }
}
