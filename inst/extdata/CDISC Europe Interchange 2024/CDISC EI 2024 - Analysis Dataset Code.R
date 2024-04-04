### Load Packages
library(tidyverse)
library(iddoverse)

### Demographics Domain
DM_RPTESTD                                    # Curated Data
PREP_DM(DM_RPTESTD, DISEASE = "MALARIA")      # Pivoted Data using iddoverse

### Laboratory Results Domain
LB_RPTESTD
PREP_LB_BL(LB_RPTESTD)
PREP_LB_FU(LB_RPTESTD)

# Subset Dataset (Table 1 in Poster) 
LB_RPTESTD %>% 
  select(USUBJID, LBTESTCD, LBORRES, LBORRESU, LBSTRESC, LBSTRESN, LBSTRESU, 
         VISITNUM, VISITDY, LBDY, EPOCH)

### Vital Signs Domain
VS_RPTESTD
PREP_VS_BL(VS_RPTESTD)
PREP_VS_FU(VS_RPTESTD)

### Analysis Dataset
# Complete Dataset
ANALYSE_FOLLOW_UP(
  DISEASE_THEME = "MALARIA",
  DATA_DM = DM_RPTESTD,
  DATA_LB = LB_RPTESTD,
  DATA_VS = VS_RPTESTD
) 

# Subset Dataset (Table 2 in Poster)
ANALYSE_FOLLOW_UP(
  DISEASE_THEME = "MALARIA",
  DATA_DM = DM_RPTESTD,
  DATA_LB = LB_RPTESTD,
  DATA_VS = VS_RPTESTD
) %>% 
  select(USUBJID, AGE, SEX, ARMCD, VISITNUM, VISITDY, DAY, 
         HGB, HGB_UNITS, PLAT, PLAT_UNITS, 
         HEIGHT, HEIGHT_UNITS, WEIGHT, WEIGHT_UNITS)
