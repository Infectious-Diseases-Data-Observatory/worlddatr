library(devtools)
load_all()

load("~/Git/IDDO/IDDO/VL Data GE.RData")

load("~/Git/IDDO/IDDO/Malaria Data GE.RData")
PO_MAL =  read_csv("C:/Users/rpeploe/Documents/R/Analysis Dataset Code/Datasets/Malaria_July_2023/PO.csv", guess_max = Inf)
SC_MAL =  read_csv("C:/Users/rpeploe/Documents/R/Analysis Dataset Code/Datasets/Malaria_July_2023/SC.csv", guess_max = Inf)

load("~/Git/IDDO/IDDO/Ebola Data GE.RData")
MB_EBOLA =  read_csv("C:/Users/rpeploe/Documents/R/Analysis Dataset Code/Datasets/Ebola/MB 2023-06-01.csv", guess_max = Inf)
ER_EBOLA = ER_EBOLA %>% mutate(ERDECOD = NA)

################################################################################
# Create Analysis Datasets
################################################################################

ANALYSE_BASELINE(DISEASE_THEME = "MALARIA",
                 DATA_DM = DM_MAL,
                 DATA_IN = IN_MAL,
                 DATA_LB = LB_MAL,
                 DATA_MB = MB_MAL,
                 DATA_MP = MP_MAL,
                 DATA_RP = RP_MAL,
                 DATA_SA = SA_MAL,
                 DATA_TS = TS_MAL,
                 DATA_VS = VS_MAL,

                 DM_VARS = "DTHFL", #User can specify extra variables
                 LB_VARS = NULL,
                 MB_VARS = NULL,
                 MB_VARS_SPEC = NULL,
                 MP_VARS = NULL,
                 IN_VARS = NULL,
                 RP_VARS = NULL,
                 SA_VARS = NULL,
                 VS_VARS = NULL,

                 IN_DUR = FALSE, #Choose to include Duration (DUR) or Time since event (TIME)
                 IN_TIME = FALSE,
                 SA_DUR = FALSE,
                 SA_TIME = FALSE,

                 MP_TESTCD = "LENGTH") %>% View

ANALYSE_BASELINE(DISEASE_THEME = "VL",
                 DATA_DM = DM_PULL,
                 DATA_IN = IN_PULL,
                 DATA_LB = LB_PULL,
                 DATA_MB = MB_PULL,
                 DATA_MP = MP_PULL,
                 DATA_RP = RP_PULL,
                 DATA_SA = SA_PULL,
                 DATA_TS = TS_PULL,
                 DATA_VS = VS_PULL,

                 DM_VARS = NULL, #User can specify extra variables
                 LB_VARS = NULL,
                 MB_VARS = NULL,
                 MB_VARS_SPEC = NULL,
                 MP_VARS = NULL,
                 IN_VARS = NULL,
                 RP_VARS = NULL,
                 SA_VARS = NULL,
                 VS_VARS = NULL,

                 IN_DUR = FALSE, #Choose to include Duration (DUR) or Time since event (TIME)
                 IN_TIME = FALSE,
                 SA_DUR = FALSE,
                 SA_TIME = FALSE,

                 MP_TESTCD = "LENGTH") %>% View

ANALYSE_FOLLOW_UP(DISEASE_THEME = "MALARIA",
                  DATA_DM = DM_MAL,
                  DATA_LB = LB_MAL,
                  DATA_IN = IN_MAL,
                  DATA_MB = MB_MAL,
                  DATA_MP = MP_MAL,
                  DATA_RP = RP_MAL,
                  DATA_SA = SA_MAL,
                  DATA_TS = TS_MAL,
                  DATA_VS = VS_MAL,
                  DATA_SC = SC_MAL,
                  DATA_PO = PO_MAL,

                  DM_VARS = NULL,
                  LB_VARS = NULL,
                  IN_VARS = NULL,
                  MB_VARS = NULL,
                  MP_VARS = NULL,
                  RP_VARS = NULL,
                  SA_VARS = NULL,
                  VS_VARS = NULL,

                  MP_TESTCD = "LENGTH") %>% View

ANALYSE_FOLLOW_UP(DISEASE_THEME = "VL",
                  DATA_DM = DM_PULL,
                  DATA_LB = LB_PULL,
                  DATA_IN = IN_PULL,
                  DATA_MB = MB_PULL,
                  DATA_MP = MP_PULL,
                  DATA_RP = RP_PULL,
                  DATA_SA = SA_PULL,
                  DATA_TS = TS_PULL,
                  DATA_VS = VS_PULL,

                  DM_VARS = NULL,
                  LB_VARS = NULL,
                  IN_VARS = NULL,
                  MB_VARS = NULL,
                  MP_VARS = NULL,
                  RP_VARS = NULL,
                  SA_VARS = NULL,
                  VS_VARS = NULL,

                  MP_TESTCD = "LENGTH") %>% View

ANALYSE_OUTCOME_VL(DATA_DM = DM_PULL,
                   DATA_DS = DS_PULL,
                   DATA_RS = RS_PULL,
                   DATA_MB = MB_PULL,

                   DM_VARS = NULL) %>% View

