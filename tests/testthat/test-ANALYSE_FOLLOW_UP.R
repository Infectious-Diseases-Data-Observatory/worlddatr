# test_that("analyse follow up runs without error", {
#     expect_error(
#       ANALYSE_FOLLOW_UP(DISEASE_THEME = "VL",
#                       DATA_DM = read.csv("inst/extdata/Synthetic_Data_RPTESTB/DM_RPTESTB.csv"),
#                       DATA_LB = read.csv("inst/extdata/Synthetic_Data_RPTESTB/LB_RPTESTB.csv"),
#                       DATA_IN = read.csv("inst/extdata/Synthetic_Data_RPTESTB/IN_RPTESTB.csv"),
#                       DATA_MB = read.csv("inst/extdata/Synthetic_Data_RPTESTB/MB_RPTESTB.csv"),
#                       DATA_MP = read.csv("inst/extdata/Synthetic_Data_RPTESTB/MP_RPTESTB.csv"),
#                       DATA_RP = read.csv("inst/extdata/Synthetic_Data_RPTESTB/RP_RPTESTB.csv"),
#                       DATA_SA = read.csv("inst/extdata/Synthetic_Data_RPTESTB/SA_RPTESTB.csv"),
#                       DATA_TS = read.csv("inst/extdata/Synthetic_Data_RPTESTB/TS_RPTESTB.csv"),
#                       DATA_VS = read.csv("inst/extdata/Synthetic_Data_RPTESTB/VS_RPTESTB.csv"),
#
#                       DM_VARS = NULL,
#                       LB_VARS = NULL,
#                       IN_VARS = NULL,
#                       MB_VARS = NULL,
#                       MP_VARS = NULL,
#                       RP_VARS = NULL,
#                       SA_VARS = NULL,
#                       VS_VARS = NULL,
#
#                       MP_TESTCD = "LENGTH"))
# })
