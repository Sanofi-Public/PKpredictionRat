
library(data.table)
#library(readr)
#library(readxl)
library(dplyr)


######   NCA #########


table3_NCA_AUC_CPTPSP <- readRDS("datasets/table3_NCA_AUC_CPTPSP.RDS")

table3_NCA_AUC_CPTPSP %>%
  summarize(median_value = median(AucRatio, na.rm = TRUE))


table3_NCA_CLAST_CPTPSP <- readRDS("datasets/table3_NCA_CLAST_CPTPSP.RDS")

table3_NCA_CLAST_CPTPSP %>%
  summarize(median_value = median(ClastRatio, na.rm = TRUE))


table3_NCA_C0_CPTPSP <- readRDS("datasets/table3_NCA_C0_CPTPSP.RDS")

table3_NCA_C0_CPTPSP %>%
  summarize(median_value = median(C0Ratio, na.rm = TRUE))


####### PBPK #############


distribution_models <- c(
  "_SM_Berezh_PKSim_v11_PLS_CL",
  "_SM_Berezh_Schmitt_v11_PLS_CL",
  "_SM_PKSim_PKSim_v11_PLS_CL",
  "_SM_PKSim_Schmitt_v11_PLS_CL",
  "_SM_PT_PKSim_v11_PLS_CL",
  "_SM_PT_Schmitt_v11_PLS_CL",
  "_SM_RR_PKSim_v11_PLS_CL",
  "_SM_RR_Schmitt_v11_PLS_CL",
  "_SM_Schmitt_PKSim_v11_PLS_CL",
  "_SM_Schmitt_Schmitt_v11_PLS_CL"
)


data_results <- data.table(
  Model = character(),
  AUC = numeric(),
  C0 = numeric(),
  Ctrough = numeric()
)


for(j in distribution_models){

  table3_PBPK_AUC <- readRDS(paste0("datasets/table3_PBPK_AUC_", j, "_CPTPSP.RDS"))

  myAUC <- median(table3_PBPK_AUC$AucRatio)


  table3_PBPK_C0 <- readRDS(paste0("datasets/table3_PBPK_C0_", j, "_CPTPSP.RDS"))

  myC0 <- median(table3_PBPK_C0$C0Ratio)


  table3_PBPK_CLAST <- readRDS(paste0("datasets/table3_PBPK_CLAST_", j, "_CPTPSP.RDS"))
  myCtorugh <- median(table3_PBPK_CLAST$ClastRatio)

  data_results <- rbind(data_results,
                        data.table(Model = j,
                                   AUC = myAUC,
                                   C0 = myC0,
                                   Ctrough = myCtorugh
                        )
  )

}

data_results <- rbind(data_results,
                      data.table(Model = "Min",
                                 AUC = min(data_results$AUC),
                                 C0 = min(data_results$C0),
                                 Ctrough = min(data_results$Ctrough)
                      ),
                      data.table(Model = "Max",
                                 AUC = max(data_results$AUC),
                                 C0 = max(data_results$C0),
                                 Ctrough = max(data_results$Ctrough)
                      )
)


####### 3CMT-ML #############



table3_3CMT_AUC_CPTPSP <- readRDS("datasets/table3_3CMT_AUC_CPTPSP.RDS")

table3_3CMT_AUC_CPTPSP %>%
  summarize(median_value = median(AucRatio, na.rm = TRUE))


table3_3CMT_CLAST_CPTPSP <- readRDS("datasets/table3_3CMT_CLAST_CPTPSP.RDS")

table3_3CMT_CLAST_CPTPSP %>%
  summarize(median_value = median(ClastRatio, na.rm = TRUE))


table3_3CMT_C0_CPTPSP <- readRDS("datasets/table3_3CMT_C0_CPTPSP.RDS")

table3_3CMT_C0_CPTPSP %>%
  summarize(median_value = median(C0Ratio, na.rm = TRUE))



####### 3CMT-PINN #############

table3_3CMTPINN_AUC_CPTPSP <- readRDS("datasets/table3_3CMTPINN_AUC_CPTPSP.RDS")

table3_3CMTPINN_AUC_CPTPSP %>%
  summarize(median_value = median(AUC_ratio, na.rm = TRUE))


table3_3CMTPINN_CLAST_CPTPSP <- readRDS("datasets/table3_3CMTPINN_CLAST_CPTPSP.RDS")

table3_3CMTPINN_CLAST_CPTPSP %>%
  summarize(median_value = median(CLAST_ratio, na.rm = TRUE))


table3_3CMTPINN_C0_CPTPSP <- readRDS("datasets/table3_3CMTPINN_C0_CPTPSP.RDS")

table3_3CMTPINN_C0_CPTPSP %>%
  summarize(median_value = median(C0_ratio, na.rm = TRUE))
