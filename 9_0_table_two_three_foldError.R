
data_NCA_fold_error <- readRDS("datasets/data_NCA_fold_error.RDS")

data_3CMT_fold_error <- readRDS("datasets/data_3CMT_fold_error.RDS")

data_3CMTPINN_fold_error <- readRDS("datasets/data_3CMTPINN_fold_error.RDS")

data_PURE_fold_error <- readRDS("datasets/data_PURE_fold_error.RDS")


####### NCA ###############

count_twoFold <- data_NCA_fold_error  %>%
  filter(fold_change >= 0.5 & fold_change <= 2) %>%
  nrow()

100*count_twoFold/dim(data_NCA_fold_error)[1]


count_threeFold <- data_NCA_fold_error %>%
  filter(fold_change >= 1/3 & fold_change <= 3) %>%
  nrow()

100*count_threeFold/dim(data_NCA_fold_error)[1]

####### 3CMT ###############

count_twoFold <- data_3CMT_fold_error  %>%
  filter(fold_change >= 0.5 & fold_change <= 2) %>%
  nrow()

100*count_twoFold/dim(data_3CMT_fold_error)[1]


count_threeFold <- data_3CMT_fold_error %>%
  filter(fold_change >= 1/3 & fold_change <= 3) %>%
  nrow()

100*count_threeFold/dim(data_3CMT_fold_error)[1]

####### 3CMTPINN ###############

count_twoFold <- data_3CMTPINN_fold_error  %>%
  filter(fold_change >= 0.5 & fold_change <= 2) %>%
  nrow()

100*count_twoFold/dim(data_3CMTPINN_fold_error)[1]


count_threeFold <- data_3CMTPINN_fold_error %>%
  filter(fold_change >= 1/3 & fold_change <= 3) %>%
  nrow()

100*count_threeFold/dim(data_3CMTPINN_fold_error)[1]

####### PURE ###############

count_twoFold <- data_PURE_fold_error  %>%
  filter(fold_change >= 0.5 & fold_change <= 2) %>%
  nrow()

100*count_twoFold/dim(data_PURE_fold_error)[1]


count_threeFold <- data_PURE_fold_error %>%
  filter(fold_change >= 1/3 & fold_change <= 3) %>%
  nrow()

100*count_threeFold/dim(data_PURE_fold_error)[1]



library(readr)
library(dplyr)
library(ggplot2)


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
  Two_fold = numeric(),
  Three_fold = numeric()
)


for(j in distribution_models){


  my_filename <-  paste0("Rat_data_PBPK_all", j, "_vs_meanConc_CPTPSP", ".csv")

  Rat_data_PBPK_all_vs_meanConc <- read_delim(my_filename,
                                              delim = ",", escape_double = FALSE, trim_ws = TRUE)

  Rat_data_PBPK_all_vs_meanConc <- Rat_data_PBPK_all_vs_meanConc %>% filter(TYPE == "SAMPLE")


  Rat_data_PBPK_all_vs_meanConc <- Rat_data_PBPK_all_vs_meanConc %>% mutate(fold_change = CONC_NM_PBPK_pred/DV_NM)
  Rat_data_PBPK_all_vs_meanConc <- Rat_data_PBPK_all_vs_meanConc %>% mutate(fold_change_log = log(CONC_NM_PBPK_pred)/log(DV_NM))

  count_twoFold <- Rat_data_PBPK_all_vs_meanConc %>%
    filter(fold_change >= 0.5 & fold_change <= 2) %>%
    nrow()


  # three fold
  count_threeFold <- Rat_data_PBPK_all_vs_meanConc %>%
    filter(fold_change >= 1/3 & fold_change <= 3) %>%
    nrow()


  print(paste0("The two-fold change of model ", j," is:", 100*count_twoFold/dim(Rat_data_PBPK_all_vs_meanConc)[1] ))
  print(paste0("The three-fold change of model ", j," is:", 100*count_threeFold/dim(Rat_data_PBPK_all_vs_meanConc)[1] ))


  data_results <- rbind(data_results,
                        data.table(Model = j,
                                   Two_fold = 100*count_twoFold/dim(Rat_data_PBPK_all_vs_meanConc)[1],
                                   Three_fold = 100*count_threeFold/dim(Rat_data_PBPK_all_vs_meanConc)[1]
                        )
  )


}

data_results <- rbind(data_results,
                      data.table(Model = "Min",
                                 Two_fold = min(data_results$Two_fold),
                                 Three_fold = min(data_results$Three_fold)
                      ),
                      data.table(Model = "Max",
                                 Two_fold = max(data_results$Two_fold),
                                 Three_fold = max(data_results$Three_fold)
                      )
)


data_results




