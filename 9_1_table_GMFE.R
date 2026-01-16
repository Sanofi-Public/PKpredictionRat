
data_GMFE_test_NCA <- readRDS("datasets/data_GMFE_test_NCA_CTPPSP.rds")


data_GMFE_test_3CMT <- readRDS("datasets/data_GMFE_test_3CMT_CTPPSP.rds")

data_GMFE_test_PINN_ThreeCMT <- readRDS("datasets/data_GMFE_test_PINN_ThreeCMT_CTPPSP.rds")

data_PURE_fold_gmfe <- readRDS("datasets/data_PURE_fold_gmfe_CTPPSP.rds")



median(data_GMFE_test_NCA$GMFE)
for(i in c(2,3,5,10,100)){
  count <- data_GMFE_test_NCA %>%
    filter(GMFE <= i) %>%
    nrow()
  #print(count)
  print(i)
  print(100*count/dim(data_GMFE_test_NCA)[1])
}



median(data_GMFE_test_3CMT$GMFE)
for(i in c(2,3,5,10,100)){
  count <- data_GMFE_test_3CMT %>%
    filter(GMFE <= i) %>%
    nrow()
  #print(count)
  print(i)
  print(100*count/dim(data_GMFE_test_3CMT)[1])
}


median(data_GMFE_test_PINN_ThreeCMT$GMFE)
for(i in c(2,3,5,10,100)){
  count <- data_GMFE_test_PINN_ThreeCMT %>%
    filter(GMFE <= i) %>%
    nrow()
  #print(count)
  print(i)
  print(100*count/dim(data_GMFE_test_PINN_ThreeCMT)[1])
}

median(data_PURE_fold_gmfe$gmfe)
for(i in c(2,3,5,10,100)){
  count <- data_PURE_fold_gmfe %>%
    filter(gmfe <= i) %>%
    nrow()
  #print(count)
  print(i)
  print(100*count/dim(data_PURE_fold_gmfe)[1])
}



library(readr)
library(dplyr)



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
  my_median = numeric(),
  GMFE_2 = numeric(),
  GMFE_3 = numeric(),
  GMFE_5 = numeric(),
  GMFE_10 = numeric(),
  GMFE_100 = numeric()
)




for(j in distribution_models){


  my_filename <- paste0("datasets/data_GMFE_test_PBPK_", j, "_CPTPSP", ".rds")


  data_GMFE_test <- readRDS( my_filename)





  GMFE_2 <- 100*(data_GMFE_test %>%
                   filter(GMFE <= 2) %>%
                   nrow())/dim(data_GMFE_test)[1]
  GMFE_3 <- 100*(data_GMFE_test %>%
                   filter(GMFE <= 3) %>%
                   nrow())/dim(data_GMFE_test)[1]
  GMFE_5 <- 100*(data_GMFE_test %>%
                   filter(GMFE <= 5) %>%
                   nrow())/dim(data_GMFE_test)[1]
  GMFE_10 <- 100*(data_GMFE_test %>%
                    filter(GMFE <= 10) %>%
                    nrow())/dim(data_GMFE_test)[1]
  GMFE_100 <- 100*(data_GMFE_test %>%
                     filter(GMFE <= 100) %>%
                     nrow())/dim(data_GMFE_test)[1]

  data_results <- rbind(data_results,
                        data.table(Model = j,
                                   my_median = median(data_GMFE_test$GMFE),
                                   GMFE_2 = GMFE_2,
                                   GMFE_3 = GMFE_3,
                                   GMFE_5 = GMFE_5,
                                   GMFE_10 = GMFE_10,
                                   GMFE_100 = GMFE_100
                        )
  )

}


data_results <- rbind(data_results,
                      data.table(Model = "Min",
                                 my_median = min(data_results$my_median),
                                 GMFE_2 = min(data_results$GMFE_2),
                                 GMFE_3 = min(data_results$GMFE_3),
                                 GMFE_5 = min(data_results$GMFE_5),
                                 GMFE_10 = min(data_results$GMFE_10),
                                 GMFE_100 = min(data_results$GMFE_100)
                      ),
                      data.table(Model = "Max",
                                 my_median = max(data_results$my_median),
                                 GMFE_2 = max(data_results$GMFE_2),
                                 GMFE_3 = max(data_results$GMFE_3),
                                 GMFE_5 = max(data_results$GMFE_5),
                                 GMFE_10 = max(data_results$GMFE_10),
                                 GMFE_100 = max(data_results$GMFE_100)
                      )
)











