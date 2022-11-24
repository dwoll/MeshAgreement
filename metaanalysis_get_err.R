#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## load packages
## read data
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
library(parallel)

source("metaanalysis_global_cl.R")

n_repl  <- 5000
n_cores <- min(10, parallel::detectCores()-2)

d_RR0_allL  <- readRDS("d_RR0_allL.rda")
d_RR_allL   <- readRDS("d_RR_allL.rda")

#####---------------------------------------------------------------------------
## get ERR from category CIs
#####---------------------------------------------------------------------------

cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(boot))
clusterEvalQ(cl, library(nlme))
clusterEvalQ(cl, library(mvtnorm))

clusterExport(cl,
              c("get_RR_SE_from_CI",
                "get_RR_logSE_from_CI",
				"get_ERR_SE_from_CI",
                "get_ERR_logSE_from_CI",
				"rGen",
				"rGen_correlated",
				"get_ERR",
				"get_ERR_correlated",
				"get_ERR_CI",
				"get_ERR_from_ORRRSIRIRR",
				"rr_impute_missing",
				"impute_highest_cat",
				"lognorm_log_to_org",
				"lognorm_org_to_log"))

# d_ERR_allL <- lapply(d_RR_allL, get_all_ERR_from_RR, n_repl=n_repl)
d_ERR_allL <- parLapply(cl, d_RR_allL, get_all_ERR_from_RR, n_repl=n_repl)
stopCluster(cl)
saveRDS(d_ERR_allL, file="d_ERR_allL.rda")

#####---------------------------------------------------------------------------
## sensitivity: different dose thresholds for thyroid
#####---------------------------------------------------------------------------

dose_thresh_thyroid <- c(5, 20)

## 5 Gy
d_RR_unique_thresh05 <- rr_impute_missing(d_RR0_allL$Thyroid) %>%
    filter(is.na(d_kPrime) | (d_kPrime < dose_thresh_thyroid[1]),
           !(Reference %in% sens_overlappingL$Thyroid))

## 20 Gy
d_RR_unique_thresh20 <- rr_impute_missing(d_RR0_allL$Thyroid) %>%
    filter(is.na(d_kPrime) | (d_kPrime < dose_thresh_thyroid[2]),
           !(Reference %in% sens_overlappingL$Thyroid))

d_ERR_thresh05L <- get_all_ERR_from_RR(d_RR_unique_thresh05, n_repl=n_repl)
d_ERR_thresh20L <- get_all_ERR_from_RR(d_RR_unique_thresh20, n_repl=n_repl)

saveRDS(d_ERR_thresh20L$d_ERR_wide,
        file=paste0("d_ERR_thyroid_thresh",
                    sprintf("%.2d", dose_thresh_thyroid[1]), "_unique_wide.rda"))

saveRDS(d_ERR_thresh05L$d_ERR_wide,
        file=paste0("d_ERR_thyroid_thresh",
                    sprintf("%.2d", dose_thresh_thyroid[2]), "_unique_wide.rda"))
