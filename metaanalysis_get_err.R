#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## load packages
## read data
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
library(parallel)

source("metaanalysis_global.R")

n_repl  <- 2000
n_cores <- min(10, parallel::detectCores()-2)

d_RR0_selL <- readRDS("d_RR0_selL.rda")
d_RR_allL  <- readRDS("d_RR_allL.rda")
d_RR_selL  <- readRDS("d_RR_selL.rda")

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
                "get_ERR_SE_from_CI",
                "get_ERR_logSE_from_CI",
				"get_RR_logSE_from_CI",
				"rGen",
				"rGen_correlated",
				"get_ERR",
				"get_ERR_correlated",
				"get_ERR_CI",
				"get_ERR_from_ORRRSIRIRR",
				"lognorm_org_to_log",
				"lognorm_log_to_org",
				"rr_impute_missing",
				"impute_highest_cat"))

# d_ERR_allL <- lapply(d_RR_allL[1], get_all_ERR_from_RR, n_repl=n_repl)
d_ERR_allL <- parLapply(cl, d_RR_allL, get_all_ERR_from_RR, n_repl=n_repl)
d_ERR_selL <- parLapply(cl, d_RR_selL, get_all_ERR_from_RR, n_repl=n_repl)

stopCluster(cl)

saveRDS(d_ERR_allL, file="d_ERR_allL.rda")
saveRDS(d_ERR_selL, file="d_ERR_selL.rda")

#####---------------------------------------------------------------------------
## sensitivity: different dose thresholds for thyroid
#####---------------------------------------------------------------------------

## 5 Gy
dose_thresh_thyroid <- 5
d_RR_sel <- rr_impute_missing(d_RR0_selL$Thyroid) %>%
    filter(is.na(d_kPrime) | (d_kPrime < dose_thresh_thyroid))

d_ERR_selL <- get_all_ERR_from_RR(d_RR_sel, n_repl=n_repl)
saveRDS(d_ERR_selL$d_ERR_wide,
        file=paste0("d_ERR_thyroid_thresh",
                    sprintf("%.2d", dose_thresh_thyroid), "_sel_wide.rda"))

## 20 Gy
dose_thresh_thyroid <- 20
d_RR_sel <- rr_impute_missing(d_RR0_selL$Thyroid) %>%
    filter(is.na(d_kPrime) | (d_kPrime < dose_thresh_thyroid))

d_ERR_selL <- get_all_ERR_from_RR(d_RR_sel, n_repl=n_repl)
saveRDS(d_ERR_selL$d_ERR_wide,
        file=paste0("d_ERR_thyroid_thresh",
                    sprintf("%.2d", dose_thresh_thyroid), "_sel_wide.rda"))
