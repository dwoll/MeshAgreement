#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Meta-analysis methods based on
## Little 2001. https://doi.org/10.1080/09553000010022634
## combined with
## Doi et al. 2014. https://doi.org/10.1093/jrr/rru045
## and
## Berrington and Cox. 2003. https://doi.org/10.1093/biostatistics/4.3.423
## Richardson et al. 2020. https://doi.org/10.1007/s00411-020-00863-w
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## TODO
## correlation between category RRs currently set to 0.5
## (based on simple simulation) for gls() estimates -> keep?
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(boot)
library(metafor)
library(nlme)
library(mvtnorm)

## ggplot2 theme
ggpt <- theme_bw() +
    theme(#text=element_text(size=rel(1.7)),
        axis.title.x=element_text(size=rel(1.1)),
        axis.title.y=element_text(size=rel(1.1)),
        axis.text.x=element_text(size=rel(1.1)),
        axis.text.y=element_text(size=rel(1.1)),
        strip.text.x=element_text(size=rel(1.1)),
        strip.text.y=element_text(size=rel(1.1)),
        legend.text=element_text(size=rel(1)),
        legend.title=element_text(size=rel(1.1)),
        plot.title=element_text(size=rel(1.1))# ,
        # plot.margin=margin(unit(.25, "cm"),
        #                    unit(.25, "cm"),
        #                    unit(.25, "cm"),
        #                    unit(1, "cm"))
    )

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## category-specific RRs -> ERR/Gy
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## impute reference dose for highest dose category
## if upper boundary of highest dose category is not available
## set highest category width = 2 times previous category width
#####---------------------------------------------------------------------------

## d_k1: representative doses for categories
## lo: lower boundaries, up: upper boundaries
impute_highest_cat <- function(d_k1, lo, up) {
    n_cat <- length(d_k1)
    d_k2  <- d_k1
    
    ## highest category representative dose missing
    ## AND upper bondary missing or Inf?
    if(is.na(d_k1[n_cat]) && !is.finite(up[n_cat])) {
        ## width of previous category
        cat_width_prev <- if(n_cat == 1L) {  # 1 dose category
            lo
        } else {                             # >= 2 dose categories
            up[n_cat-1] - lo[n_cat-1]
        }
        
        ## representative dose for highest category
        ## when category width = 2*width of next lower category
        d_k2[n_cat] <- lo[n_cat] + cat_width_prev
    } else if(is.na(d_k1[n_cat]) && !is.infinite(up[n_cat])) {
        ## highest category representative dose missing
        ## AND upper bondary available
        ## width of dose category
        cat_width <- up[n_cat] - lo[n_cat]
        
        ## representative dose for highest category
        d_k2[n_cat] <- lo[n_cat] + 0.5*cat_width
    }
    
    d_k2
}

## recover standard error of point estimate from CI boundaries
## TODO: implement Richardson et al. 2020
## RR: assuming normal Wald CI on linear scale
get_RR_SE_from_CI <- function(CIlo, CIup, CI_width) {
    (CIup - CIlo)            / (2*qnorm((1-CI_width)/2, lower.tail=FALSE))
}

## ERR: assuming normal Wald CI on linear scale
get_ERR_SE_from_CI <- function(CIlo, CIup, CI_width) {
    ((CIup+1) - (CIlo+1))    / (2*qnorm((1-CI_width)/2, lower.tail=FALSE))
}

## RR: assuming normal Wald CI on log scale (log-normal)
get_RR_logSE_from_CI <- function(CIlo, CIup, CI_width) {
    log(CIup / CIlo)         / (2*qnorm((1-CI_width)/2, lower.tail=FALSE))
}

## ERR: assuming normal Wald CI on log scale (log-normal)
get_ERR_logSE_from_CI <- function(CIlo, CIup, CI_width) {
    log((CIup+1) / (CIlo+1)) / (2*qnorm((1-CI_width)/2, lower.tail=FALSE))
}

## collapse whitespace into one space
collapse_ws <- function(x)  {
    gsub("[[:blank:]]+", " ", x)
}

## prepare RR data
## fill in missing information such as CI boundary, category reference dose
rr_impute_missing <- function(x) {
    x %>%
        mutate(## generate shorter publication ID
               Reference=trimws(Reference, which="both"),
               Reference=collapse_ws(Reference),
               pub_author=gsub("^\\{([[:alpha:] -_]+), [[:digit:]]{4}, [[:digit:]]+\\}$", "\\1", Reference),
               pub_year  =gsub("^\\{[[:alpha:] -_]+, ([[:digit:]]{4}), [[:digit:]]+\\}$", "\\1", Reference),
               auth_year =paste(abbreviate(pub_author, minlength=7, strict=TRUE),
                                pub_year, sep="_"),
               ## set representative dose for dose category if none is given
               dose_catUp=as.numeric(dose_catUp),  # may be character due to "Inf"
               d_k1=if_else(is.na(d_k) & !is.infinite(dose_catUp),
                            dose_catLo + 0.5*(dose_catUp-dose_catLo),
                            d_k),
               ## if no ERR CI is given, but ERR_SD is available
               ## -> assume normality, CI on original scale
               ERR_CI_imputed=is.na(ERR_CIlo) & is.na(ERR_CIup) & !is.na(ERR_SD) & !is.na(ERR),
               ERR_CIlo=if_else(ERR_CI_imputed,
                                ERR - qnorm((1-CI_width_ERR)/2, lower.tail=FALSE)*ERR_SD,
                                as.numeric(ERR_CIlo)),
               ERR_CIup=if_else(ERR_CI_imputed,
                                ERR + qnorm((1-CI_width_ERR)/2, lower.tail=FALSE)*ERR_SD,
                                as.numeric(ERR_CIup)),
               ## if no ERR CI lower bound is given, but upper bound is available
               ## -> assume log-normality of RR -> symmetry of CI on log scale
               ERR_CIlo_imputed=is.na(ERR_CIlo) & !is.na(ERR_CIup) & !is.na(ERR),
               ERR_CIlo=if_else(ERR_CIlo_imputed,
                                ## min lower CI bound = 0?
                                # pmin(exp(log(ERR+1) - (log(ERR_CIup+1)-log(ERR+1))) - 1, 0),
                                exp(log(ERR+1) - (log(ERR_CIup+1)-log(ERR+1))) - 1,
                                as.numeric(ERR_CIlo)),
               ## for regression to estimate ERR/Gy from category RRs
               ## assuming log-normal distribution of RR
               RR_cat_logSE =get_RR_logSE_from_CI(RR_cat_CIlo, RR_cat_CIup, CI_width_RR),
               RR_cat_logSE2=RR_cat_logSE^2,
               ## variance on original scale
               RR_cat_SE2=lognorm_log_to_org(log(RR_cat), RR_cat_logSE)$sigma^2,
               ## weight for Little 2001 linear regressions
               w=1/RR_cat_logSE2,
               ## offset for Little 2001 linear regression
               off=1) %>%
        ## impute dose for highest category if missing
        group_by(auth_year) %>%
        mutate(d_k2=impute_highest_cat(d_k1, dose_catLo, dose_catUp)) %>%
        ungroup() %>%
        ## final representative dose per category
        ## -> subtract dose from reference category
        mutate(d_kPrime=d_k2-d_0) %>%
        select(auth_year, d_0, d_k, d_k1, d_k2, d_kPrime, everything()) %>%
        relocate(all_of(c("Reference", "Note")), .after=last_col())
}

#####---------------------------------------------------------------------------
## category RRs -> per study ERR + CI using parametric bootstrapping
## Doi et al. 2014
#####---------------------------------------------------------------------------

## parametric bootstrap replicate
## draw category RRs independently from lognormal distribution
## with given log mean and log sd
## x: data frame with category RRs and logRR-SEs
## mle: ML parameter estimate vector (not used)
rGen <- function(x, mle) {
    x %>% mutate(RR_cat=rlnorm(n(), log(RR_cat), RR_cat_logSE))
}

## parametric bootstrap replicate
## draw category RRs from multivariate log-normal distribution
## with given mean vector and covariance matrix from uniform correlation
## x: data frame with category RRs and logRR-SEs
## mle: ML parameter estimate vector (not used)
rGen_correlated <- function(x, mle, cor_intra=0.5) {
    n_obs <- nrow(x)
    R_mat <- matrix(rep(cor_intra, times=n_obs^2), nrow=n_obs) # correlation matrix
    diag(R_mat) <- 1
    S_mat <- diag(x[["RR_cat_logSE"]], nrow=n_obs) # diagonal matrix std devs log scale
    K_mat <- S_mat %*% R_mat %*% S_mat             # covariance matrix log scale
    
    ## multivariate normal = exp(multivariate lognormal)
    x %>% mutate(RR_cat=exp(c(rmvnorm(1, mean=log(RR_cat), sigma=K_mat))))
}

## fit ERR model based on category RRs and return coefficient estimates
## Little 2001 p10: linear regression inverse variance weighted,
## no intercept, offset = 1
get_ERR <- function(x, return_fit=FALSE) {
    fit_ERR <- lm(RR_cat ~ d_kPrime - 1 + offset(off),
                  weights=w,
                  data=x)
    
    if(return_fit) {
        fit_ERR
    } else {
        coef(fit_ERR)
    }
}

## fit ERR model based on category RRs and return coefficient estimates
## use gls() to allow for correlation with corCompSymm()
## use varFixed() and lmeControl(sigma=1) to use estimated variances
get_ERR_correlated <- function(x, return_fit=FALSE, cor_intra=0.5) {
    ## alternative approach
    # vfFixed  <- varFixed(~ RR_cat_SE2)
    # vfFixedI <- initialize(vfFixed, data=x)
    # varWeights(vfFixedI)
    ## gls() cannot deal with offset() term
    ## -> use RR_cat - 1
    x$RR_cat_m1 <- x$RR_cat - 1
    fit_ERR <- try(gls(RR_cat_m1 ~ d_kPrime - 1,
                       correlation=corCompSymm(value=cor_intra, form=~ 1),
                       weights=varFixed(~ RR_cat_logSE2), # variance proportional to logSE^2
                       # weights=varFixed(~ RR_cat_SE2),  # variance equal to ~ RR_cat_SE2
                       # control=lmeControl(sigma=1),     # needed when using "variance equal to"
                       data=x))
    
    if(return_fit) {
        fit_ERR
    } else {
        if(!inherits(fit_ERR, "try-error")) {
            coef(fit_ERR)
        } else {
            NA_real_
        }
    }
}

## get ERR estimate + CI using parametric bootstrap
## nR: number of bootstrap replicates
## ERR_method: use either linear model for uncorrelated data (Little 2001)
##             or linear model for correlated data (generalized least squares)
##             (Berrington & Cox 2003)
## BSCI_method: bootstrap confidence interval method
get_ERR_CI <- function(x,
                       n_repl=100,
                       cor_intra=0.5,
                       CI_width=0.95,
                       ERR_method=c("lm", "gls"),
                       BSCI_method=c("perc", "norm", "basic")) {
    ERR_method  <- match.arg(ERR_method)
    BSCI_method <- match.arg(BSCI_method)
    
    ## bootstrap CI -> name of output component != CI type
    BSCI_method_out <- unname(c(perc="percent",
                                norm="normal",
                                basic="basic")[BSCI_method])
    
    ## indices where CI is stored in returned boot object
    bs_ci_idx_out <- if(BSCI_method == "norm") {
        2:3
    } else {
        4:5
    }
    
    ## proceed only if category RRs and variance weights are available
    x_sub <- x %>%
        select(auth_year,
               d_kPrime, RR_cat, RR_cat_logSE, RR_cat_logSE2, RR_cat_SE2,
               off, w) %>%
        na.omit()
    
    if(nrow(x_sub) >= 1L) {
        ## parametric bootstrapping for ERR CI
        if(ERR_method == "lm") {
            fit_ERR <- get_ERR(x_sub, return_fit=TRUE) # ML point estimate
            pbs_ERR <- boot(x_sub,
                            statistic=get_ERR,
                            R=n_repl,
                            sim="parametric",
                            mle=fit_ERR,
                            ran.gen=rGen)
            
            ERR_slope  <- unname(coef(fit_ERR)["d_kPrime"])
            boot_out   <- boot.ci(pbs_ERR, conf=CI_width, type=BSCI_method)
            ERR_CI_pbs <- boot_out[[BSCI_method_out]][bs_ci_idx_out]
        } else if(ERR_method == "gls") {
            fit_ERR <- get_ERR_correlated(x_sub, return_fit=TRUE, cor_intra=cor_intra)
            
            ## gls might fail
            if(!inherits(fit_ERR, "try-error")) {
                pbs_ERR <- boot(x_sub,
                                statistic=get_ERR_correlated,
                                cor_intra=cor_intra,
                                R=n_repl,
                                sim="parametric",
                                mle=fit_ERR,
                                ran.gen=rGen_correlated)
                
                ERR_slope  <- unname(coef(fit_ERR)["d_kPrime"])
                boot_out   <- boot.ci(pbs_ERR, conf=CI_width, type=BSCI_method)
                ERR_CI_pbs <- boot_out[[BSCI_method_out]][bs_ci_idx_out]
            } else {
                ERR_slope  <- NA_real_
                ERR_CI_pbs <- c(NA_real_, NA_real_)
            }
        }
    } else {
        ERR_slope  <- NA_real_
        ERR_CI_pbs <- c(NA_real_, NA_real_)
    }
    
    ## return ERR estimate with bootstrap CI
    data.frame(auth_year   =x$auth_year[1],
               ERR         =ERR_slope,
               ERR_CIlo    =ERR_CI_pbs[1],
               ERR_CIup    =ERR_CI_pbs[2],
               from        =ERR_method,
               CI_width_ERR=CI_width,
               stringsAsFactors=FALSE)
}

## if only OR / RR / SIR / IRR available -> use as proxy for ERR
get_ERR_from_ORRRSIRIRR <- function(x) {
    idx_no_ERR         <- is.na(x$ERR)
    idx_have_RR        <- !is.na(x$RR) & !is.na(x$RR_CIlo) & !is.na(x$RR_CIup)
    idx_have_dose_mean <- !is.na(x$dose_mean)
    idx <- idx_no_ERR & idx_have_RR & idx_have_dose_mean
    
    x$ERR[idx]      <- (x$RR[idx]      - 1) / x$dose_mean[idx]
    x$ERR_CIlo[idx] <- (x$RR_CIlo[idx] - 1) / x$dose_mean[idx]
    x$ERR_CIup[idx] <- (x$RR_CIup[idx] - 1) / x$dose_mean[idx]
    
    x
}

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## collect ERR estimates from category-specific RRs
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

get_all_ERR_from_RR <- function(x, n_repl=1000, cor_intra=0.5) {
    ## for each study/endpoint, get ERR and CI
    d_RR_spl <- split(x, x$auth_year)
    ERRL_lm  <- lapply(d_RR_spl,
                       get_ERR_CI,
                       n_repl=n_repl,
                       ERR_method="lm",
                       BSCI_method="perc")
    
    ERRL_gls <- lapply(d_RR_spl,
                       get_ERR_CI,
                       n_repl=n_repl,
                       cor_intra=cor_intra,
                       ERR_method="gls",
                       BSCI_method="perc")
    
    d_ERR_lm  <- bind_rows(ERRL_lm)  %>% tibble::remove_rownames()
    d_ERR_gls <- bind_rows(ERRL_gls) %>% tibble::remove_rownames()
    d_ERR_org <- x %>%
        ## if only OR / RR / SIR / IRR available -> use as proxy for ERR
        get_ERR_from_ORRRSIRIRR() %>%
        select(auth_year, CI_width_ERR, ERR, ERR_CIlo, ERR_CIup) %>%
        mutate(from="org") %>%
        unique()
    
    ## combine and calculate SE assuming normality or log-normality of RR
    d_ERR_long <- bind_rows(d_ERR_org, d_ERR_lm, d_ERR_gls) %>%
        mutate(ERR_SE    =get_ERR_SE_from_CI(ERR_CIlo, ERR_CIup, CI_width_ERR),
               ERR_SE2   =ERR_SE^2,
               ERR_logSE =get_ERR_logSE_from_CI(ERR_CIlo, ERR_CIup, CI_width_ERR),
               ERR_logSE2=ERR_logSE^2,
               ## ERR_logSE may not be available when lower CI bound < -1
               ## -> take log(SE) instead
               ## note: introduces bias as get_ERR_SE_from_CI() assumes
               ## normality (symmetry on linear scale)
               ERR_SE_meta=if_else(is.na(ERR_logSE),
                                   lognorm_org_to_log(ERR+1, ERR_SE)$sdlog,
                                   ERR_logSE))
    
    ## reshape to wide format
    d_ERR_wide <- d_ERR_long %>%
        as.data.frame() %>%
        reshape(direction="wide",
                idvar=c("auth_year"),
                timevar="from",
                v.names=c("ERR", "ERR_CIlo", "ERR_CIup", "CI_width_ERR",
                          "ERR_SE", "ERR_SE2",
                          "ERR_logSE", "ERR_logSE2",
                          "ERR_SE_meta"),
                sep="_")
    
    list(d_ERR_long=d_ERR_long,
         d_ERR_wide=d_ERR_wide)
}

get_single_ERR_from_RRs <- function(x, cor_intra=0.5, CI_width=0.95) {
    ## studies with only category RR + CIs but no ERR/Gy
    d_fit_cat <- x %>%
        filter(is.na(ERR)) %>%
        ## gls() does not deal with offset() terms -> use RR_cat - 1
        mutate(RR_cat_m1=RR_cat - 1) %>%
        select(auth_year, d_kPrime, RR_cat_m1, RR_cat_CIlo, RR_cat_CIup,
               RR_cat_logSE2, w, off) %>%
        na.omit()
    
    ## correlation between category RRs fixed to cor_intra
    ## get ERR/Gy estimate from category-RR-only studies
    ## gls() does not deal with offset() terms
    fit_gls <- try(gls(RR_cat_m1 ~ d_kPrime - 1,
                       correlation=corCompSymm(value=cor_intra, form=~ 1 | auth_year),
                       weights=varFixed(~ RR_cat_logSE2), # variance proportional to logSE^2
                       data=d_fit_cat))
    
    if(!inherits(fit_gls, "try-error")) {
        err_coef <- unname(coef(fit_gls)["d_kPrime"])
        err_ci   <- confint(fit_gls, level=CI_width)
        used     <- "gls"
    } else {
        fit_lm <- lm(RR_cat ~ d_kPrime - 1 + offset(off),
                     weights=w,
                     data=x)
        
        err_coef <- unname(coef(fit_lm)["d_kPrime"])
        err_ci   <- confint(fit_lm, level=CI_width)
        used     <- "lm"
    }
    
    data.frame(auth_year="Category_RRs",
               ERR     =err_coef,
               ERR_CIlo=err_ci[1],
               ERR_CIup=err_ci[2]) %>%
        mutate(ERR_SE    =get_ERR_SE_from_CI(ERR_CIlo, ERR_CIup, CI_width),
               ERR_SE2   =ERR_SE^2,
               ERR_logSE =get_ERR_logSE_from_CI(ERR_CIlo, ERR_CIup, CI_width),
               ERR_logSE2=ERR_logSE^2,
               ## ERR_logSE may not be available when lower CI bound < -1
               ## then estimate based on SE instead
               ## note: introduces bias as get_ERR_SE_from_CI() assumes
               ## normality (symmetry on linear scale)
               ERR_SE_meta=if_else(is.na(ERR_logSE),
                                   lognorm_org_to_log(ERR+1, ERR_SE)$sdlog,
                                   ERR_logSE),
               CI_width_ERR=CI_width,
               used=used)
}

try_get <- function(x) {
    if(exists(x)) { get(x) }
}

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## metaanalysis for ERR values
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

## determine which values is available and thus used for metaanalysis
## original, gls, lm, none
get_used <- function(val_org, val_gls, val_lm) {
    used <- if_else(is.na(val_org),
                    if_else(is.na(val_gls),
                                  if_else(is.na(val_lm),
                                          "none",
                                          "lm"),
                            "gls"),
                    "org")
    
    factor(used, levels=c("org", "gls", "lm", "none"))
}

## transform log RR <-> ERR
transf_logRR_to_ERR <- function(x) { exp(x) - 1 }
transf_ERR_to_logRR <- function(x) { log(x + 1) }

## lognormal distribution: convert parameters between log and original scale
lognorm_log_to_org <- function(meanlog, sdlog) {
    varlog <- sdlog^2
    list(mu   =exp(meanlog + 0.5*varlog),
         sigma=sqrt((exp(varlog) - 1)*exp(2*meanlog + varlog)))
}

lognorm_org_to_log <- function(mu, sigma) {
    list(meanlog=      2*log(mu) - 0.5*log(sigma^2 + mu^2),
         sdlog  =sqrt(-2*log(mu) +     log(sigma^2 + mu^2)))
}

## pooled logRR from random-effects meta-analysis
## assume log-normal distribution for ERR+1
get_logRR_pooled <- function(x) {
    rma.uni(log(ERR+1),
            sei=ERR_SE_meta,
            method="REML",
            data=x,
            slab=auth_year)
}

## pooled ERR from random-effects meta-analysis
## assume normal distribution for ERR
get_ERR_pooled <- function(x) {
    rma.uni(ERR,
            sei=ERR_SE,
            method="REML",
            data=x,
            slab=auth_year)
}

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## check using data provided in Little 2012, used in Richardson 2020
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

# d_little2012 <- as.data.frame(rbind(
#     c(1,  0.12,  0.051,  0.186,	5.92),
#     c(2,  0.41,  0.05, 	 0.78, 	0.50),
#     c(3,  0.15, -0.14, 	 0.58, 	0.12),
#     c(4,  4.10, -2.9, 	13.7, 	0.60),
#     c(5,  0.26, -0.05, 	 0.61, 	0.40),
#     c(6,  0.02, -0.1, 	 0.15, 	4.00),
#     c(7, -0.01, -0.59, 	 0.69, 	0.50),
#     c(8,  0.05, -0.05, 	 0.16, 	4.00))
# )
# 
# d_little2012_mod <- d_little2012 %>%
#     setNames(c("auth_year", "ERR", "l", "u", "x")) %>%
#     mutate(l_imputed=is.na(l) & !is.na(u) & !is.na(ERR),
#            l=if_else(l_imputed,
#                      ## min lower CI bound = 0?
#                      # pmin(exp(log(ERR+1) - (log(ERR_CIup+1)-log(ERR+1))) - 1, 0),
#                      exp(log(ERR+1) - (log(u+1)-log(ERR+1))) - 1,
#                      as.numeric(l)),
#            ERR_SE=get_ERR_SE_from_CI(l, u, 0.95),
#            ERR_logSE=get_ERR_logSE_from_CI(l, u, 0.95),
#            ## ERR_logSE may not be available when lower CI bound < -1
#            ## then estimate based on SE instead
#            ## note: introduces bias as get_ERR_SE_from_CI() assumes
#            ## normality (symmetry on linear scale)
#            ERR_SE_meta=if_else(is.na(ERR_logSE),
#                                lognorm_org_to_log(ERR+1, ERR_SE)$sdlog,
#                                ERR_logSE))
# 
# pooled_logRR <- get_logRR_pooled(d_little2012_mod)
# predict(pooled_logRR)
