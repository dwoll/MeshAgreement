#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Daniel Wollschlaeger <wollschlaeger@uni-mainz.de>
## 2022-10-21
## simulate category RRs from continuous dose-response relationships
## in order to estimate the correlation between RR estimates due to
## same reference group
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

sim_1 <- function(n_obs=1000,
                  ERR=0.1,
                  dose_range=c(0, 20),
                  n_cat=4,
                  meanlog_dose=1,
                  sdlog_dose=1,
                  sdlog_RR=0.6) {
    dose_all <- rlnorm(n_obs, meanlog=meanlog_dose, sdlog=sdlog_dose)
    dose     <- dose_all[dose_all <= max(dose_range)]
    dose_cat <- cut(dose, breaks=c(-Inf,
                                   0.5,
                                   quantile(dose, probs=seq(from=0.1, to=0.9, length.out=n_cat)),
                                   Inf))
    R0       <- 1
    RR       <- (1+ERR)*dose
    R_mu     <- R0*RR
    p_event  <- plogis(R_mu)
    event    <- rbinom(length(p_event), size=1, prob=p_event)
    d_fit    <- data.frame(dose_cat=dose_cat,
                           event=event)
    
    fit_glm <- glm(event ~ dose_cat,
                   family=binomial(link="logit"),
                   data=d_fit)
    
    coef(fit_glm)[-1]
}

sims    <- replicate(1000, sim_1())
cor_mat <- cor(t(sims))
dimnames(cor_mat) <- list(paste("dcat", 1:5, sep="_"),
                          paste("dcat", 1:5, sep="_"))
cor_mat
