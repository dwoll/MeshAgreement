## Richardson et al. Meta-analysis of published excess relative risk estimates.
## Radiat Environ Biophys 59, 631–641 (2020).
## https://doi.org/10.1007/s00411-020-00863-w
# R helper functions
print.metaan <- function(x, ...){
    retmat = cbind(x$err_tot, x$sd_tot, x$l_tot, x$u_tot)
    colnames(retmat) <- c("ERR(total)", "SE(ERR)", "Lower 95% CI", "Upper 95% CI")
    rownames(retmat) <- x$type
    if(any(is.na(x$sd_tot))) retmat = retmat[,-2, drop=FALSE]
    printCoefmat(retmat)
}


# original data from empirical exmample
source = as.data.frame(rbind(
    c(1,	0.12,	  0.051, 	0.186, 	5.92),
    c(2,	0.41,	  0.05, 	0.78, 	0.50),
    c(3,	0.15,	  -0.14, 	0.58, 	0.12),
    c(4,	4.10,	  -2.9, 	13.7, 	0.60),
    c(5,	0.26,	  -0.05, 	0.61, 	0.40),
    c(6,	0.02,	  -0.1, 	0.15, 	4.00),
    c(7,	-0.01,  -0.59, 	0.69, 	0.50),
    c(8,	0.05,	  -0.05, 	0.16, 	4.00))
)

names(source) <- c("i", "err", "l", "u", "x")

# Fixed effect meta-analysis (standard)
ma_fixed_std <- function(err, u, l){
    sd = (u-l)/(2*1.96)
    var = sd^2
    sum_num = sum(err/var, na.rm = T)
    sum_den = sum(1/var, na.rm = T)
    err_tot = sum_num/sum_den
    sd_tot = 1/(sqrt(sum_den))
    ret = list(err_tot = err_tot,
               sd_tot = sd_tot,
               l_tot = err_tot - 1.96*sd_tot,
               u_tot = err_tot + 1.96*sd_tot,
               type="Standard approach: fixed effect"
    )
    class(ret) <- "metaan"
    ret
}

# Random effect meta-analysis (standard)
ma_random_std <- function(err, u, l){
    sd = (u-l)/(2*1.96)
    var = sd^2
    err_tot1 = sum(err/var, na.rm = T)/sum(1/var, na.rm = T)
    q = sum(((err - err_tot1)/sd)^2)
    sum_num_rand = sum(1/sd^4, na.rm = T)
    sum_den_rand = sum(1/var, na.rm = T)
    nstudies = length(err)
    deltasq = max(0, (q - (nstudies-1)) / (sum_den_rand - (sum_num_rand/sum_den_rand)))
    sum_num = sum(err/(var + deltasq))
    sum_den = sum(1/(var + deltasq))
    err_tot = sum_num/sum_den
    sd_tot = 1/sqrt(sum_den)
    ret = list(err_tot = err_tot,
               sd_tot = sd_tot,
               l_tot = err_tot - 1.96*sd_tot,
               u_tot = err_tot + 1.96*sd_tot,
               type="Standard approach: random effect"
    )
    class(ret) <- "metaan"
    ret
}


# Fixed effect meta-analysis (alternative approach)
ma_fixed_alt <- function(err, u, l, d){
    C = min(d, na.rm=T)
    A = log(C*err+1)
    sd_A = log((C*u+1)/(C*l+1))/(2*1.96)
    var_A = sd_A^2
    sum_num = sum(A/var_A, na.rm = T)
    sum_den = sum(1/var_A, na.rm = T)
    A_tot = sum_num/sum_den
    sd_Atot = 1/(sqrt(sum_den))
    ret = list(err_tot = (exp(A_tot)-1)/C,
               sd_tot = NA,
               l_tot = (exp(A_tot - 1.96*sd_Atot)-1)/C,
               u_tot = (exp(A_tot + 1.96*sd_Atot)-1)/C,
               type="Proposed approach: fixed effect"
    )
    class(ret) <- "metaan"
    ret
}

# Random effect meta-analysis (alternative approach)
ma_random_alt <- function(err, u, l, d){
    C=min(d, na.rm=T)
    A = log(C*err+1)
    sd_A = log((C*u+1)/(C*l+1))/(2*1.96)
    var_A = sd_A^2
    A_tot = sum(A/var_A, na.rm = T)/sum(1/var_A, na.rm = T)
    #err_tot1 = err_tot = (exp(A_tot)-1)/C
    q = sum(((A - A_tot)/sd_A)^2)
    sum_num_rand = sum(1/sd_A^4, na.rm = T)
    sum_den_rand = sum(1/var_A, na.rm = T)
    nstudies = length(A)
    deltasq = max(0, (q - (nstudies-1)) / (sum_den_rand - (sum_num_rand/sum_den_rand)))
    sum_num = sum(A/(var_A + deltasq))
    sum_den = sum(1/(var_A + deltasq))
    A_tot = sum_num/sum_den
    sd_Atot = 1/sqrt(sum_den)
    ret = list(err_tot = (exp(A_tot)-1) / C,
               sd_tot = NA,
               l_tot = (exp(A_tot - 1.96*sd_Atot)-1)/C,
               u_tot = (exp(A_tot + 1.96*sd_Atot)-1)/C,
               type="Proposed approach: random effect"
    )
    class(ret) <- "metaan"
    ret
}

ma_fixed_std(source$err, source$u, source$l)
#                                 ERR(total)  SE(ERR) Lower 95% CI Upper 95% CI
# Standard approach: fixed effect   0.096773 0.025477     0.046838       0.1467

ma_random_std(source$err, source$u, source$l)
#                                  ERR(total)  SE(ERR) Lower 95% CI Upper 95% CI
# Standard approach: random effect   0.095428 0.028984     0.038619       0.1522

ma_fixed_alt(source$err, source$u, source$l, source$x)
#                                 ERR(total) Lower 95% CI Upper 95% CI
# Proposed approach: fixed effect   0.097624     0.047849       0.1477

ma_random_alt(source$err, source$u, source$l, source$x)
#                                  ERR(total) Lower 95% CI Upper 95% CI
# Proposed approach: random effect   0.096300     0.035343       0.1577

# compare to:
# the fixed effect meta-analytical result reported by Little et al.
#   (ERR_tot = 0.10; 95%CI: 0.05, 0.15)
#  the random effects meta-analytical result reported by Little et al
#   (ERR_tot = 0.10; 95%CI: 0.05, 0.15)
