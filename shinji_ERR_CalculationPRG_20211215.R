#R version 4.1.2 (2021-11-01)

#Replace with study name, number of category 0 to N-1, dose estimete in each category, point estimetes of RR, and lower and upper 95% confidence limits
study<-c("Tucker","Tucker","Tucker","Tucker")
category<-c(0,1,2,3)
dose<-c(1,6,20,53)
estimate<-c(1,13.1,12.1,17.6)
lower<-c(NA,1.5,1.3,1.4)
upper<-c(NA,114,117,226)

estimates<-data.frame(study=study,cat=category,dose=dose,estimate=estimate,lower=lower,upper=upper)


#*************************************************************************;
#* Treatment for reference category                                      *;
#*************************************************************************;

#Offset value is set as dose for reference category. For study without reference category, offset value is set as 0.
estimates$offset<-0*(1:nrow(estimates))
offset_istudy<-0*(1:nlevels(factor(estimates$study)))

for (istudy in 1:nlevels(factor(estimates$study))){
  sub_istudy<-subset(estimates,estimates$study==levels(factor(estimates$study))[istudy])
  if(min(sub_istudy$cat)==0){
      offset_istudy[istudy]<-sub_istudy$dose[sub_istudy$cat==0]
  }
  for(i in 1:nrow(estimates)){
       if(estimates$study[i]==levels(factor(estimates$study))[istudy]){
          estimates$offset[i]<-offset_istudy[istudy]  
       }
  }
}

estimates$mod_dose<-estimates$dose-estimates$offset
estimates$mod_estimate<-estimates$estimate-1

#Calculation of SE in log scale
estimates$logest  <- log(estimates$estimate)
estimates$logdiff <- log(estimates$upper)-log(estimates$lower)
estimates$logse   <-     estimates$logdiff/(2*qnorm(0.975,0,1))
estimates$se      <- exp(estimates$logdiff/(2*qnorm(0.975,0,1)))
estimates$logvar  <- estimates$logse*estimates$logse

estimates$var<-estimates$se*estimates$se

#Calculation of ERR by point estimates for each study
ERR_est_p<-0*(1:nlevels(factor(estimates$study)))
for (istudy in 1:nlevels(factor(estimates$study))){
  sub_istudy<-subset(estimates,(estimates$study==levels(factor(estimates$study))[istudy])&(estimates$cat!=0))
  res<-lm("sub_istudy$mod_estimate~0+sub_istudy$mod_dose",weight = 1/(sub_istudy$var))
  ERR_est_p[istudy]<-res$coefficients[1]
}

#Estimarion of ERR and its 95% credible interval by 20,000 set of bootstrup data
trial<-20000
ERR_est_b<-0*(1:nlevels(factor(estimates$study)))
ERR_upper_b<-0*(1:nlevels(factor(estimates$study)))
ERR_lower_b<-0*(1:nlevels(factor(estimates$study)))
for (istudy in 1:nlevels(factor(estimates$study))){
  sub_istudy<-subset(estimates,estimates$study==levels(factor(estimates$study))[istudy]&estimates$cat!=0)
  maxcate<-nrow(sub_istudy)
  ERR_genest_b<-0*(1:trial)
  genest<-0*(1:maxcate)
  gendose<-0*(1:maxcate)
  genvar<-0*(1:maxcate)
  for(itrial in 1:trial){
    for(icat in 1:maxcate){
      genest[icat]<-exp(rnorm(1)*sub_istudy$logse[icat]+sub_istudy$logest[icat])-1
      gendose[icat]<-sub_istudy$mod_dose[icat]
      genvar[icat]<-sub_istudy$var[icat]
    }#icat
    res<-lm("genest~0+gendose",weight = 1/(genvar))
    ERR_genest_b[itrial]<-res$coefficients[1]

  }#itrial
  ERR_est_b[istudy]<-mean(ERR_genest_b)
  ERR_lower_b[istudy]<-quantile(ERR_genest_b,c(0.025))
  ERR_upper_b[istudy]<-quantile(ERR_genest_b,c(0.975))
}#istudy

for (istudy in 1:nlevels(factor(estimates$study))){
  print(levels(factor(estimates$study))[istudy])
  print(ERR_est_p[istudy])
  print(ERR_lower_b[istudy])
  print(ERR_upper_b[istudy])
}
