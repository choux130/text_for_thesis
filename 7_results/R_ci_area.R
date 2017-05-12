####################
####### Area #######
####################

# SET DATA ####
datt=read.csv("/Users/chou/Google Drive/Spring2016/Plan B/final/clean_dat.csv",header=TRUE)
datt.area=datt$Area
datt.area_p=datt[datt$PMD=="P",]$Area
datt.area_m=datt[datt$PMD=="M",]$Area
datt.area_d=datt[datt$PMD=="D",]$Area

## Function: Weighted Mean ####
fun_wm.area=function(data.area, data.area_p, data.area_m, data.area_d){
  w.area.all=sum(data.area)/data.area
  wm.area.all=sum(data.area*w.area.all)/sum(w.area.all) 
  
  w.area.p=sum(data.area_p)/data.area_p
  wm.area.p=sum(data.area_p*w.area.p)/sum(w.area.p) 
  w.area.m=sum(data.area_m)/data.area_m
  wm.area.m=sum(data.area_m*w.area.m)/sum(w.area.m) 
  w.area.d=sum(data.area_d)/data.area_d
  wm.area.d=sum(data.area_d*w.area.d)/sum(w.area.d) 
  
  output_wm.area=c(wm.area.all,wm.area.p,wm.area.m,wm.area.d)
  return(output_wm.area)
}

# BOOTSTRAP FOR CI #### 
## Setting Arguments ####
reps=10000
set.seed(12345)

set_wm.area.all=numeric(length=reps)
set_wm.area.p=numeric(length=reps)
set_wm.area.m=numeric(length=reps)
set_wm.area.d=numeric(length=reps)

set_mle.area.all=numeric(length=reps)
set_mle.area.p=numeric(length=reps)
set_mle.area.m=numeric(length=reps)
set_mle.area.d=numeric(length=reps)

set_diff_wm.area.pm = numeric(length=reps)
set_diff_wm.area.md = numeric(length=reps)
set_diff_wm.area.pd = numeric(length=reps)
set_diff_mle.area.pm = numeric(length=reps)
set_diff_mle.area.md = numeric(length=reps)
set_diff_mle.area.pd = numeric(length=reps)

## Bootstrap Algorithm ####
for (i in 1:reps){
  boot.datt.area=sample(datt.area,length(datt.area),replace=T)
  boot.datt.area_p=sample(datt.area_p,length(datt.area_p),replace=T)
  boot.datt.area_m=sample(datt.area_m,length(datt.area_m),replace=T)
  boot.datt.area_d=sample(datt.area_d,length(datt.area_d),replace=T)
  
  wm.area_boot=fun_wm.area(boot.datt.area, boot.datt.area_p, 
                           boot.datt.area_m, boot.datt.area_d) 
  
  set_wm.area.all[i]=wm.area_boot[1]
  set_wm.area.p[i]=wm.area_boot[2]
  set_wm.area.m[i]=wm.area_boot[3]
  set_wm.area.d[i]=wm.area_boot[4]
  
  set_mle.area.all[i]=mean(boot.datt.area)/2
  set_mle.area.p[i]=mean(boot.datt.area_p)/2
  set_mle.area.m[i]=mean(boot.datt.area_m)/2
  set_mle.area.d[i]=mean(boot.datt.area_d)/2
  
  set_diff_wm.area.pm[i] = wm.area_boot[2] - wm.area_boot[3]
  set_diff_wm.area.md[i] = wm.area_boot[3] - wm.area_boot[4]
  set_diff_wm.area.pd[i] = wm.area_boot[2] - wm.area_boot[4]
  set_diff_mle.area.pm[i] = set_mle.area.p[i] - set_mle.area.m[i]
  set_diff_mle.area.md[i] = set_mle.area.m[i] - set_mle.area.d[i]
  set_diff_mle.area.pd[i] = set_mle.area.p[i] - set_mle.area.d[i]
}

## CI ####
### overall and every locations ####
ci_wm.area.all=quantile(set_wm.area.all,c(0.05/2,1-0.05/2)) # 1400.362 1842.506  
ci_wm.area.p=quantile(set_wm.area.p,c(0.05/2,1-0.05/2)) # 1680.974 2243.383  
ci_wm.area.m=quantile(set_wm.area.m,c(0.05/2,1-0.05/2)) # 2038.772 2791.910
ci_wm.area.d=quantile(set_wm.area.d,c(0.05/2,1-0.05/2)) # 873.0193 1350.5643

ci_mle.area.all=quantile(set_mle.area.all,c(0.05/2,1-0.05/2)) # 1062.192 1310.405 
ci_mle.area.p=quantile(set_mle.area.p,c(0.05/2,1-0.05/2)) # 1032.411 1426.765 
ci_mle.area.m=quantile(set_mle.area.m,c(0.05/2,1-0.05/2)) # 1276.535 1686.788 
ci_mle.area.d=quantile(set_mle.area.d,c(0.05/2,1-0.05/2)) # 669.2359 1055.7634 

### difference in each pairs ####
#### P vs. M 
ci_wm.area.pm=quantile(set_diff_wm.area.pm,c(0.0167/2,1-0.0167/2)) # -1036.8946   120.9274
ci_mle.area.pm=quantile(set_diff_mle.area.pm,c(0.0167/2,1-0.0167/2)) # -602.7395  104.3074

#### M vs. D
ci_wm.area.md=quantile(set_diff_wm.area.md,c(0.0167/2,1-0.0167/2)) # 767.2153 1869.3910 
ci_mle.area.md=quantile(set_diff_mle.area.md,c(0.0167/2,1-0.0167/2)) # 274.5067 961.3635 

#### P vs. D
ci_wm.area.pd=quantile(set_diff_wm.area.pd,c(0.0167/2,1-0.0167/2)) # 400.3935 1313.6611 
ci_mle.area.pd=quantile(set_diff_mle.area.pd,c(0.0167/2,1-0.0167/2)) # 27.9301 711.4919 

#################################################################################################
## Plots for the Bootstrap Distribution ####
### overall and every locations ####
#dist.boot_wm_area=ggplot(data.frame(x=set.wm_all),aes(x=set.wm_all))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, P vs. M")+
#  geom_vline(aes(xintercept=ci.boot_wm_area[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_wm_area[2]), color="red", linetype="dashed", size=1)
#dist.boot_wm_area_p=ggplot(data.frame(x=set.wm_p),aes(x=set.wm_p))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, P vs. M")+
#  geom_vline(aes(xintercept=ci.boot_wm_area_p[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_wm_area_p[2]), color="red", linetype="dashed", size=1)
#dist.boot_wm_area_m=ggplot(data.frame(x=set.wm_m),aes(x=set.wm_m))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, P vs. M")+
#  geom_vline(aes(xintercept=ci.boot_wm_area_m[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_wm_area_m[2]), color="red", linetype="dashed", size=1)
#dist.boot_wm_area_d=ggplot(data.frame(x=set.wm_d),aes(x=set.wm_d))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, P vs. M")+
#  geom_vline(aes(xintercept=ci.boot_wm_area_d[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_wm_area_d[2]), color="red", linetype="dashed", size=1)

#dist.boot_mle_area=ggplot(data.frame(x=set.mle_all),aes(x=set.mle_all))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, P vs. M")+
#  geom_vline(aes(xintercept=ci.boot_mle_area[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_mle_area[2]), color="red", linetype="dashed", size=1)
#dist.boot_mle_area_p=ggplot(data.frame(x=set.mle_p),aes(x=set.mle_p))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, P vs. M")+
#  geom_vline(aes(xintercept=ci.boot_mle_area_p[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_mle_area_p[2]), color="red", linetype="dashed", size=1)
#dist.boot_mle_area_m=ggplot(data.frame(x=set.mle_m),aes(x=set.mle_m))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, P vs. M")+
#  geom_vline(aes(xintercept=ci.boot_mle_area_m[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_mle_area_m[2]), color="red", linetype="dashed", size=1)
#dist.boot_mle_area_d=ggplot(data.frame(x=set.mle_d),aes(x=set.mle_d))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, P vs. M")+
#  geom_vline(aes(xintercept=ci.boot_wm_area_d[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_wm_area_d[2]), color="red", linetype="dashed", size=1)

### difference in each pairs ####
#### P vs.M 
#dist.boot_wm_area_pm=ggplot(data.frame(x=set.diff_wm_area_pm),aes(x=set.diff_wm_area_pm))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, P vs. M")+
#  geom_vline(aes(xintercept=ci.boot_wm_area_pm[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_wm_area_pm[2]), color="red", linetype="dashed", size=1)

#dist.boot_mle_area_pm=ggplot(data.frame(x=set.diff_mle_area_pm),aes(x=set.diff_mle_area_pm))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, MLE for Area, P vs. M")+
#  geom_vline(aes(xintercept=ci.boot_mle_area_pm[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_mle_area_pm[2]), color="red", linetype="dashed", size=1)

#### M vs. D 
#dist.boot_wm_area_md=ggplot(data.frame(x=set.diff_wm_area_md),aes(x=set.diff_wm_area_md))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, M vs. D")+
#  geom_vline(aes(xintercept=ci.boot_wm_area_md[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_wm_area_md[2]), color="red", linetype="dashed", size=1)

#dist.boot_mle_area_md=ggplot(data.frame(x=set.diff_mle_area_md),aes(x=set.diff_mle_area_md))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, MLE for Area, M vs. D")+
#  geom_vline(aes(xintercept=ci.boot_mle_area_md[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_mle_area_md[2]), color="red", linetype="dashed", size=1)

#### P vs. D 
#dist.boot_wm_area_pd=ggplot(data.frame(x=set.diff_wm_area_pd),aes(x=set.diff_wm_area_pd))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, Weighted Mean  for Area, P vs. D")+
#  geom_vline(aes(xintercept=ci.boot_wm_area_pd[1]), color="red", linetype="dashed", size=1)+
#  geom_vline(aes(xintercept=ci.boot_wm_area_pd[2]), color="red", linetype="dashed", size=1)

#dist.boot_mle_area_pd=ggplot(data.frame(x=set.diff_mle_area_pd),aes(x=set.diff_mle_area_pd))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Bootstrap for CI, MLE for Area, M vs. D")+
#  geom_vline(aes(xintercept=ci.boot_mle_area_pd[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_mle_area_pd[2]), color="red", linetype="dashed", size=1)


