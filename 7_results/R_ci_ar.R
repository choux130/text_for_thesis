############################
####### Asepct Ratio #######
############################

# SET DATA ####
datt=read.csv("/Users/chou/Google Drive/Spring2016/Plan B/final/clean_dat.csv",header=TRUE)
datt.ar=datt$Aspect.Ratio
datt.ar_p=datt[datt$PMD=="P",]$Aspect.Ratio
datt.ar_m=datt[datt$PMD=="M",]$Aspect.Ratio
datt.ar_d=datt[datt$PMD=="D",]$Aspect.Ratio

# BOOTSTRAP FOR CI #### 
## Setting Arguments ####
reps=10000
set.seed(12345)

set_m.ar.all=numeric(length=reps)
set_m.ar.p=numeric(length=reps)
set_m.ar.m=numeric(length=reps)
set_m.ar.d=numeric(length=reps)

set_diff_m.ar.pm = numeric(length=reps)
set_diff_m.ar.md = numeric(length=reps)
set_diff_m.ar.pd = numeric(length=reps)

## Bootstrap Algorithm ####
for (i in 1:reps){
  boot.datt.ar=sample(datt.ar,length(datt.ar),replace=T)
  boot.datt.ar_p=sample(datt.ar_p,length(datt.ar_p),replace=T)
  boot.datt.ar_m=sample(datt.ar_m,length(datt.ar_m),replace=T)
  boot.datt.ar_d=sample(datt.ar_d,length(datt.ar_d),replace=T)
  
  m.ar_boot=c(mean(boot.datt.ar), mean(boot.datt.ar_p),
               mean(boot.datt.ar_m), mean(boot.datt.ar_d))
  
  set_m.ar.all[i]=m.ar_boot[1]
  set_m.ar.p[i]=m.ar_boot[2]
  set_m.ar.m[i]=m.ar_boot[3]
  set_m.ar.d[i]=m.ar_boot[4]
  
  set_diff_m.ar.pm[i] = m.ar_boot[2] - m.ar_boot[3]
  set_diff_m.ar.md[i] = m.ar_boot[3] - m.ar_boot[4]
  set_diff_m.ar.pd[i] = m.ar_boot[2] - m.ar_boot[4]
}

## CI ####
### overall and every locations ####
ci_m.ar.all=quantile(set_m.ar.all,c(0.05/2,1-0.05/2)) # 1.399457 1.558271 
ci_m.ar.p=quantile(set_m.ar.p,c(0.05/2,1-0.05/2)) # 1.384360 1.704902 
ci_m.ar.m=quantile(set_m.ar.m,c(0.05/2,1-0.05/2)) # 1.268968 1.490073 
ci_m.ar.d=quantile(set_m.ar.d,c(0.05/2,1-0.05/2)) # 1.409164 1.657349 

### difference in each pairs ####
#### P vs. M 
ci_m.ar.pm=quantile(set_diff_m.ar.pm,c(0.0167/2,1-0.0167/2)) # -0.07729163  0.40830669 

#### M vs. D
ci_m.ar.md=quantile(set_diff_m.ar.md,c(0.0167/2,1-0.0167/2)) # -0.36042486  0.04415264  

#### P vs. D
ci_m.ar.pd=quantile(set_diff_m.ar.pd,c(0.0167/2,1-0.0167/2)) # -0.2379284  0.2610344 

#########################################################################################
## Plots for the Bootstrap Distribution ####
### overall and every locations ####

### difference in each pairs ####
#### P vs.M 
# dist.boot_ar_pm=ggplot(data.frame(x=set.diff_ar_pm),aes(x=set.diff_ar_pm))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, Weighted Mean  for Aspect Ratio, P vs. M")+
#   geom_vline(aes(xintercept=ci.boot_ar_pm[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_ar_pm[2]), color="red", linetype="dashed", size=1)

# #### M vs. D
# dist.boot_ar_md=ggplot(data.frame(x=set.diff_ar_md),aes(x=set.diff_ar_md))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, Weighted Mean  for Aspect Ratio, M vs. D")+
#   geom_vline(aes(xintercept=ci.boot_ar_md[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_ar_md[2]), color="red", linetype="dashed", size=1)

# #### P vs. D 
# dist.boot_ar_pd=ggplot(data.frame(x=set.diff_ar_pd),aes(x=set.diff_ar_pd))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, Weighted Mean  for Asepct Ratio, M vs. D")+
#   geom_vline(aes(xintercept=ci.boot_ar_pd[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_ar_pd[2]), color="red", linetype="dashed", size=1)





