###########################
####### Circularity ######
##########################

# SET DATA ####
datt=read.csv("/Users/chou/Google Drive/Spring2016/Plan B/final/clean_dat.csv",header=TRUE)
datt.cir=datt$Circularity
datt.cir_p=datt[datt$PMD=="P",]$Circularity
datt.cir_m=datt[datt$PMD=="M",]$Circularity
datt.cir_d=datt[datt$PMD=="D",]$Circularity


# BOOTSTRAP FOR CI #### 
## Setting Arguments ####
reps=10000
set.seed(12345)

set_m.cir.all=numeric(length=reps)
set_m.cir.p=numeric(length=reps)
set_m.cir.m=numeric(length=reps)
set_m.cir.d=numeric(length=reps)

set_diff_m.cir.pm = numeric(length=reps)
set_diff_m.cir.md = numeric(length=reps)
set_diff_m.cir.pd = numeric(length=reps)


## Bootstrap Algorithm ####
for (i in 1:reps){
  boot.datt.cir=sample(datt.cir,length(datt.cir),replace=T)
  boot.datt.cir_p=sample(datt.cir_p,length(datt.cir_p),replace=T)
  boot.datt.cir_m=sample(datt.cir_m,length(datt.cir_m),replace=T)
  boot.datt.cir_d=sample(datt.cir_d,length(datt.cir_d),replace=T)
  
  m.cir_boot=c(mean(boot.datt.cir), mean(boot.datt.cir_p),
                mean(boot.datt.cir_m), mean(boot.datt.cir_d))
  
  set_m.cir.all[i]=m.cir_boot[1]
  set_m.cir.p[i]=m.cir_boot[2]
  set_m.cir.m[i]=m.cir_boot[3]
  set_m.cir.d[i]=m.cir_boot[4]
  
  set_diff_m.cir.pm[i] = m.cir_boot[2] - m.cir_boot[3]
  set_diff_m.cir.md[i] = m.cir_boot[3] - m.cir_boot[4]
  set_diff_m.cir.pd[i] = m.cir_boot[2] - m.cir_boot[4]
}
## CI ####
### overall and every locations ####
ci_m.cir.all=quantile(set_m.cir.all,c(0.05/2,1-0.05/2)) # 0.7207111 0.7566492 
ci_m.cir.p=quantile(set_m.cir.p,c(0.05/2,1-0.05/2)) # 0.7105172 0.7760976 
ci_m.cir.m=quantile(set_m.cir.m,c(0.05/2,1-0.05/2)) # 0.7466694 0.7937179 
ci_m.cir.d=quantile(set_m.cir.d,c(0.05/2,1-0.05/2)) # 0.6681738 0.7322703 

### difference in each pairs ####
#### P vs. M 
ci_m.cir.pm=quantile(set_diff_m.cir.pm,c(0.0167/2,1-0.0167/2)) # -0.07683523  0.02251121 

#### M vs. D
ci_m.cir.md=quantile(set_diff_m.cir.md,c(0.0167/2,1-0.0167/2)) # 0.02000446 0.11876638 

#### P vs. D
ci_m.cir.pd=quantile(set_diff_m.cir.pd,c(0.0167/2,1-0.0167/2)) # -0.01389986  0.09895042 

#########################################################################################
## Plots for the Bootstrap Distribution ####
### overall and every locations ####

### difference in each pairs ####
# #### P vs.M 
# dist.boot_cir_pm=ggplot(data.frame(x=set.diff_cir_pm),aes(x=set.diff_cir_pm))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, Weighted Mean  for Circularity, P vs. M")+
#   geom_vline(aes(xintercept=ci.boot_cir_pm[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_cir_pm[2]), color="red", linetype="dashed", size=1)
# 
# #### M vs.D
# dist.boot_cir_md=ggplot(data.frame(x=set.diff_cir_md),aes(x=set.diff_cir_md))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, Weighted Mean  for Circularity, M vs. D")+
#   geom_vline(aes(xintercept=ci.boot_cir_md[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_cir_md[2]), color="red", linetype="dashed", size=1)
# 
# #### P vs.D
# dist.boot_cir_pd=ggplot(data.frame(x=set.diff_cir_pd),aes(x=set.diff_cir_pd))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, Weighted Mean  for Circularity, M vs. D")+
#   geom_vline(aes(xintercept=ci.boot_cir_pd[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_cir_pd[2]), color="red", linetype="dashed", size=1)















