##########################
####### Perimeter #######
#########################

# SET DATA ####
datt=read.csv("/Users/chou/Google Drive/Spring2016/Plan B/final/clean_dat.csv",header=TRUE)
datt.area=datt$Area
datt.area_p=datt[datt$PMD=="P",]$Area
datt.area_m=datt[datt$PMD=="M",]$Area
datt.area_d=datt[datt$PMD=="D",]$Area
datt.per=datt$Perimeter
datt.per_p=datt[datt$PMD=="P",]$Perimeter
datt.per_m=datt[datt$PMD=="M",]$Perimeter
datt.per_d=datt[datt$PMD=="D",]$Perimeter
datt.cir=datt$Circularity
datt.cir_p=datt[datt$PMD=="P",]$Circularity
datt.cir_m=datt[datt$PMD=="M",]$Circularity
datt.cir_d=datt[datt$PMD=="D",]$Circularity
datt_p=datt[datt$PMD=="P",]
datt_m=datt[datt$PMD=="M",]
datt_d=datt[datt$PMD=="D",]

## Function: Weighted Mean ####
fun_wm.per=function(data.area, data.per, data.area_p, data.per_p,
                    data.area_m, data.per_m, 
                    data.area_d, data.per_d){
  w.per.all=sum(data.area)/data.area
  wm.per.all=sum(data.per*w.per.all)/sum(w.per.all) #158.7518
  
  w.per.p=sum(data.area_p)/data.area_p
  wm.per.p=sum(data.per_p*w.per.p)/sum(w.per.p) 
  w.per.m=sum(data.area_m)/data.area_m
  wm.per.m=sum(data.per_m*w.per.m)/sum(w.per.m) 
  w.per.d=sum(data.area_d)/data.area_d
  wm.per.d=sum(data.per_d*w.per.d)/sum(w.per.d) 
  
  output_wm.per=c(wm.per.all, wm.per.p, wm.per.m, wm.per.d)
  return(output_wm.per)
}

## Function: sTAE ####
fun1_stae.per=function(data.area, data.cir){
  m.area=mean(data.area)
  var.area=var(data.area)
  m.cir=mean(data.cir)
  var.cir=var(data.cir)
  
  output1_stae.per=sqrt(4*pi)*(sqrt(m.area/(2*m.cir))-
                                 (1/8)*(m.area/2)^(-3/2)*(m.cir)^(-1/2)*(var.area/2)+
                                 (3/8)*(m.area/2)^(1/2)*(m.cir)^(-5/2)*(var.cir)) 
  return(output1_stae.per)
}

fun2_stae.per=function(data.area, data.cir,
                       data.area_p, data.cir_p,
                       data.area_m, data.cir_m,
                       data.area_d, data.cir_d){
  
  output2_stae.per.all=fun1_stae.per(data.area, data.cir) #130.5544
  
  output2_stae.per.p=fun1_stae.per(data.area_p, data.cir_p) #130.5544
  output2_stae.per.m=fun1_stae.per(data.area_m, data.cir_m) #130.5544
  output2_stae.per.d=fun1_stae.per(data.area_d, data.cir_d) #130.5544
  
  output2_stae.per=c(output2_stae.per.all, output2_stae.per.p, 
                     output2_stae.per.m, output2_stae.per.d)
  return(output2_stae.per)
}

# BOOTSTRAP FOR CI #### 
## Setting Arguments ####
reps=10000
set.seed(12345)

set_wm.per.all=numeric(length=reps)
set_wm.per.p=numeric(length=reps)
set_wm.per.m=numeric(length=reps)
set_wm.per.d=numeric(length=reps)

set_stae.per.all=numeric(length=reps)
set_stae.per.p=numeric(length=reps)
set_stae.per.m=numeric(length=reps)
set_stae.per.d=numeric(length=reps)

set_diff_wm.per.pm = numeric(length=reps)
set_diff_wm.per.md = numeric(length=reps)
set_diff_wm.per.pd = numeric(length=reps)
set_diff_stae.per.pm = numeric(length=reps)
set_diff_stae.per.md = numeric(length=reps)
set_diff_stae.per.pd = numeric(length=reps)

## Bootstrap Algorithm ####
for (i in 1:reps){
  boot.datt=datt[sample(1:dim(datt)[1],dim(datt),replace=T),]
  boot.datt_p=datt_p[sample(1:dim(datt_p)[1],dim(datt_p),replace=T),]
  boot.datt_m=datt_m[sample(1:dim(datt_m)[1],dim(datt_m),replace=T),]
  boot.datt_d=datt_d[sample(1:dim(datt_d)[1],dim(datt_d),replace=T),]
  
  boot.datt.area=boot.datt$Area
  boot.datt.area_p=boot.datt_p$Area
  boot.datt.area_m=boot.datt_m$Area
  boot.datt.area_d=boot.datt_d$Area
  
  boot.datt.per=boot.datt$Perimeter
  boot.datt.per_p=boot.datt_p$Perimeter
  boot.datt.per_m=boot.datt_m$Perimeter
  boot.datt.per_d=boot.datt_d$Perimeter
  
  boot.datt.cir=boot.datt$Circularity
  boot.datt.cir_p=boot.datt_p$Circularity
  boot.datt.cir_m=boot.datt_m$Circularity
  boot.datt.cir_d=boot.datt_d$Circularity
  
  ## Overall weighted mean
  wm.per_boot=fun_wm.per(boot.datt.area, boot.datt.per, 
                         boot.datt.area_p, boot.datt.per_p, 
                         boot.datt.area_m, boot.datt.per_m, 
                         boot.datt.area_d, boot.datt.per_d)
  
  ## Overall 2nd order Taylor approximation
  stae.per_boot=fun2_stae.per(boot.datt.area, boot.datt.cir, 
                              boot.datt.area_p,  boot.datt.cir_p, 
                              boot.datt.area_m,  boot.datt.cir_m, 
                              boot.datt.area_d,  boot.datt.cir_d)
  
  set_wm.per.all[i]=wm.per_boot[1]
  set_wm.per.p[i]=wm.per_boot[2]
  set_wm.per.m[i]=wm.per_boot[3]
  set_wm.per.d[i]=wm.per_boot[4]
  
  set_stae.per.all[i] = stae.per_boot[1]
  set_stae.per.p[i] = stae.per_boot[2]
  set_stae.per.m[i] = stae.per_boot[3]
  set_stae.per.d[i] = stae.per_boot[4]
  
  set_diff_wm.per.pm[i] = wm.per_boot[2] - wm.per_boot[3]
  set_diff_wm.per.md[i] = wm.per_boot[3] - wm.per_boot[4]
  set_diff_wm.per.pd[i] = wm.per_boot[2] - wm.per_boot[4]
  set_diff_stae.per.pm[i] = set_stae.per.p[i] - set_stae.per.m[i]
  set_diff_stae.per.md[i] = set_stae.per.m[i] - set_stae.per.d[i]
  set_diff_stae.per.pd[i] = set_stae.per.p[i] - set_stae.per.d[i]
}

## CI ####
### overall and every locations ####
ci_wm.per.all=quantile(set_wm.per.all,c(0.05/2,1-0.05/2)) # 147.8760 171.0028 
ci_wm.per.p=quantile(set_wm.per.p,c(0.05/2,1-0.05/2)) # 164.4985 191.4685 
ci_wm.per.m=quantile(set_wm.per.m,c(0.05/2,1-0.05/2)) # 177.8577 209.6533 
ci_wm.per.d=quantile(set_wm.per.d,c(0.05/2,1-0.05/2)) # 121.0496 150.0380 

ci_stae.per.all=quantile(set_stae.per.all,c(0.05/2,1-0.05/2)) # 123.2696 138.3049 
ci_stae.per.p=quantile(set_stae.per.p,c(0.05/2,1-0.05/2)) # 123.3723 147.1682 
ci_stae.per.m=quantile(set_stae.per.m,c(0.05/2,1-0.05/2)) # 137.3493 158.8023 
ci_stae.per.d=quantile(set_stae.per.d,c(0.05/2,1-0.05/2)) # 95.11829 120.94418 

### difference in each pairs ####
#### P vs. M 
ci_wm.per.pm=quantile(set_diff_wm.per.pm,c(0.0167/2,1-0.0167/2)) # -41.62799  10.16246 
ci_stae.per.pm=quantile(set_diff_stae.per.pm,c(0.0167/2,1-0.0167/2)) # -32.273450   6.945564 

#### M vs. D
ci_wm.per.md=quantile(set_diff_wm.per.md,c(0.0167/2,1-0.0167/2)) # 32.03035 84.53292 
ci_stae.per.md=quantile(set_diff_stae.per.md,c(0.0167/2,1-0.0167/2)) # 19.23077 60.69774 

#### P vs. D
ci_wm.per.pd=quantile(set_diff_wm.per.pd,c(0.0167/2,1-0.0167/2)) # 17.91930 66.71862 
ci_stae.per.pd=quantile(set_diff_stae.per.pd,c(0.0167/2,1-0.0167/2)) # 6.08976 48.08719 

#################################################################################################
## Plots for the Bootstrap Distribution ####
### overall and every locations ####

### difference in each pairs ####
#### P vs.M 
# dist.boot_w.m_per_pm=ggplot(data.frame(x=set.diff_w.m_per_pm),aes(x=set.diff_w.m_per_pm))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, Weighted Mean  for Perimeter, P vs. M")+
#   geom_vline(aes(xintercept=ci.boot_w.m_per_pm[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_w.m_per_pm[2]), color="red", linetype="dashed", size=1)
# dist.boot_approx_per_pm=ggplot(data.frame(x=set.diff_approx_per_pm),aes(x=set.diff_approx_per_pm))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, 2nd Order Approximation  for Perimeter, P vs. M")+
#   geom_vline(aes(xintercept=ci.boot_approx_per_pm[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_approx_per_pm[2]), color="red", linetype="dashed", size=1)
# dist.boot_approx_per_pm

#### M vs. D 
# dist.boot_w.m_per_md=ggplot(data.frame(x=set.diff_w.m_per_md),aes(x=set.diff_w.m_per_md))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, Weighted Mean  for Perimeter, M vs. D")+
#   geom_vline(aes(xintercept=ci.boot_w.m_per_md[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_w.m_per_md[2]), color="red", linetype="dashed", size=1)

# dist.boot_approx_per_md=ggplot(data.frame(x=set.diff_approx_per_md),aes(x=set.diff_approx_per_md))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, 2nd Order Approximation  for Perimeter, M vs. D")+
#   geom_vline(aes(xintercept=ci.boot_approx_per_md[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_approx_per_md[2]), color="red", linetype="dashed", size=1)

#### P vs. D 
# dist.boot_w.m_per_pd=ggplot(data.frame(x=set.diff_w.m_per_pd),aes(x=set.diff_w.m_per_pd))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, Weighted Mean  for Perimeter, P vs. D")+
#   geom_vline(aes(xintercept=ci.boot_w.m_per_pd[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_w.m_per_pd[2]), color="red", linetype="dashed", size=1)

# dist.boot_approx_per_pd=ggplot(data.frame(x=set.diff_approx_per_pd),aes(x=set.diff_approx_per_pd))+
#   geom_histogram(colour="black", fill="grey")+
#   ggtitle("Bootstrap for CI, 2nd Order Approximation  for Perimeter, P vs. D")+
#   geom_vline(aes(xintercept=ci.boot_approx_per_pd[1]), color="red", linetype="dashed", size=1)+
#   geom_vline(aes(xintercept=ci.boot_approx_per_pd[2]), color="red", linetype="dashed", size=1)







