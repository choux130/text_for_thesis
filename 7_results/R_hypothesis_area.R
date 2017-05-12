####################
####### Area #######
####################
# SET DATA ####
datt=read.csv("/Users/chou/Google Drive/Spring2016/Plan B/final/clean_dat.csv",header=TRUE)
datt.area=datt$Area
datt.area_p=datt[datt$PMD=="P",]$Area
datt.area_m=datt[datt$PMD=="M",]$Area
datt.area_d=datt[datt$PMD=="D",]$Area

## All weighted means ####
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
wm.area=fun_wm.area(datt.area, datt.area_p, datt.area_m, datt.area_d)
names(wm.area)=c("wm.area.all","wm.area.p","wm.area.m","wm.area.d")
# wm.area.all   wm.area.p   wm.area.m   wm.area.d 
#   1602.977    1928.229    2381.802    1071.746 

## All MLE ####
mle.area.all=mean(datt.area)/2 
mle.area.p=mean(datt.area_p)/2 
mle.area.m=mean(datt.area_m)/2 
mle.area.d=mean(datt.area_d)/2 
mle.area=c(mle.area.all,mle.area.p,mle.area.m,mle.area.d)
names(mle.area)=c("mle.area.all","mle.area.p","mle.area.m","mle.area.d")
# mle.area.all   mle.area.p   mle.area.m   mle.area.d 
#   1183.0417    1221.5875    1478.7500     848.7875 

# PERMUTATION TEST ####
## Setting Arguments ####
set.seed(12345)
reps=10000

set_f_wm.area = numeric(length=reps)
set_t_wm.area.pm = numeric(length=reps)
set_t_wm.area.md = numeric(length=reps)
set_t_wm.area.pd = numeric(length=reps)

set_f_mle.area = numeric(length=reps)
set_t_mle.area.pm = numeric(length=reps)
set_t_mle.area.md = numeric(length=reps)
set_t_mle.area.pd = numeric(length=reps)

## Permutation Test Algorithm ####
for (i in 1:reps){
  ### Setting data
  permu.area=sample(datt$Area, dim(datt)[1], replace=FALSE)
  permu.dat=data.frame(PMD=datt$PMD,Area=permu.area)
  
  permu.dat.area=permu.dat$Area
  permu.dat.area_p=permu.dat[permu.dat$PMD=="P",]$Area
  permu.dat.area_m=permu.dat[permu.dat$PMD=="M",]$Area
  permu.dat.area_d=permu.dat[permu.dat$PMD=="D",]$Area
  
  ### Calculating overall weighted mean and weighted mean by locations  
  wm.area_permu=fun_wm.area(permu.dat.area, permu.dat.area_p, 
                            permu.dat.area_m, permu.dat.area_d)
  
  ### Calculating overall mle and mle by locations
  mle.area_permu=c(mean(permu.dat.area)/2,mean(permu.dat.area_p)/2,
                   mean(permu.dat.area_m)/2,mean(permu.dat.area_d)/2)
  
  ### Save the f and t values
  set_f_wm.area[i] = sum((wm.area_permu[2:4]-wm.area_permu[1])^2)
  set_t_wm.area.pm[i] = wm.area_permu[2]-wm.area_permu[3]
  set_t_wm.area.md[i] = wm.area_permu[3]-wm.area_permu[4]
  set_t_wm.area.pd[i] = wm.area_permu[2]-wm.area_permu[4]
  
  
  set_f_mle.area[i] = sum((mle.area_permu[2:4]-mle.area_permu[1])^2)
  set_t_mle.area.pm[i] = mle.area_permu[2]-mle.area_permu[3]
  set_t_mle.area.md[i] = mle.area_permu[3]-mle.area_permu[4]
  set_t_mle.area.pd[i] = mle.area_permu[2]-mle.area_permu[4]
}

## P-value for the Permutation Tests ####
### overall test ####
f_wm.area = sum((wm.area[2:4] - wm.area[1])^2) # 994563.6
f_mle.area= sum((mle.area[2:4] - mle.area[1])^2) # 200655

p_f_wm.area=length(set_f_wm.area[set_f_wm.area>=f_wm.area])/reps 
p_f_mle.area=length(set_f_mle.area[set_f_mle.area>=f_mle.area])/reps 
p_f.area=c(p_f_wm.area, p_f_mle.area)
names(p_f.area)=c("p_f_wm.area","p_f_mle.area")
# p_f_wm.area p_f_mle.area 
#   0e+00        1e-04 

### pairwise tests ####
#### P vs. M 
t_wm.area.pm = unname(wm.area[2] - wm.area[3]) # -453.5734
t_mle.area.pm = unname(mle.area[2] - mle.area[3]) # -257.1625

p_t_wm.area.pm = 2*length(set_t_wm.area.pm[set_t_wm.area.pm<=t_wm.area.pm])/reps
p_t_mle.area.pm = 2*length(set_t_mle.area.pm[set_t_mle.area.pm<=t_mle.area.pm])/reps
p_t.area.pm = c(p_t_wm.area.pm, p_t_mle.area.pm)
names(p_t.area.pm) = c("p_t_wm.area.pm","p_t_mle.area.pm")
# p_t_wm.area.pm p_t_mle.area.pm 
#    0.0976          0.0950 

#### M vs. D
t_wm.area.md = unname(wm.area[3] - wm.area[4]) # 1310.056
t_mle.area.md = unname(mle.area[3] - mle.area[4]) # 629.9625

p_t_wm.area.md = 2*length(set_t_wm.area.md[set_t_wm.area.md>=t_wm.area.md])/reps
p_t_mle.area.md = 2*length(set_t_mle.area.md[set_t_mle.area.md>=t_mle.area.md])/reps
p_t.area.md = c(p_t_wm.area.md, p_t_mle.area.md)
names(p_t.area.md) = c("p_t_wm.area.md","p_t_mle.area.md")
# p_t_wm.area.md p_t_mle.area.md 
#    0e+00           2e-04 

#### P vs. D
t_wm.area.pd = unname(wm.area[2] - wm.area[4]) # 856.4828
t_mle.area.pd = unname(mle.area[2] - mle.area[4]) # 372.8

p_t_wm.area.pd = 2*length(set_t_wm.area.pd[set_t_wm.area.pd>=t_wm.area.pd])/reps
p_t_mle.area.pd = 2*length(set_t_mle.area.pd[set_t_mle.area.pd>=t_mle.area.pd])/reps 
p_t.area.pd = c(p_t_wm.area.pd, p_t_mle.area.pd)
names(p_t.area.pd) = c("p_t_wm.area.pd","p_t_mle.area.pd")
# p_t_wm.area.pd p_t_mle.area.pd 
#    0.0022          0.0140 

#######################################################################################

## Plots the Permutation Distribution ####
### overall test ####
#library(ggplot2)
#dist.f_mle_area=ggplot(data.frame(x=set.f_mle_area),aes(x=set.f_mle_area))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, MLE for Area")+
#  geom_vline(aes(xintercept=f_mle_area), color="red", linetype="dashed", size=1)
  
#dist.f_wm_area=ggplot(data.frame(x=set.f_wm_area),aes(x=set.f_wm_area))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, Weighted Mean for Area")+
#  geom_vline(aes(xintercept=f_wm_area), color="red", linetype="dashed", size=1)
### pairwise tests ####
#### P vs. M 
#dist.t_wm_area_pm=ggplot(data.frame(x=set.t_wm_area_pm),aes(x=set.t_wm_area_pm))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, Weighted Mean  for Area, P vs. M")+
#  geom_vline(aes(xintercept=t_wm_area_pm), color="red", linetype="dashed", size=1)
#dist.t_mle_area_pm=ggplot(data.frame(x=set.t_mle_area_pm),aes(x=set.t_mle_area_pm))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, MLE for Area, P vs. M")+
#  geom_vline(aes(xintercept=t_mle_area_pm), color="red", linetype="dashed", size=1)

#### M vs. D 
#dist.t_wm_area_md=ggplot(data.frame(x=set.t_wm_area_md),aes(x=set.t_wm_area_md))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, Weighted Mean  for Area, M vs. D")+
#  geom_vline(aes(xintercept=t_wm_area_md), color="red", linetype="dashed", size=1)
#dist.t_mle_area_md=ggplot(data.frame(x=set.t_mle_area_md),aes(x=set.t_mle_area_md))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, MLE for Area, M vs. D")+
#  geom_vline(aes(xintercept=t_mle_area_md), color="red", linetype="dashed", size=1)

#### P vs. D
#dist.t_wm_area_pd=ggplot(data.frame(x=set.t_wm_area_pd),aes(x=set.t_wm_area_pd))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, Weighted Mean  for Area, M vs. D")+
# geom_vline(aes(xintercept=t_wm_area_pd), color="red", linetype="dashed", size=1)
#dist.t_mle_area_pd=ggplot(data.frame(x=set.t_mle_area_pd),aes(x=set.t_mle_area_pd))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, MLE for Area, M vs. D")+
#  geom_vline(aes(xintercept=t_mle_area_pd), color="red", linetype="dashed", size=1)


