#########################
####### Perimeter #######
#########################
# SETTING DATA ####
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

## All weighted mean ####
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
wm.per=fun_wm.per(datt.area, datt.per, datt.area_p, datt.per_p,
                  datt.area_m, datt.per_m, 
                  datt.area_d, datt.per_d)
names(wm.per)=c("wm.per.all","wm.per.p","wm.per.m","wm.per.d")
# wm.per.all   wm.per.p   wm.per.m   wm.per.d 
#   158.7518   176.5613   192.8953   133.4894 

## All 2nd order Taylor's Approximation Estimator####
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
stae.per=fun2_stae.per(datt.area, datt.cir,
                      datt.area_p, datt.cir_p,
                      datt.area_m, datt.cir_m,
                      datt.area_d, datt.cir_d)
names(stae.per)=c("stae.per.all","stae.per.p","stae.per.m","stae.per.d")
# stae.per.all   stae.per.p   stae.per.m   stae.per.d 
#   130.5544     134.4383     147.9494     107.2606 

# PERMUTATION TEST ####
## Setting Arguments ####
reps=10000
set.seed(12345)

set_f_wm.per = numeric(length=reps)
set_t_wm.per.pm = numeric(length=reps)
set_t_wm.per.md = numeric(length=reps)
set_t_wm.per.pd = numeric(length=reps)

set_f_stae.per = numeric(length=reps)
set_t_stae.per.pm = numeric(length=reps)
set_t_stae.per.md = numeric(length=reps)
set_t_stae.per.pd = numeric(length=reps)

## Permutation Test Algorithm ####
for (i in 1:reps){
  ### Setting data
  permu.dat=datt[sample(1:dim(datt)[1], dim(datt)[1], replace=FALSE),11:13]
  permu.dat=cbind(permu.dat, "PMD"=rep(c("P","M","D"),each=40))
  
  permu.dat.area=permu.dat$Area
  permu.dat.area_p=permu.dat[permu.dat$PMD=="P",]$Area
  permu.dat.area_m=permu.dat[permu.dat$PMD=="M",]$Area
  permu.dat.area_d=permu.dat[permu.dat$PMD=="D",]$Area
  permu.dat.per=permu.dat$Perimeter
  permu.dat.per_p=permu.dat[permu.dat$PMD=="P",]$Perimeter
  permu.dat.per_m=permu.dat[permu.dat$PMD=="M",]$Perimeter
  permu.dat.per_d=permu.dat[permu.dat$PMD=="D",]$Perimeter
  permu.dat.cir=permu.dat$Circularity
  permu.dat.cir_p=permu.dat[permu.dat$PMD=="P",]$Circularity
  permu.dat.cir_m=permu.dat[permu.dat$PMD=="M",]$Circularity
  permu.dat.cir_d=permu.dat[permu.dat$PMD=="D",]$Circularity
  
  ### Calculating overall weighted mean and weighted mean by locations 
  wm.per_permu = fun_wm.per(permu.dat.area, permu.dat.per, 
                          permu.dat.area_p, permu.dat.per_p,
                          permu.dat.area_m, permu.dat.per_m, 
                          permu.dat.area_d, permu.dat.per_d)
  
  ## Calculating overall 2TAE and 2TAE by locations
  stae.per_permu = fun2_stae.per(permu.dat.area, permu.dat.cir,
                         permu.dat.area_p, permu.dat.cir_p,
                         permu.dat.area_m, permu.dat.cir_m,
                         permu.dat.area_d, permu.dat.cir_d)
  
  set_f_wm.per[i] = sum((wm.per_permu[2:4] - wm.per_permu[1])^2)
  set_t_wm.per.pm[i] = wm.per_permu[2] - wm.per_permu[3]
  set_t_wm.per.md[i] = wm.per_permu[3] - wm.per_permu[4]
  set_t_wm.per.pd[i] = wm.per_permu[2] - wm.per_permu[4]
  
  set_f_stae.per[i] = sum((stae.per_permu[2:4] - stae.per_permu[1])^2)
  set_t_stae.per.pm[i] = stae.per_permu[2] - stae.per_permu[3]
  set_t_stae.per.md[i] = stae.per_permu[3] - stae.per_permu[4]
  set_t_stae.per.pd[i] = stae.per_permu[2] - stae.per_permu[4]
}

## P-value for the Permutation Tests ####
### overall test ####
f_wm.per = sum((wm.per[2:4] - wm.per[1])^2) # 2121.144
f_stae.per = sum((stae.per[2:4] - stae.per[1])^2) # 860.2689

p_f_wm.per=length(set_f_wm.per[set_f_wm.per>=f_wm.per])/reps 
p_f_stae.per=length(set_f_stae.per[set_f_stae.per>=f_stae.per])/reps 
p_f.per=c(p_f_wm.per, p_f_stae.per)
names(p_f.per)=c("p_f_wm.per","p_f_stae.per")
# p_f_wm.per p_f_stae.per 
#   1e-04        0e+00 
### pairwise tests ####
#### P vs. M 
t_wm.per.pm = unname(wm.per[2] - wm.per[3]) # -16.33404
t_stae.per.pm = unname(stae.per[2] - stae.per[3]) # -13.51103

p_t_wm.per.pm = 2*length(set_t_wm.per.pm[set_t_wm.per.pm<=t_wm.per.pm])/reps
p_t_stae.per.pm = 2*length(set_t_stae.per.pm[set_t_stae.per.pm<=t_stae.per.pm])/reps
p_t.per.pm = c(p_t_wm.per.pm, p_t_stae.per.pm)
names(p_t.per.pm) = c("p_t_wm.per.pm","p_t_stae.per.pm")
# p_t_wm.per.pm p_t_stae.per.pm 
#   0.2744          0.1518 

#### M vs. D
t_wm.per.md = unname(wm.per[3] - wm.per[4]) # 59.40592
t_stae.per.md = unname(stae.per[3] - stae.per[4]) # 40.68873

p_t_wm.per.md = 2*length(set_t_wm.per.md[set_t_wm.per.md>=t_wm.per.md])/reps
p_t_stae.per.md = 2*length(set_t_stae.per.md[set_t_stae.per.md>=t_stae.per.md])/reps
p_t.per.md = c(p_t_wm.per.md, p_t_stae.per.md)
names(p_t.per.md) = c("p_t_wm.per.md","p_t_stae.per.md")
# p_t_wm.per.md p_t_stae.per.md 
#       0               0 

#### P vs. D
t_wm.per.pd = unname(wm.per[2] - wm.per[4]) # 43.07188
t_stae.per.pd = unname(stae.per[2] - stae.per[4]) # 27.17769

p_t_wm.per.pd = 2*length(set_t_wm.per.pd[set_t_wm.per.pd>=t_wm.per.pd])/reps
p_t_stae.per.pd = 2*length(set_t_stae.per.pd[set_t_stae.per.pd>=t_stae.per.pd])/reps
p_t.per.pd = c(p_t_wm.per.pd, p_t_stae.per.pd)
names(p_t.per.pd) = c("p_t_wm.per.pd","p_t_stae.per.pd")
# p_t_wm.per.pd p_t_stae.per.pd 
#    0.0018          0.0024 

#####################################################################

## Plots the Permutation Distribution ####
### overall test ####
#library(ggplot2)
#dist.f_w.m_per=ggplot(data.frame(x=set.f_w.m_per),aes(x=set.f_w.m_per))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, Weighted Mean  for Perimeter")+
#  geom_vline(aes(xintercept=f_w.m_per), color="red", linetype="dashed", size=1)

#dist.f_approx_per=ggplot(data.frame(x=set.f_approx_per),aes(x=set.f_approx_per))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, Weighted Mean  for Perimeter")+
#  geom_vline(aes(xintercept=f_approx_per), color="red", linetype="dashed", size=1)

### pairwise Test ####
#### P vs. M 
#dist.t_w.m_per_pm=ggplot(data.frame(x=set.t_w.m_per_pm),aes(x=set.t_w.m_per_pm))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, Weighted Mean  for Perimeter, P vs. M")+
#  geom_vline(aes(xintercept=t_w.m_per_pm), color="red", linetype="dashed", size=1)

#dist.t_approx_per_pm=ggplot(data.frame(x=set.t_approx_per_pm),aes(x=set.t_approx_per_pm))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, 2nd Order Approximation for Perimeter, P vs. M")+
#  geom_vline(aes(xintercept=t_approx_per_pm), color="red", linetype="dashed", size=1)

#### M vs. D
#dist.t_w.m_per_md=ggplot(data.frame(x=set.t_w.m_per_md),aes(x=set.t_w.m_per_md))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, Weighted Mean  for Perimeter, M vs. D")+
#  geom_vline(aes(xintercept=t_w.m_per_md), color="red", linetype="dashed", size=1)

#dist.t_approx_per_md=ggplot(data.frame(x=set.t_approx_per_md),aes(x=set.t_approx_per_md))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, 2nd Order Approximation for Perimeter, M vs. D")+
#  geom_vline(aes(xintercept=t_approx_per_md), color="red", linetype="dashed", size=1)

#### P vs. D 
#dist.t_w.m_per_pd=ggplot(data.frame(x=set.t_w.m_per_pd),aes(x=set.t_w.m_per_pd))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, Weighted Mean  for Perimeter, P vs. D")+
#  geom_vline(aes(xintercept=t_w.m_per_pd), color="red", linetype="dashed", size=1)

#dist.t_approx_per_pd=ggplot(data.frame(x=set.f_approx_per_pd),aes(x=set.t_approx_per_pd))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Permutation Test, 2nd Order Approximation for Perimeter, P vs. D")+
#  geom_vline(aes(xintercept=t_approx_per_pd), color="red", linetype="dashed", size=1)

