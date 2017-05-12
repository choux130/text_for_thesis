###########################
####### Circularity #######
###########################

# SET DATA ####
datt=read.csv("/Users/chou/Google Drive/Spring2016/Plan B/final/clean_dat.csv",header=TRUE)
datt.cir=datt$Circularity
datt.cir_p=datt[datt$PMD=="P",]$Circularity
datt.cir_m=datt[datt$PMD=="M",]$Circularity
datt.cir_d=datt[datt$PMD=="D",]$Circularity

## All Arithmetic Means ####
m.cir=c(mean(datt.cir),mean(datt.cir_p),
        mean(datt.cir_m),mean(datt.cir_d))
names(m.cir)=c("wm.cir.all","wm.cir.p","wm.cir.m","wm.cir.d")
# wm.cir.all   wm.cir.p   wm.cir.m   wm.cir.d 
# 0.7389333  0.7442475  0.7706850  0.7018675 

# PERMUTATION TEST ####
## Setting Arguments ####
set.seed(12345)
reps=10000

set_f_m.cir = numeric(length=reps)
set_t_m.cir.pm = numeric(length=reps)
set_t_m.cir.md = numeric(length=reps)
set_t_m.cir.pd = numeric(length=reps)

for (i in 1:reps){
  ### Setting data
  permu.cir=sample(datt$Circularity, dim(datt)[1], replace=FALSE)
  permu.dat=data.frame(PMD=datt$PMD,Circularity=permu.cir)
  
  permu.dat.cir=permu.dat$Circularity
  permu.dat.cir_p=permu.dat[permu.dat$PMD=="P",]$Circularity
  permu.dat.cir_m=permu.dat[permu.dat$PMD=="M",]$Circularity
  permu.dat.cir_d=permu.dat[permu.dat$PMD=="D",]$Circularity
  
  ### Calculating overall mean and mean by locations  
  m.cir_permu = c(mean(permu.dat.cir),mean(permu.dat.cir_p),
                  mean(permu.dat.cir_m),mean(permu.dat.cir_d))
  
  ### Save the f and t values
  set_f_m.cir[i] = sum((m.cir_permu[2:4]-m.cir_permu[1])^2)
  set_t_m.cir.pm[i] = m.cir_permu[2]-m.cir_permu[3]
  set_t_m.cir.md[i] = m.cir_permu[3]-m.cir_permu[4]
  set_t_m.cir.pd[i] = m.cir_permu[2]-m.cir_permu[4]
}

## P-value for the Permutation Tests ####
### overall test ####
f_m.cir = sum((m.cir[2:4] - m.cir[1])^2) # 994563.6
p_f_m.cir=length(set_f_m.cir[set_f_m.cir>=f_m.cir])/reps # 0.007

### pairwise tests ####
#### P vs. M 
t_m.cir.pm = unname(m.cir[2] - m.cir[3]) # -0.0264375
p_t_m.cir.pm = 2*length(set_t_m.cir.pm[set_t_m.cir.pm<=t_m.cir.pm])/reps # 0.2476

#### M vs. D
t_m.cir.md = unname(m.cir[3] - m.cir[4]) # 0.0688175
p_t_m.cir.md = 2*length(set_t_m.cir.md[set_t_m.cir.md>=t_m.cir.md])/reps # 0.0022

#### P vs. D
t_m.cir.pd = unname(m.cir[2] - m.cir[4]) # 0.04238
p_t_m.cir.pd = 2*length(set_t_m.cir.pd[set_t_m.cir.pd>=t_m.cir.pd])/reps # 0.0616

#######################################################################################
## Plots the Permutation Distribution ####
### overall test ####

#library(ggplot2)
#dist.f_m_cir=ggplot(data.frame(x=set.f_m_cir),aes(x=set.f_m_cir))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, Sample Mean  for Circularity")+
#  geom_vline(aes(xintercept=f_cir), color="red", linetype="dashed", size=1)
#dist.f_m_cir
#(p.f_m_cir=length(set.f_m_cir[set.f_m_cir>=f_cir])/reps) #0.007

### pairwise tests ####
#### P vs. M 
#dist.t_m_cir_pm=ggplot(data.frame(x=set.t_m_cir_pm),aes(x=set.t_m_cir_pm))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, Sample Mean  for Circularity, P vs. M")+
#  geom_vline(aes(xintercept=t_cir_pm), color="red", linetype="dashed", size=1)

#### M vs. D
#dist.t_m_cir_md=ggplot(data.frame(x=set.t_m_cir_md),aes(x=set.t_m_cir_md))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, Sample Mean  for Circularity, M vs. D")+
#  geom_vline(aes(xintercept=t_cir_md), color="red", linetype="dashed", size=1)

#### P vs. D
#dist.t_m_cir_pd=ggplot(data.frame(x=set.t_m_cir_pd),aes(x=set.t_m_cir_pd))+
#  geom_histogram(colour="black", fill="grey")+
# ggtitle("Overall Permutation Test, Sample Mean  for Circularity, P vs. D")+
#  geom_vline(aes(xintercept=t_cir_pd), color="red", linetype="dashed", size=1)

