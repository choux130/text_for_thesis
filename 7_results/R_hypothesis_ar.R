############################
####### Asepct Ratio #######
############################

# SET DATA ####
datt=read.csv("/Users/chou/Google Drive/Spring2016/Plan B/final/clean_dat.csv",header=TRUE)
datt.ar=datt$Aspect.Ratio
datt.ar_p=datt[datt$PMD=="P",]$Aspect.Ratio
datt.ar_m=datt[datt$PMD=="M",]$Aspect.Ratio
datt.ar_d=datt[datt$PMD=="D",]$Aspect.Ratio

## All Arithmetic Means ####
m.ar=c(mean(datt.ar),mean(datt.ar_p),
        mean(datt.ar_m),mean(datt.ar_d))
names(m.ar)=c("m.ar.all","m.ar.p","m.ar.m","m.ar.d")
# wm.ar.all   wm.ar.p   wm.ar.m   wm.ar.d 
#  1.474672  1.529380  1.369450  1.525185 


# PERMUTATION TEST ####
## Setting Arguments ####
set.seed(12345)
reps=10000

set_f_m.ar = numeric(length=reps)
set_t_m.ar.pm = numeric(length=reps)
set_t_m.ar.md = numeric(length=reps)
set_t_m.ar.pd = numeric(length=reps)

for (i in 1:reps){
  ### Setting data
  permu.ar=sample(datt$Aspect.Ratio, dim(datt)[1], replace=FALSE)
  permu.dat=data.frame(PMD=datt$PMD,Aspect.Ratio=permu.ar)
  
  permu.dat.ar=permu.dat$Aspect.Ratio
  permu.dat.ar_p=permu.dat[permu.dat$PMD=="P",]$Aspect.Ratio
  permu.dat.ar_m=permu.dat[permu.dat$PMD=="M",]$Aspect.Ratio
  permu.dat.ar_d=permu.dat[permu.dat$PMD=="D",]$Aspect.Ratio
  
  ### Calculating overall mean and mean by locations  
  m.ar_permu = c(mean(permu.dat.ar),mean(permu.dat.ar_p),
                  mean(permu.dat.ar_m),mean(permu.dat.ar_d))
  
  ### Save the f and t values
  set_f_m.ar[i] = sum((m.ar_permu[2:4]-m.ar_permu[1])^2)
  set_t_m.ar.pm[i] = m.ar_permu[2]-m.ar_permu[3]
  set_t_m.ar.md[i] = m.ar_permu[3]-m.ar_permu[4]
  set_t_m.ar.pd[i] = m.ar_permu[2]-m.ar_permu[4]
}

## P-value for the Permutation Tests ####
### overall test ####
f_m.ar = sum((m.ar[2:4] - m.ar[1])^2) # 0.0166162
p_f_m.area = length(set_f_m.ar[set_f_m.ar>=f_m.ar])/reps # 0.1838

### pairwise tests ####
#### P vs. M 
t_m.ar.pm = unname(m.ar[2] - m.ar[3]) # 0.15993
p_t_m.ar.pm = 2*length(set_t_m.ar.pm[set_t_m.ar.pm<=t_m.ar.pm])/reps # 0.2476

#### M vs. D
t_m.ar.md = unname(m.ar[3] - m.ar[4]) # -0.155735
p_t_m.ar.md = 2*length(set_t_m.ar.md[set_t_m.ar.md>=t_m.ar.md])/reps # 0.0022

#### P vs. D
t_m.ar.pd = unname(m.ar[2] - m.ar[4]) # 0.004195
p_t_m.ar.pd = 2*length(set_t_m.ar.pd[set_t_m.ar.pd>=t_m.ar.pd])/reps # 0.0616

#######################################################################################
## Plots the Permutation Distribution ####
### overall test ####
#library(ggplot2)
#dist.f_m_ar=ggplot(data.frame(x=set.f_m_ar),aes(x=set.f_m_ar))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, Sample Mean  for Aspect Ratio")+
#  geom_vline(aes(xintercept=f_ar), color="red", linetype="dashed", size=1)

### pairwise tests ####
#### P vs. M 
#dist.t_m_ar_pm=ggplot(data.frame(x=set.t_m_ar_pm),aes(x=set.t_m_ar_pm))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, Sample Mean  for Aspect Ratio, P vs. M")+
#  geom_vline(aes(xintercept=t_ar_pm), color="red", linetype="dashed", size=1)

#### M vs. D
#dist.t_m_ar_md=ggplot(data.frame(x=set.t_m_ar_md),aes(x=set.t_m_ar_md))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, Sample Mean  for Aspect Ratio, M vs. D")+
#  geom_vline(aes(xintercept=t_ar_md), color="red", linetype="dashed", size=1)

#### M vs. D
#dist.t_m_ar_pd=ggplot(data.frame(x=set.t_m_ar_pd),aes(x=set.t_m_ar_pd))+
#  geom_histogram(colour="black", fill="grey")+
#  ggtitle("Overall Permutation Test, Sample Mean  for Aspect Ratio, P vs. D")+
#  geom_vline(aes(xintercept=t_ar_pd), color="red", linetype="dashed", size=1)

