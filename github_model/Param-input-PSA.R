#######################
###   model input   ###
#######################

#####################
###   PSA input   ###
#####################

set.seed(12345678)
nsim=10000
#TP
PSAp_s2d=rbeta(nsim,275,9725)
PSAp_m2d=rbeta(nsim,970,9030)
PSAp_hf2d=rbeta(nsim,1380,8620)
PSAp_ps2d=rbeta(nsim,713,9287)
PSAp_pm2d=rbeta(nsim,610,9390)
PSAp_phf2d=rbeta(nsim,2480,7520)

#IC
PSAc_uc_p=rgamma(nsim,shape=96.04,scale=0.209096016)
PSAc_uc_h=rgamma(nsim,shape=96.04,scale=0.548877043)
PSAc_ls_p=rgamma(nsim,shape=96.04,scale=1.484859423)
PSAc_ls_h=rgamma(nsim,shape=96.04,scale=1.844243201)
PSAc_sex_p=rgamma(nsim,shape=96.04,scale=2.087856396)
PSAc_sex_h=rgamma(nsim,shape=96.04,scale=2.447240174)
# c_sex_s=3923.44 		
PSAc_rex_p=rgamma(nsim,shape=96.04,scale=4.212054115)
PSAc_rex_h=rgamma(nsim,shape=96.04,scale=4.571437894)
# c_rex_s=16819.53 		
PSAc_di_p=rgamma(nsim,shape=96.04,scale=5.49677489)
PSAc_di_h=rgamma(nsim,shape=96.04,scale=5.856158668)
# c_di_s=14119.86 		

#HC
PSAc_s=rgamma(nsim,shape=96.04,scale=46.65275176)
PSAc_m=rgamma(nsim,shape=96.04,scale=47.1923175)
PSAc_hf=rgamma(nsim,shape=96.04,scale=13.53766022)
PSAc_ps=rgamma(nsim,shape=96.04,scale=4.665275176)
PSAc_pm=rgamma(nsim,shape=96.04,scale=4.71923175)
PSAc_phf=rgamma(nsim,shape=96.04,scale=1.353766022)

#Utility
# u_ph=1	
PSAu_h=rbeta(nsim,29.66645813,1.236102422)
PSAu_s=rbeta(nsim,172.322,140.9907273)
PSAu_m=rbeta(nsim,153.064,102.0426667)
PSAu_hf=rbeta(nsim,141.5092,83.10857778)
PSAu_ps=rbeta(nsim,133.806,72.04938462)
PSAu_pm=rbeta(nsim,114.548,49.092)
PSAu_phf=rbeta(nsim,102.9932,38.09337534)

#New supplement
PSAc_drug=rgamma(nsim,shape=96.01737707,scale=0.884110799)
PSAc_eof=rgamma(nsim, shape = 96.04,scale = 22.68846314)
PSAdisu=rbeta(nsim,14.54808,276.41352)

##TP for intervention
#baseline characteristics
age=46
sbp=126.63
dbp=81.8
source("risk-prediction-function_pre.R")

#usual care
mean_us_sbp=-0.47
mean_us_dbp=-0.25
sd_us_sbp=(1.22+0.28)/(2*1.96)
sd_us_dbp=(0.91+0.41)/(2*1.96)
delta_us_sbp=rnorm(nsim,mean = mean_us_sbp, sd_us_sbp)
delta_us_dbp=rnorm(nsim,mean = mean_us_dbp, sd_us_dbp)
sbp_us=sbp+delta_us_sbp
dbp_us=dbp+delta_us_dbp

tp_uc=list()
for (j in 1:nsim) {
  temp_sbp=sbp_us[j]
  temp_dbp=dbp_us[j]
  tp_pre=as.data.frame(matrix(nrow = 55,ncol = 4))
  for (i in 1:55) {
    temp_age=age+i-1
    temp=pre_predict(temp_age,temp_sbp,temp_dbp)
    tp_pre[i,]=temp
    colnames(tp_pre)=c("p_ph2h","p_ph2hs","p_ph2hhf","p_ph2hm")
  }
  tp_uc[[j]]=tp_pre
}

#lifestyle
mean_ls_sbp=-3.5
mean_ls_dbp=-2.86
sd_ls_sbp=(5.64-1.41)/(2*1.96)
sd_ls_dbp=(4.43-1.34)/(2*1.96)
delta_ls_sbp=rnorm(nsim,mean = mean_ls_sbp, sd_ls_sbp)
delta_ls_dbp=rnorm(nsim,mean = mean_ls_dbp, sd_ls_dbp)
sbp_ls=sbp_us+delta_ls_sbp
dbp_ls=dbp_us+delta_ls_dbp

tp_ls=list()
for (j in 1:nsim) {
  temp_sbp=sbp_ls[j]
  temp_dbp=dbp_ls[j]
  tp_pre=as.data.frame(matrix(nrow = 55,ncol = 4))
  for (i in 1:55) {
    temp_age=age+i-1
    temp=pre_predict(temp_age,temp_sbp,temp_dbp)
    tp_pre[i,]=temp
    colnames(tp_pre)=c("p_ph2h","p_ph2hs","p_ph2hhf","p_ph2hm")
  }
  tp_ls[[j]]=tp_pre
}

#Strengthen exercise
mean_Sex_sbp=-6.03
mean_Sex_dbp=-3.48
sd_Sex_sbp=(8.16-3.87)/(2*1.96)
sd_Sex_dbp=(5.09-1.82)/(2*1.96)
delta_Sex_sbp=rnorm(nsim,mean = mean_Sex_sbp, sd_Sex_sbp)
delta_Sex_dbp=rnorm(nsim,mean = mean_Sex_dbp, sd_Sex_dbp)
sbp_Sex=sbp_us+delta_Sex_sbp
dbp_Sex=dbp_us+delta_Sex_dbp

tp_Sex=list()
for (j in 1:nsim) {
  temp_sbp=sbp_Sex[j]
  temp_dbp=dbp_Sex[j]
  tp_pre=as.data.frame(matrix(nrow = 55,ncol = 4))
  for (i in 1:55) {
    temp_age=age+i-1
    temp=pre_predict(temp_age,temp_sbp,temp_dbp)
    tp_pre[i,]=temp
    colnames(tp_pre)=c("p_ph2h","p_ph2hs","p_ph2hhf","p_ph2hm")
  }
  tp_Sex[[j]]=tp_pre
}

#Relaxation
mean_Rex_sbp=-4.97
mean_Rex_dbp=-4.99
sd_Rex_sbp=(7.66-2.15)/(2*1.96)
sd_Rex_dbp=(7.03-2.96)/(2*1.96)
delta_Rex_sbp=rnorm(nsim,mean = mean_Rex_sbp, sd_Rex_sbp)
delta_Rex_dbp=rnorm(nsim,mean = mean_Rex_dbp, sd_Rex_dbp)
sbp_Rex=sbp_us+delta_Rex_sbp
dbp_Rex=dbp_us+delta_Rex_dbp

tp_Rex=list()
for (j in 1:nsim) {
  temp_sbp=sbp_Rex[j]
  temp_dbp=dbp_Rex[j]
  tp_pre=as.data.frame(matrix(nrow = 55,ncol = 4))
  for (i in 1:55) {
    temp_age=age+i-1
    temp=pre_predict(temp_age,temp_sbp,temp_dbp)
    tp_pre[i,]=temp
    colnames(tp_pre)=c("p_ph2h","p_ph2hs","p_ph2hhf","p_ph2hm")
  }
  tp_Rex[[j]]=tp_pre
}

#Dietary
mean_di_sbp=-2.54
mean_di_dbp=-1.73
sd_di_sbp=(4.73-0.49)/(2*1.96)
sd_di_dbp=(3.34-0.23)/(2*1.96)
delta_di_sbp=rnorm(nsim,mean = mean_di_sbp, sd_di_sbp)
delta_di_dbp=rnorm(nsim,mean = mean_di_dbp, sd_di_dbp)
sbp_di=sbp_us+delta_di_sbp
dbp_di=dbp_us+delta_di_dbp

tp_di=list()
for (j in 1:nsim) {
  temp_sbp=sbp_di[j]
  temp_dbp=dbp_di[j]
  tp_pre=as.data.frame(matrix(nrow = 55,ncol = 4))
  for (i in 1:55) {
    temp_age=age+i-1
    temp=pre_predict(temp_age,temp_sbp,temp_dbp)
    tp_pre[i,]=temp
    colnames(tp_pre)=c("p_ph2h","p_ph2hs","p_ph2hhf","p_ph2hm")
  }
  tp_di[[j]]=tp_pre
}
