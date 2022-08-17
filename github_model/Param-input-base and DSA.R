#######################
###   model input   ###
#######################

##Deterministic parameters
c_sex_s=3923.44 
c_rex_s=16819.53 
c_di_s=14119.86 
u_ph=1
##Natural mortality input
China_nm<-data.frame(Index=c(46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100),
                     p_d=c(0.0026111975 ,0.0026111975 ,0.0026111975 ,0.0026111975 ,0.0041818225 ,0.0041818225 ,0.0041818225 ,0.0041818225 ,0.0041818225 ,0.0061857011 ,0.0061857011 ,0.0061857011 ,0.0061857011 ,
                           0.0061857011 ,0.0103071354 ,0.0103071354 ,0.0103071354 ,0.0103071354 ,0.0103071354 ,0.0172064422 ,0.0172064422 ,0.0172064422 ,0.0172064422 ,0.0172064422 ,0.0306391094 ,0.0306391094 ,
                           0.0306391094 ,0.0306391094 ,0.0306391094 ,0.0495234847 ,0.0495234847 ,0.0495234847 ,0.0495234847 ,0.0495234847 ,0.0848091844 ,0.0848091844 ,0.0848091844 ,0.0848091844 ,0.0848091844 ,
                           0.1274253931 ,0.1274253931 ,0.1274253931 ,0.1274253931 ,0.1274253931 ,0.1907820749 ,0.1907820749 ,0.1907820749 ,0.1907820749 ,0.1907820749 ,0.2171035621 ,0.2171035621 ,0.2171035621 ,
                           0.2171035621 ,0.2171035621 ,0.4543450101))
##TP for hypertension 
source("risk-prediction-function_hyper.R")
age=46
tp_hypertension=as.data.frame(matrix(nrow = 55,ncol = 3))
for (i in 1:55) {
  temp_age=age+i-1
  temp=hyp_predict(temp_age,temp_sbp,temp_dbp)
  tp_hypertension[i,]=temp
  colnames(tp_hypertension)=c("p_h2s","p_h2hf","p_h2m")
}


#####################
###   DSA input   ###
#####################

DSA=1
source("DSA_input.R")
##TP for intervention
#baseline characteristics
age=46
sbp=126.63
dbp=81.8
source("risk-prediction-function_pre.R")
#usual care
mean_us_sbp=-0.47
mean_us_dbp=-0.25
sbp_us=sbp+mean_us_sbp
dbp_us=dbp+mean_us_dbp

tp_pre=as.data.frame(matrix(nrow = 55,ncol = 4))
for (i in 1:55) {
  temp_age=age+i-1
  temp=pre_predict(temp_age,sbp_us,dbp_us)
  tp_pre[i,]=temp
  colnames(tp_pre)=c("p_ph2h","p_ph2hs","p_ph2hhf","p_ph2hm")
}
tp_pre_uc=tp_pre

#lifestyle
mean_ls_sbp=-3.5
mean_ls_dbp=-2.86
sbp_ls=sbp_us+mean_ls_sbp
dbp_ls=dbp_us+mean_ls_dbp

tp_pre=as.data.frame(matrix(nrow = 55,ncol = 4))
for (i in 1:55) {
  temp_age=age+i-1
  temp=pre_predict(temp_age,sbp_ls,dbp_ls)
  tp_pre[i,]=temp
  colnames(tp_pre)=c("p_ph2h","p_ph2hs","p_ph2hhf","p_ph2hm")
}
tp_pre_ls=tp_pre

#Strengthen exercise
mean_Sex_sbp=-6.03
mean_Sex_dbp=-3.48
sbp_Sex=sbp_us+mean_Sex_sbp
dbp_Sex=dbp_us+mean_Sex_dbp


tp_pre=as.data.frame(matrix(nrow = 55,ncol = 4))
for (i in 1:55) {
  temp_age=age+i-1
  temp=pre_predict(temp_age,sbp_Sex,dbp_Sex)
  tp_pre[i,]=temp
  colnames(tp_pre)=c("p_ph2h","p_ph2hs","p_ph2hhf","p_ph2hm")
}
tp_pre_Sex=tp_pre

#Relaxation
mean_Rex_sbp=-4.97
mean_Rex_dbp=-4.99
sbp_Rex=sbp_us+mean_Rex_sbp
dbp_Rex=dbp_us+mean_Rex_dbp

tp_pre=as.data.frame(matrix(nrow = 55,ncol = 4))
for (i in 1:55) {
  temp_age=age+i-1
  temp=pre_predict(temp_age,sbp_Rex,dbp_Rex)
  tp_pre[i,]=temp
  colnames(tp_pre)=c("p_ph2h","p_ph2hs","p_ph2hhf","p_ph2hm")
}
tp_pre_Rex=tp_pre


#Dietary
mean_di_sbp=-2.54
mean_di_dbp=-1.73
sbp_di=sbp_us+mean_di_sbp
dbp_di=dbp_us+mean_di_dbp

tp_pre=as.data.frame(matrix(nrow = 55,ncol = 4))
for (i in 1:55) {
  temp_age=age+i-1
  temp=pre_predict(temp_age,sbp_di,dbp_di)
  tp_pre[i,]=temp
  colnames(tp_pre)=c("p_ph2h","p_ph2hs","p_ph2hhf","p_ph2hm")
}
tp_pre_di=tp_pre

