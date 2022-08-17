
cea_model_uc<-function(tp_pre_uc){
  n_input=10000
  #UC
  res_uc=as.data.frame(matrix(ncol = 16,nrow = 56))
  # res_uc=as.numeric(res_uc)
  # res_uc=format(res_uc,scientific=FALSE)
  colnames(res_uc)=c("Age","prehypertension","hypertension","stroke","MI","HF","post_stroke","post_MI","post_HF","death","Check","Cost","Life_years","Qaly","n_ph2h","t_ph2h")
  n=n_input
  res_uc$Age[1]=45
  res_uc$prehypertension[1]=n
  res_uc[1,3:10]=0
  res_uc$Check[1]=sum(res_uc[1,2:10],na.rm = TRUE)
  res_uc$Cost[1]=res_uc$prehypertension[1]*c_uc_p
  res_uc$Life_years[1]=sum(res_uc[1,2:9])
  res_uc$Qaly[1]=res_uc$prehypertension[1]*u_ph+res_uc$hypertension[1]*u_h+res_uc$stroke[1]*u_s+res_uc$MI[1]*u_m+res_uc$HF[1]*u_hf+res_uc$post_stroke[1]*u_ps+res_uc$post_MI[1]*u_pm+res_uc$post_HF[1]*u_phf
  res_uc$n_ph2h=0
  res_uc$t_ph2h=0
  
  for (i in 1:55) {
    res_uc$Age[i+1]=45+i
    res_uc$prehypertension[i+1]=res_uc$prehypertension[i]-res_uc$prehypertension[i]*(tp_pre_uc$p_ph2h[i]+tp_pre_uc$p_ph2hs[i]+tp_pre_uc$p_ph2hhf[i]+tp_pre_uc$p_ph2hm[i]+China_nm$p_d[i])
    res_uc$hypertension[i+1]=res_uc$hypertension[i]+res_uc$prehypertension[i]*tp_pre_uc$p_ph2h[i]-res_uc$hypertension[i]*(tp_hypertension$p_h2hf[i]+tp_hypertension$p_h2s[i]+tp_hypertension$p_h2m[i]+China_nm$p_d[i])
    res_uc$stroke[i+1]=res_uc$prehypertension[i]*tp_pre_uc$p_ph2hs[i]+res_uc$hypertension[i]*tp_hypertension$p_h2s[i]-res_uc$stroke[i]*China_nm$p_d[i]-res_uc$stroke[i]*p_s2d
    res_uc$MI[i+1]=res_uc$prehypertension[i]*tp_pre_uc$p_ph2hm[i]+res_uc$hypertension[i]*tp_hypertension$p_h2m[i]-res_uc$MI[i]*China_nm$p_d[i]-res_uc$MI[i]*p_m2d
    res_uc$HF[i+1]=res_uc$prehypertension[i]*tp_pre_uc$p_ph2hhf[i]+res_uc$hypertension[i]*tp_hypertension$p_h2hf[i]-res_uc$HF[i]*China_nm$p_d[i]-res_uc$HF[i]*p_hf2d
    res_uc$post_stroke[i+1]=res_uc$stroke[i]+res_uc$post_stroke[i]*(1 - p_ps2d -China_nm$p_d[i])
    res_uc$post_MI[i+1]=res_uc$MI[i]+res_uc$post_MI[i]*(1 - p_pm2d -China_nm$p_d[i])
    res_uc$post_HF[i+1]=res_uc$HF[i]+res_uc$post_HF[i]*(1 - p_phf2d -China_nm$p_d[i])
    res_uc$death[i+1]=China_nm$p_d[i]*sum(res_uc[i,2:9])+res_uc$stroke[i]*p_s2d+res_uc$MI[i]*p_m2d+res_uc$HF[i]*p_hf2d+res_uc$post_stroke[i]*p_ps2d+res_uc$post_MI[i]*p_pm2d+res_uc$post_HF[i]*p_phf2d+res_uc$death[i]
    res_uc$Check[i+1]=sum(res_uc[i+1,2:10],na.rm = TRUE)
    res_uc$Cost[i+1]=res_uc$prehypertension[i+1]*c_uc_p + res_uc$hypertension[i+1]*(c_uc_h+c_drug)+res_uc$stroke[i+1]*c_s+res_uc$MI[i+1]*c_m+res_uc$HF[i+1]*c_hf+res_uc$post_stroke[i+1]*c_ps+res_uc$post_MI[i+1]*c_pm+res_uc$post_HF[i+1]*c_phf+(res_uc$death[i+1]-res_uc$death[i])*c_eof
    res_uc$Life_years[i+1]=sum(res_uc[i+1,2:9],na.rm = TRUE)
    res_uc$Qaly[i+1]=res_uc$prehypertension[i+1]*u_ph+res_uc$hypertension[i+1]*u_h+res_uc$stroke[i+1]*u_s+res_uc$MI[i+1]*u_m+res_uc$HF[i+1]*u_hf+res_uc$post_stroke[i+1]*u_ps+res_uc$post_MI[i+1]*u_pm+res_uc$post_HF[i+1]*u_phf
    res_uc$n_ph2h[i+1]=res_uc$prehypertension[i]*tp_pre_uc$p_ph2h[i]
    res_uc$t_ph2h[i+1]=res_uc$prehypertension[i]*tp_pre_uc$p_ph2h[i]*i
  }
  
  cost_uc=sum(res_uc$Cost)/10000
  LYs_uc=sum(res_uc$Life_years)/10000
  QALYs_uc=sum(res_uc$Qaly)/10000
  n_ph2h_uc=sum(res_uc$n_ph2h)
  t_ph2h_uc=sum(res_uc$t_ph2h)
  tran_t_uc=t_ph2h_uc/n_ph2h_uc
  
  outcome_uc<-c(cost_uc,LYs_uc,QALYs_uc,tran_t_uc)
  
  return(outcome_uc)
}

cea_model_ls<-function(tp_pre_ls){
  n_input=10000
  #ls
  res_ls=as.data.frame(matrix(ncol = 16,nrow = 56))
  # res_ls=as.numeric(res_ls)
  # res_ls=format(res_ls,scientific=FALSE)
  colnames(res_ls)=c("Age","prehypertension","hypertension","stroke","MI","HF","post_stroke","post_MI","post_HF","death","Check","Cost","Life_years","Qaly","n_ph2h","t_ph2h")
  n=n_input
  res_ls$Age[1]=45
  res_ls$prehypertension[1]=n
  res_ls[1,3:10]=0
  res_ls$Check[1]=sum(res_ls[1,2:10],na.rm = TRUE)
  res_ls$Cost[1]=res_ls$prehypertension[1]*c_ls_p
  res_ls$Life_years[1]=sum(res_ls[1,2:9])
  res_ls$Qaly[1]=res_ls$prehypertension[1]*u_ph+res_ls$hypertension[1]*u_h+res_ls$stroke[1]*u_s+res_ls$MI[1]*u_m+res_ls$HF[1]*u_hf+res_ls$post_stroke[1]*u_ps+res_ls$post_MI[1]*u_pm+res_ls$post_HF[1]*u_phf
  res_ls$n_ph2h=0
  res_ls$t_ph2h=0
  
  for (i in 1:55) {
    res_ls$Age[i+1]=45+i
    res_ls$prehypertension[i+1]=res_ls$prehypertension[i]-res_ls$prehypertension[i]*(tp_pre_ls$p_ph2h[i]+tp_pre_ls$p_ph2hs[i]+tp_pre_ls$p_ph2hhf[i]+tp_pre_ls$p_ph2hm[i]+China_nm$p_d[i])
    res_ls$hypertension[i+1]=res_ls$hypertension[i]+res_ls$prehypertension[i]*tp_pre_ls$p_ph2h[i]-res_ls$hypertension[i]*(tp_hypertension$p_h2hf[i]+tp_hypertension$p_h2s[i]+tp_hypertension$p_h2m[i]+China_nm$p_d[i])
    res_ls$stroke[i+1]=res_ls$prehypertension[i]*tp_pre_ls$p_ph2hs[i]+res_ls$hypertension[i]*tp_hypertension$p_h2s[i]-res_ls$stroke[i]*China_nm$p_d[i]-res_ls$stroke[i]*p_s2d
    res_ls$MI[i+1]=res_ls$prehypertension[i]*tp_pre_ls$p_ph2hm[i]+res_ls$hypertension[i]*tp_hypertension$p_h2m[i]-res_ls$MI[i]*China_nm$p_d[i]-res_ls$MI[i]*p_m2d
    res_ls$HF[i+1]=res_ls$prehypertension[i]*tp_pre_ls$p_ph2hhf[i]+res_ls$hypertension[i]*tp_hypertension$p_h2hf[i]-res_ls$HF[i]*China_nm$p_d[i]-res_ls$HF[i]*p_hf2d
    res_ls$post_stroke[i+1]=res_ls$stroke[i]+res_ls$post_stroke[i]*(1 - p_ps2d -China_nm$p_d[i])
    res_ls$post_MI[i+1]=res_ls$MI[i]+res_ls$post_MI[i]*(1 - p_pm2d -China_nm$p_d[i])
    res_ls$post_HF[i+1]=res_ls$HF[i]+res_ls$post_HF[i]*(1 - p_phf2d -China_nm$p_d[i])
    res_ls$death[i+1]=China_nm$p_d[i]*sum(res_ls[i,2:9])+res_ls$stroke[i]*p_s2d+res_ls$MI[i]*p_m2d+res_ls$HF[i]*p_hf2d+res_ls$post_stroke[i]*p_ps2d+res_ls$post_MI[i]*p_pm2d+res_ls$post_HF[i]*p_phf2d+res_ls$death[i]
    res_ls$Check[i+1]=sum(res_ls[i+1,2:10],na.rm = TRUE)
    res_ls$Cost[i+1]=res_ls$prehypertension[i+1]*c_ls_p + res_ls$hypertension[i+1]*(c_ls_h+c_drug)+res_ls$stroke[i+1]*c_s+res_ls$MI[i+1]*c_m+res_ls$HF[i+1]*c_hf+res_ls$post_stroke[i+1]*c_ps+res_ls$post_MI[i+1]*c_pm+res_ls$post_HF[i+1]*c_phf+(res_ls$death[i+1]-res_ls$death[i])*c_eof
    res_ls$Life_years[i+1]=sum(res_ls[i+1,2:9],na.rm = TRUE)
    res_ls$Qaly[i+1]=res_ls$prehypertension[i+1]*u_ph+res_ls$hypertension[i+1]*u_h+res_ls$stroke[i+1]*u_s+res_ls$MI[i+1]*u_m+res_ls$HF[i+1]*u_hf+res_ls$post_stroke[i+1]*u_ps+res_ls$post_MI[i+1]*u_pm+res_ls$post_HF[i+1]*u_phf
    res_ls$n_ph2h[i+1]=res_ls$prehypertension[i]*tp_pre_ls$p_ph2h[i]
    res_ls$t_ph2h[i+1]=res_ls$prehypertension[i]*tp_pre_ls$p_ph2h[i]*i
  }
  
  cost_ls=sum(res_ls$Cost)/10000
  LYs_ls=sum(res_ls$Life_years)/10000
  QALYs_ls=sum(res_ls$Qaly)/10000
  n_ph2h_ls=sum(res_ls$n_ph2h)
  t_ph2h_ls=sum(res_ls$t_ph2h)
  tran_t_ls=t_ph2h_ls/n_ph2h_ls
  
  outcome_ls<-c(cost_ls,LYs_ls,QALYs_ls,tran_t_ls)
  
  return(outcome_ls)
}

cea_model_sex<-function(tp_pre_sex){
  n_input=10000
  #sex
  res_sex=as.data.frame(matrix(ncol = 16,nrow = 56))
  # res_sex=as.numeric(res_sex)
  # res_sex=format(res_sex,scientific=FALSE)
  colnames(res_sex)=c("Age","prehypertension","hypertension","stroke","MI","HF","post_stroke","post_MI","post_HF","death","Check","Cost","Life_years","Qaly","n_ph2h","t_ph2h")
  n=n_input
  res_sex$Age[1]=45
  res_sex$prehypertension[1]=n
  res_sex[1,3:10]=0
  res_sex$Check[1]=sum(res_sex[1,2:10],na.rm = TRUE)
  res_sex$Cost[1]=res_sex$prehypertension[1]*c_sex_p+c_sex_s
  res_sex$Life_years[1]=sum(res_sex[1,2:9])
  res_sex$Qaly[1]=res_sex$prehypertension[1]*u_ph+res_sex$hypertension[1]*u_h+res_sex$stroke[1]*u_s+res_sex$MI[1]*u_m+res_sex$HF[1]*u_hf+res_sex$post_stroke[1]*u_ps+res_sex$post_MI[1]*u_pm+res_sex$post_HF[1]*u_phf
  res_sex$n_ph2h=0
  res_sex$t_ph2h=0
  
  for (i in 1:55) {
    res_sex$Age[i+1]=45+i
    res_sex$prehypertension[i+1]=res_sex$prehypertension[i]-res_sex$prehypertension[i]*(tp_pre_sex$p_ph2h[i]+tp_pre_sex$p_ph2hs[i]+tp_pre_sex$p_ph2hhf[i]+tp_pre_sex$p_ph2hm[i]+China_nm$p_d[i])
    res_sex$hypertension[i+1]=res_sex$hypertension[i]+res_sex$prehypertension[i]*tp_pre_sex$p_ph2h[i]-res_sex$hypertension[i]*(tp_hypertension$p_h2hf[i]+tp_hypertension$p_h2s[i]+tp_hypertension$p_h2m[i]+China_nm$p_d[i])
    res_sex$stroke[i+1]=res_sex$prehypertension[i]*tp_pre_sex$p_ph2hs[i]+res_sex$hypertension[i]*tp_hypertension$p_h2s[i]-res_sex$stroke[i]*China_nm$p_d[i]-res_sex$stroke[i]*p_s2d
    res_sex$MI[i+1]=res_sex$prehypertension[i]*tp_pre_sex$p_ph2hm[i]+res_sex$hypertension[i]*tp_hypertension$p_h2m[i]-res_sex$MI[i]*China_nm$p_d[i]-res_sex$MI[i]*p_m2d
    res_sex$HF[i+1]=res_sex$prehypertension[i]*tp_pre_sex$p_ph2hhf[i]+res_sex$hypertension[i]*tp_hypertension$p_h2hf[i]-res_sex$HF[i]*China_nm$p_d[i]-res_sex$HF[i]*p_hf2d
    res_sex$post_stroke[i+1]=res_sex$stroke[i]+res_sex$post_stroke[i]*(1 - p_ps2d -China_nm$p_d[i])
    res_sex$post_MI[i+1]=res_sex$MI[i]+res_sex$post_MI[i]*(1 - p_pm2d -China_nm$p_d[i])
    res_sex$post_HF[i+1]=res_sex$HF[i]+res_sex$post_HF[i]*(1 - p_phf2d -China_nm$p_d[i])
    res_sex$death[i+1]=China_nm$p_d[i]*sum(res_sex[i,2:9])+res_sex$stroke[i]*p_s2d+res_sex$MI[i]*p_m2d+res_sex$HF[i]*p_hf2d+res_sex$post_stroke[i]*p_ps2d+res_sex$post_MI[i]*p_pm2d+res_sex$post_HF[i]*p_phf2d+res_sex$death[i]
    res_sex$Check[i+1]=sum(res_sex[i+1,2:10],na.rm = TRUE)
    res_sex$Cost[i+1]=res_sex$prehypertension[i+1]*c_sex_p + res_sex$hypertension[i+1]*(c_sex_h+c_drug)+res_sex$stroke[i+1]*c_s+res_sex$MI[i+1]*c_m+res_sex$HF[i+1]*c_hf+res_sex$post_stroke[i+1]*c_ps+res_sex$post_MI[i+1]*c_pm+res_sex$post_HF[i+1]*c_phf+(res_sex$death[i+1]-res_sex$death[i])*c_eof
    res_sex$Life_years[i+1]=sum(res_sex[i+1,2:9],na.rm = TRUE)
    res_sex$Qaly[i+1]=res_sex$prehypertension[i+1]*u_ph+res_sex$hypertension[i+1]*u_h+res_sex$stroke[i+1]*u_s+res_sex$MI[i+1]*u_m+res_sex$HF[i+1]*u_hf+res_sex$post_stroke[i+1]*u_ps+res_sex$post_MI[i+1]*u_pm+res_sex$post_HF[i+1]*u_phf
    res_sex$n_ph2h[i+1]=res_sex$prehypertension[i]*tp_pre_sex$p_ph2h[i]
    res_sex$t_ph2h[i+1]=res_sex$prehypertension[i]*tp_pre_sex$p_ph2h[i]*i
  }
  
  cost_sex=sum(res_sex$Cost)/10000
  LYs_sex=sum(res_sex$Life_years)/10000
  QALYs_sex=sum(res_sex$Qaly)/10000
  n_ph2h_sex=sum(res_sex$n_ph2h)
  t_ph2h_sex=sum(res_sex$t_ph2h)
  tran_t_sex=t_ph2h_sex/n_ph2h_sex
  
  outcome_sex<-c(cost_sex,LYs_sex,QALYs_sex,tran_t_sex)
  
  return(outcome_sex)
}

cea_model_rex<-function(tp_pre_rex){
  n_input=10000
  #rex
  res_rex=as.data.frame(matrix(ncol = 16,nrow = 56))
  # res_rex=as.numeric(res_rex)
  # res_rex=format(res_rex,scientific=FALSE)
  colnames(res_rex)=c("Age","prehypertension","hypertension","stroke","MI","HF","post_stroke","post_MI","post_HF","death","Check","Cost","Life_years","Qaly","n_ph2h","t_ph2h")
  n=n_input
  res_rex$Age[1]=45
  res_rex$prehypertension[1]=n
  res_rex[1,3:10]=0
  res_rex$Check[1]=sum(res_rex[1,2:10],na.rm = TRUE)
  res_rex$Cost[1]=res_rex$prehypertension[1]*c_rex_p+c_rex_s
  res_rex$Life_years[1]=sum(res_rex[1,2:9])
  res_rex$Qaly[1]=res_rex$prehypertension[1]*u_ph+res_rex$hypertension[1]*u_h+res_rex$stroke[1]*u_s+res_rex$MI[1]*u_m+res_rex$HF[1]*u_hf+res_rex$post_stroke[1]*u_ps+res_rex$post_MI[1]*u_pm+res_rex$post_HF[1]*u_phf
  res_rex$n_ph2h=0
  res_rex$t_ph2h=0
  
  for (i in 1:55) {
    res_rex$Age[i+1]=45+i
    res_rex$prehypertension[i+1]=res_rex$prehypertension[i]-res_rex$prehypertension[i]*(tp_pre_rex$p_ph2h[i]+tp_pre_rex$p_ph2hs[i]+tp_pre_rex$p_ph2hhf[i]+tp_pre_rex$p_ph2hm[i]+China_nm$p_d[i])
    res_rex$hypertension[i+1]=res_rex$hypertension[i]+res_rex$prehypertension[i]*tp_pre_rex$p_ph2h[i]-res_rex$hypertension[i]*(tp_hypertension$p_h2hf[i]+tp_hypertension$p_h2s[i]+tp_hypertension$p_h2m[i]+China_nm$p_d[i])
    res_rex$stroke[i+1]=res_rex$prehypertension[i]*tp_pre_rex$p_ph2hs[i]+res_rex$hypertension[i]*tp_hypertension$p_h2s[i]-res_rex$stroke[i]*China_nm$p_d[i]-res_rex$stroke[i]*p_s2d
    res_rex$MI[i+1]=res_rex$prehypertension[i]*tp_pre_rex$p_ph2hm[i]+res_rex$hypertension[i]*tp_hypertension$p_h2m[i]-res_rex$MI[i]*China_nm$p_d[i]-res_rex$MI[i]*p_m2d
    res_rex$HF[i+1]=res_rex$prehypertension[i]*tp_pre_rex$p_ph2hhf[i]+res_rex$hypertension[i]*tp_hypertension$p_h2hf[i]-res_rex$HF[i]*China_nm$p_d[i]-res_rex$HF[i]*p_hf2d
    res_rex$post_stroke[i+1]=res_rex$stroke[i]+res_rex$post_stroke[i]*(1 - p_ps2d -China_nm$p_d[i])
    res_rex$post_MI[i+1]=res_rex$MI[i]+res_rex$post_MI[i]*(1 - p_pm2d -China_nm$p_d[i])
    res_rex$post_HF[i+1]=res_rex$HF[i]+res_rex$post_HF[i]*(1 - p_phf2d -China_nm$p_d[i])
    res_rex$death[i+1]=China_nm$p_d[i]*sum(res_rex[i,2:9])+res_rex$stroke[i]*p_s2d+res_rex$MI[i]*p_m2d+res_rex$HF[i]*p_hf2d+res_rex$post_stroke[i]*p_ps2d+res_rex$post_MI[i]*p_pm2d+res_rex$post_HF[i]*p_phf2d+res_rex$death[i]
    res_rex$Check[i+1]=sum(res_rex[i+1,2:10],na.rm = TRUE)
    res_rex$Cost[i+1]=res_rex$prehypertension[i+1]*c_rex_p + res_rex$hypertension[i+1]*(c_rex_h+c_drug)+res_rex$stroke[i+1]*c_s+res_rex$MI[i+1]*c_m+res_rex$HF[i+1]*c_hf+res_rex$post_stroke[i+1]*c_ps+res_rex$post_MI[i+1]*c_pm+res_rex$post_HF[i+1]*c_phf+(res_rex$death[i+1]-res_rex$death[i])*c_eof
    res_rex$Life_years[i+1]=sum(res_rex[i+1,2:9],na.rm = TRUE)
    res_rex$Qaly[i+1]=res_rex$prehypertension[i+1]*u_ph+res_rex$hypertension[i+1]*u_h+res_rex$stroke[i+1]*u_s+res_rex$MI[i+1]*u_m+res_rex$HF[i+1]*u_hf+res_rex$post_stroke[i+1]*u_ps+res_rex$post_MI[i+1]*u_pm+res_rex$post_HF[i+1]*u_phf
    res_rex$n_ph2h[i+1]=res_rex$prehypertension[i]*tp_pre_rex$p_ph2h[i]
    res_rex$t_ph2h[i+1]=res_rex$prehypertension[i]*tp_pre_rex$p_ph2h[i]*i
  }
  
  cost_rex=sum(res_rex$Cost)/10000
  LYs_rex=sum(res_rex$Life_years)/10000
  QALYs_rex=sum(res_rex$Qaly)/10000
  n_ph2h_rex=sum(res_rex$n_ph2h)
  t_ph2h_rex=sum(res_rex$t_ph2h)
  tran_t_rex=t_ph2h_rex/n_ph2h_rex
  
  outcome_rex<-c(cost_rex,LYs_rex,QALYs_rex,tran_t_rex)
  
  return(outcome_rex)
}

cea_model_di<-function(tp_pre_di){
  n_input=10000
  #di
  res_di=as.data.frame(matrix(ncol = 16,nrow = 56))
  # res_di=as.numeric(res_di)
  # res_di=format(res_di,scientific=FALSE)
  colnames(res_di)=c("Age","prehypertension","hypertension","stroke","MI","HF","post_stroke","post_MI","post_HF","death","Check","Cost","Life_years","Qaly","n_ph2h","t_ph2h")
  n=n_input
  res_di$Age[1]=45
  res_di$prehypertension[1]=n
  res_di[1,3:10]=0
  res_di$Check[1]=sum(res_di[1,2:10],na.rm = TRUE)
  res_di$Cost[1]=res_di$prehypertension[1]*c_di_p+c_di_s
  res_di$Life_years[1]=sum(res_di[1,2:9])
  res_di$Qaly[1]=res_di$prehypertension[1]*u_ph+res_di$hypertension[1]*u_h+res_di$stroke[1]*u_s+res_di$MI[1]*u_m+res_di$HF[1]*u_hf+res_di$post_stroke[1]*u_ps+res_di$post_MI[1]*u_pm+res_di$post_HF[1]*u_phf
  res_di$n_ph2h=0
  res_di$t_ph2h=0
  
  for (i in 1:55) {
    res_di$Age[i+1]=45+i
    res_di$prehypertension[i+1]=res_di$prehypertension[i]-res_di$prehypertension[i]*(tp_pre_di$p_ph2h[i]+tp_pre_di$p_ph2hs[i]+tp_pre_di$p_ph2hhf[i]+tp_pre_di$p_ph2hm[i]+China_nm$p_d[i])
    res_di$hypertension[i+1]=res_di$hypertension[i]+res_di$prehypertension[i]*tp_pre_di$p_ph2h[i]-res_di$hypertension[i]*(tp_hypertension$p_h2hf[i]+tp_hypertension$p_h2s[i]+tp_hypertension$p_h2m[i]+China_nm$p_d[i])
    res_di$stroke[i+1]=res_di$prehypertension[i]*tp_pre_di$p_ph2hs[i]+res_di$hypertension[i]*tp_hypertension$p_h2s[i]-res_di$stroke[i]*China_nm$p_d[i]-res_di$stroke[i]*p_s2d
    res_di$MI[i+1]=res_di$prehypertension[i]*tp_pre_di$p_ph2hm[i]+res_di$hypertension[i]*tp_hypertension$p_h2m[i]-res_di$MI[i]*China_nm$p_d[i]-res_di$MI[i]*p_m2d
    res_di$HF[i+1]=res_di$prehypertension[i]*tp_pre_di$p_ph2hhf[i]+res_di$hypertension[i]*tp_hypertension$p_h2hf[i]-res_di$HF[i]*China_nm$p_d[i]-res_di$HF[i]*p_hf2d
    res_di$post_stroke[i+1]=res_di$stroke[i]+res_di$post_stroke[i]*(1 - p_ps2d -China_nm$p_d[i])
    res_di$post_MI[i+1]=res_di$MI[i]+res_di$post_MI[i]*(1 - p_pm2d -China_nm$p_d[i])
    res_di$post_HF[i+1]=res_di$HF[i]+res_di$post_HF[i]*(1 - p_phf2d -China_nm$p_d[i])
    res_di$death[i+1]=China_nm$p_d[i]*sum(res_di[i,2:9])+res_di$stroke[i]*p_s2d+res_di$MI[i]*p_m2d+res_di$HF[i]*p_hf2d+res_di$post_stroke[i]*p_ps2d+res_di$post_MI[i]*p_pm2d+res_di$post_HF[i]*p_phf2d+res_di$death[i]
    res_di$Check[i+1]=sum(res_di[i+1,2:10],na.rm = TRUE)
    res_di$Cost[i+1]=res_di$prehypertension[i+1]*c_di_p + res_di$hypertension[i+1]*(c_di_h+c_drug)+res_di$stroke[i+1]*c_s+res_di$MI[i+1]*c_m+res_di$HF[i+1]*c_hf+res_di$post_stroke[i+1]*c_ps+res_di$post_MI[i+1]*c_pm+res_di$post_HF[i+1]*c_phf+(res_di$death[i+1]-res_di$death[i])*c_eof
    res_di$Life_years[i+1]=sum(res_di[i+1,2:9],na.rm = TRUE)
    res_di$Qaly[i+1]=res_di$prehypertension[i+1]*u_ph+res_di$hypertension[i+1]*u_h+res_di$stroke[i+1]*u_s+res_di$MI[i+1]*u_m+res_di$HF[i+1]*u_hf+res_di$post_stroke[i+1]*u_ps+res_di$post_MI[i+1]*u_pm+res_di$post_HF[i+1]*u_phf
    res_di$n_ph2h[i+1]=res_di$prehypertension[i]*tp_pre_di$p_ph2h[i]
    res_di$t_ph2h[i+1]=res_di$prehypertension[i]*tp_pre_di$p_ph2h[i]*i
  }
  
  cost_di=sum(res_di$Cost)/10000
  LYs_di=sum(res_di$Life_years)/10000
  QALYs_di=sum(res_di$Qaly)/10000
  n_ph2h_di=sum(res_di$n_ph2h)
  t_ph2h_di=sum(res_di$t_ph2h)
  tran_t_di=t_ph2h_di/n_ph2h_di
  
  outcome_di<-c(cost_di,LYs_di,QALYs_di,tran_t_di)
  
  return(outcome_di)
}
