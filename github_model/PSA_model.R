time1 <- Sys.time()
source("Param-input-PSA.R")
time2 <- Sys.time()
print(time2-time1)


#Time difference of 14.20823 mins 100,000
#Time difference of 1.163174 mins 10,000


# save(tp_di,tp_ls,tp_Rex,tp_Sex,tp_uc,file = "tp_data.RData")
# load("tp_data.RData")


source("pre-cea-model-PSA_version.R")

dftemp1=cbind(PSAp_s2d,PSAp_m2d,PSAp_hf2d,PSAp_ps2d,PSAp_pm2d,PSAp_phf2d,PSAc_uc_p,PSAc_uc_h,PSAc_ls_p,PSAc_ls_h,PSAc_sex_p,PSAc_sex_h,PSAc_rex_p,PSAc_rex_h,PSAc_di_p,
              PSAc_di_h,PSAc_s,PSAc_m,PSAc_hf,PSAc_ps,PSAc_pm,PSAc_phf,PSAu_h,PSAu_s,PSAu_m,PSAu_hf,PSAu_ps,PSAu_pm,PSAu_phf,PSAc_drug,PSAc_eof,PSAdisu)
dftemp1<-as.data.frame(dftemp1)

# write.csv(dftemp1,file = "dftemp_0818.csv",row.names = FALSE)
# dftemp1=read.csv("dftemp_0818.csv")


PSA_outcome<-function(j){
  
  res_base_case<-as.data.frame(matrix(ncol = 4,nrow = 6))
  colnames(res_base_case)<-c("Costs","Lifeyears","QALYs","Trans")
  res_base_case[1,]<-cea_model_zero_psa(tp_zero,j,dftemp1)
  res_base_case[2,]<-cea_model_uc_psa(tp_uc,j,dftemp1)
  res_base_case[3,]<-cea_model_ls_psa(tp_ls,j,dftemp1)
  res_base_case[4,]<-cea_model_sex_psa(tp_Sex,j,dftemp1)
  res_base_case[5,]<-cea_model_rex_psa(tp_Rex,j,dftemp1)
  res_base_case[6,]<-cea_model_di_psa(tp_di,j,dftemp1)
  
  res_psa<-as.data.frame(matrix(ncol = 12,nrow = 1))
  colnames(res_psa)<-c("Costs_zero","QALYs_zero","Costs_uc","QALYs_uc","Costs_ls","QALYs_ls","Costs_sex","QALYs_sex","Costs_rex","QALYs_rex","Costs_di","QALYs_di")
  res_psa$Costs_zero=res_base_case$Costs[1]
  res_psa$QALYs_zero=res_base_case$QALYs[1]
  res_psa$Costs_uc=res_base_case$Costs[2]
  res_psa$QALYs_uc=res_base_case$QALYs[2]
  res_psa$Costs_ls=res_base_case$Costs[3]
  res_psa$QALYs_ls=res_base_case$QALYs[3]
  res_psa$Costs_sex=res_base_case$Costs[4]
  res_psa$QALYs_sex=res_base_case$QALYs[4]
  res_psa$Costs_rex=res_base_case$Costs[5]
  res_psa$QALYs_rex=res_base_case$QALYs[5]
  res_psa$Costs_di=res_base_case$Costs[6]
  res_psa$QALYs_di=res_base_case$QALYs[6]
  
  return(res_psa)
  
}

cl <- makeCluster(8)
registerDoParallel(cl)

time1 <- Sys.time()
Res_fe_psa <-foreach (j = 1:10000, .combine = rbind) %dopar% {
    temp_psa=PSA_outcome(j)
    }
# temp_psa=cbind(paste(i,j,sep = ","),temp_psa)
# df_temp_psa[j,]=temp_psa
time2 <- Sys.time()
print(time2-time1)
#Time difference of 2.396795 mins

stopImplicitCluster()
stopCluster(cl)

