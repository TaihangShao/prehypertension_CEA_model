trans_zero<-read.delim("zero.txt",header = TRUE)
trans_sex<-read.delim("sex.txt",header = TRUE)

# transition probabilities
trans_zero_46_60<-trans_zero[1:15,]
trans_zero_51_65<-trans_zero[6:20,]
trans_zero_56_70<-trans_zero[11:25,]
trans_zero_61_75<-trans_zero[16:30,]
trans_zero_66_80<-trans_zero[21:35,]
trans_zero_71_85<-trans_zero[26:40,]
trans_zero_76_90<-trans_zero[31:45,]
trans_zero_81_95<-trans_zero[36:50,]
trans_zero_86_100<-trans_zero[41:55,]


trans_sex_46_60<-trans_sex[1:15,]
trans_sex_51_65<-trans_sex[6:20,]
trans_sex_56_70<-trans_sex[11:25,]
trans_sex_61_75<-trans_sex[16:30,]
trans_sex_66_80<-trans_sex[21:35,]
trans_sex_71_85<-trans_sex[26:40,]
trans_sex_76_90<-trans_sex[31:45,]
trans_sex_81_95<-trans_sex[36:50,]
trans_sex_86_100<-trans_sex[41:55,]

# parameters
## cost of intervention
c_zero_p=0
c_sex_p=200.52
## others
c_h_m=103.55
c_s=4480.53
c_m=4532.35
c_hf=1300.16
c_ps=448.05
c_pm=453.24
c_phf=130.02
c_eof=2179
u_phf=0.96
u_h=0.96
u_s=0.55
u_m=0.6
u_hf=0.63
u_ps=0.65
u_pm=0.7
p_s2d=0.0275
p_m2d=0.097
p_hf2d=0.138
p_ps2d=0.0713
p_pm2d=0.061
p_phf2d=0.248
disu=0.05
# population
pop_45_49<-12370.8392344918*10000*0.2144
pop_50_54<-12047.9225662987*10000*0.2144
pop_55_59<-9582.44862969788*10000*0.2144
pop_60_64<-7854.5339580685*10000*0.2144
pop_65_69<-7126.67771798263*10000*0.2144
pop_70_74<-4614.11176727028*10000*0.2144
pop_75_79<-2925.13856884848*10000*0.2144
pop_80_84<-1852.88963215603*10000*0.2144
pop_85_89<-910.273107935333*10000*0.2144

# cvd rate
cvd_45_49<-0.027
cvd_50_54<-0.076
cvd_55_59<-0.077
cvd_60_64<-0.29
cvd_65_69<-0.027
cvd_70_74<-0.076
cvd_75_79<-0.077
cvd_80_84<-0.29
cvd_85_89<-0.077

cea_model_bia<-function(input,c_i,tp_pre_dur1,tp_pre_dur2,duration,comp,cvd,cvd_rate){
  n_input=input*comp
  n_input_nc=input*(1-comp)
  
  if(cvd==1){
    n=n_input*cvd_rate
    nc=n_input_nc*cvd_rate
  }else{
    n=n_input
    nc=n_input_nc
  }
  
  dura=duration
  
  # comp
  res_zero_n=as.data.frame(matrix(ncol = 13,nrow = 16))
  colnames(res_zero_n)=c("Year","prehypertension","hypertension","stroke","MI","HF","post_stroke","post_MI","post_HF","death","Check","Cost_i","Cost_saving")
  res_zero_n$Year[1]=0
  res_zero_n$prehypertension[1]=n
  res_zero_n[1,3:10]=0
  res_zero_n$Check[1]=sum(res_zero_n[1,2:10],na.rm = TRUE)
  res_zero_n$Cost_i[1]=res_zero_n$prehypertension[1]*c_i
  res_zero_n$Cost_saving[1]=0
  
 
  for (i in 1:15) {
    
    if (i>dura){
      tp_temp=tp_pre_dur1
    }else{
      tp_temp=tp_pre_dur2
    }
    
    res_zero_n$Year[i+1]=i
    res_zero_n$prehypertension[i+1]=res_zero_n$prehypertension[i]-res_zero_n$prehypertension[i]*(tp_temp$p_ph2h[i]+tp_temp$p_ph2hs[i]+tp_temp$p_ph2hhf[i]+tp_temp$p_ph2hm[i]+tp_temp$p_d[i])
    res_zero_n$hypertension[i+1]=res_zero_n$hypertension[i]+res_zero_n$prehypertension[i]*tp_temp$p_ph2h[i]-res_zero_n$hypertension[i]*(tp_temp$p_h2hf[i]+tp_temp$p_h2s[i]+tp_temp$p_h2m[i]+tp_temp$p_d[i])
    res_zero_n$stroke[i+1]=res_zero_n$prehypertension[i]*tp_temp$p_ph2hs[i]+res_zero_n$hypertension[i]*tp_temp$p_h2s[i]-res_zero_n$stroke[i]*tp_temp$p_d[i]-res_zero_n$stroke[i]*p_s2d
    res_zero_n$MI[i+1]=res_zero_n$prehypertension[i]*tp_temp$p_ph2hm[i]+res_zero_n$hypertension[i]*tp_temp$p_h2m[i]-res_zero_n$MI[i]*tp_temp$p_d[i]-res_zero_n$MI[i]*p_m2d
    res_zero_n$HF[i+1]=res_zero_n$prehypertension[i]*tp_temp$p_ph2hhf[i]+res_zero_n$hypertension[i]*tp_temp$p_h2hf[i]-res_zero_n$HF[i]*tp_temp$p_d[i]-res_zero_n$HF[i]*p_hf2d
    res_zero_n$post_stroke[i+1]=res_zero_n$stroke[i]+res_zero_n$post_stroke[i]*(1 - p_ps2d -tp_temp$p_d[i])
    res_zero_n$post_MI[i+1]=res_zero_n$MI[i]+res_zero_n$post_MI[i]*(1 - p_pm2d -tp_temp$p_d[i])
    res_zero_n$post_HF[i+1]=res_zero_n$HF[i]+res_zero_n$post_HF[i]*(1 - p_phf2d -tp_temp$p_d[i])
    res_zero_n$death[i+1]=tp_temp$p_d[i]*sum(res_zero_n[i,2:9])+res_zero_n$stroke[i]*p_s2d+res_zero_n$MI[i]*p_m2d+res_zero_n$HF[i]*p_hf2d+res_zero_n$post_stroke[i]*p_ps2d+res_zero_n$post_MI[i]*p_pm2d+res_zero_n$post_HF[i]*p_phf2d+res_zero_n$death[i]
    res_zero_n$Check[i+1]=sum(res_zero_n[i+1,2:10],na.rm = TRUE)

    if (i>dura){
      res_zero_n$Cost_i[i+1]=0+res_zero_n$Cost_i[i]
    }else{
      res_zero_n$Cost_i[i+1]=(res_zero_n$prehypertension[i+1]*c_i)/((1+disu)^i)+res_zero_n$Cost_i[i]
      }
    
    res_zero_n$Cost_saving[i+1]=res_zero_n$Cost_saving[i]+(res_zero_n$hypertension[i+1]*c_h_m+res_zero_n$stroke[i+1]*c_s+res_zero_n$MI[i+1]*c_m+res_zero_n$HF[i+1]*c_hf+res_zero_n$post_stroke[i+1]*c_ps+res_zero_n$post_MI[i+1]*c_pm+res_zero_n$post_HF[i+1]*c_phf+(res_zero_n$death[i+1]-res_zero_n$death[i])*c_eof)/((1+disu)^i)
  }
  
  
  # non-comp
  res_zero_nc=as.data.frame(matrix(ncol = 13,nrow = 16))
  colnames(res_zero_nc)=c("Year","prehypertension","hypertension","stroke","MI","HF","post_stroke","post_MI","post_HF","death","Check","Cost_i","Cost_saving")
  res_zero_nc$Year[1]=0
  res_zero_nc$prehypertension[1]=nc
  res_zero_nc[1,3:10]=0
  res_zero_nc$Check[1]=sum(res_zero_nc[1,2:10],na.rm = TRUE)
  res_zero_nc$Cost_i[1]=res_zero_nc$prehypertension[1]*c_i
  res_zero_nc$Cost_saving[1]=0
  
  
  for (i in 1:15) {
    
    tp_temp=tp_pre_dur1
    
    res_zero_nc$Year[i+1]=i
    res_zero_nc$prehypertension[i+1]=res_zero_nc$prehypertension[i]-res_zero_nc$prehypertension[i]*(tp_temp$p_ph2h[i]+tp_temp$p_ph2hs[i]+tp_temp$p_ph2hhf[i]+tp_temp$p_ph2hm[i]+tp_temp$p_d[i])
    res_zero_nc$hypertension[i+1]=res_zero_nc$hypertension[i]+res_zero_nc$prehypertension[i]*tp_temp$p_ph2h[i]-res_zero_nc$hypertension[i]*(tp_temp$p_h2hf[i]+tp_temp$p_h2s[i]+tp_temp$p_h2m[i]+tp_temp$p_d[i])
    res_zero_nc$stroke[i+1]=res_zero_nc$prehypertension[i]*tp_temp$p_ph2hs[i]+res_zero_nc$hypertension[i]*tp_temp$p_h2s[i]-res_zero_nc$stroke[i]*tp_temp$p_d[i]-res_zero_nc$stroke[i]*p_s2d
    res_zero_nc$MI[i+1]=res_zero_nc$prehypertension[i]*tp_temp$p_ph2hm[i]+res_zero_nc$hypertension[i]*tp_temp$p_h2m[i]-res_zero_nc$MI[i]*tp_temp$p_d[i]-res_zero_nc$MI[i]*p_m2d
    res_zero_nc$HF[i+1]=res_zero_nc$prehypertension[i]*tp_temp$p_ph2hhf[i]+res_zero_nc$hypertension[i]*tp_temp$p_h2hf[i]-res_zero_nc$HF[i]*tp_temp$p_d[i]-res_zero_nc$HF[i]*p_hf2d
    res_zero_nc$post_stroke[i+1]=res_zero_nc$stroke[i]+res_zero_nc$post_stroke[i]*(1 - p_ps2d -tp_temp$p_d[i])
    res_zero_nc$post_MI[i+1]=res_zero_nc$MI[i]+res_zero_nc$post_MI[i]*(1 - p_pm2d -tp_temp$p_d[i])
    res_zero_nc$post_HF[i+1]=res_zero_nc$HF[i]+res_zero_nc$post_HF[i]*(1 - p_phf2d -tp_temp$p_d[i])
    res_zero_nc$death[i+1]=tp_temp$p_d[i]*sum(res_zero_nc[i,2:9])+res_zero_nc$stroke[i]*p_s2d+res_zero_nc$MI[i]*p_m2d+res_zero_nc$HF[i]*p_hf2d+res_zero_nc$post_stroke[i]*p_ps2d+res_zero_nc$post_MI[i]*p_pm2d+res_zero_nc$post_HF[i]*p_phf2d+res_zero_nc$death[i]
    res_zero_nc$Check[i+1]=sum(res_zero_nc[i+1,2:10],na.rm = TRUE)
    
    if (i>dura){
      res_zero_nc$Cost_i[i+1]=0+res_zero_nc$Cost_i[i]
    }else{
      res_zero_nc$Cost_i[i+1]=(res_zero_nc$prehypertension[i+1]*c_i)/((1+disu)^i)+res_zero_nc$Cost_i[i]
    }
    
    res_zero_nc$Cost_saving[i+1]=res_zero_nc$Cost_saving[i]+(res_zero_nc$hypertension[i+1]*c_h_m+res_zero_nc$stroke[i+1]*c_s+res_zero_nc$MI[i+1]*c_m+res_zero_nc$HF[i+1]*c_hf+res_zero_nc$post_stroke[i+1]*c_ps+res_zero_nc$post_MI[i+1]*c_pm+res_zero_nc$post_HF[i+1]*c_phf+(res_zero_nc$death[i+1]-res_zero_nc$death[i])*c_eof)/((1+disu)^i)
  }
  
  
  res_zero=res_zero_nc + res_zero_n
  res_zero$Year=res_zero$Year/2
  
  outcome<-cbind(res_zero[,1],res_zero[,12:13])
  # outcome=rbind(c("Cost_i","Cost_cvd","Cost_h"),outcome)
  
  return(outcome)
}

# delta
res_delta_45_49<-as.data.frame(matrix(nrow=1,ncol=6))
res_delta_45_49[1,]<-c("Year","Cost","Saving","Duration","Compliance","CVE")
# res_delta_45_49[1,1]<-"Year"
# res_delta_45_49[2:17,1]<-c(seq(0,15,1))
res_delta_50_54<-as.data.frame(matrix(nrow=1,ncol=6))
res_delta_50_54[1,]<-c("Year","Cost","Saving","Duration","Compliance","CVE")
# res_delta_50_54[1,1]<-"Year"
# res_delta_50_54[2:17,1]<-c(seq(0,15,1))
res_delta_55_59<-as.data.frame(matrix(nrow=1,ncol=6))
res_delta_55_59[1,]<-c("Year","Cost","Saving","Duration","Compliance","CVE")
# res_delta_55_59[1,1]<-"Year"
# res_delta_55_59[2:17,1]<-c(seq(0,15,1))
res_delta_60_64<-as.data.frame(matrix(nrow=1,ncol=6))
res_delta_60_64[1,]<-c("Year","Cost","Saving","Duration","Compliance","CVE")
# res_delta_60_64[1,1]<-"Year"
# res_delta_60_64[2:17,1]<-c(seq(0,15,1))

# calculation

for (i in 1:15) {
  for (j in c(0.2,0.4,0.6,0.8,1)) {
    for (k in c(0,1)) {
      res_zero_45_49=cea_model_bia(pop_45_49,c_zero_p,trans_zero_46_60,trans_zero_46_60,i,j,k,cvd_45_49)
      res_zero_50_54=cea_model_bia(pop_50_54,c_zero_p,trans_zero_51_65,trans_zero_51_65,i,j,k,cvd_50_54)
      res_zero_55_59=cea_model_bia(pop_55_59,c_zero_p,trans_zero_56_70,trans_zero_56_70,i,j,k,cvd_55_59)
      res_zero_60_64=cea_model_bia(pop_60_64,c_zero_p,trans_zero_61_75,trans_zero_61_75,i,j,k,cvd_60_64)
      res_zero_65_69=cea_model_bia(pop_65_69,c_zero_p,trans_zero_66_80,trans_zero_66_80,i,j,k,cvd_65_69)
      res_zero_70_74=cea_model_bia(pop_70_74,c_zero_p,trans_zero_71_85,trans_zero_71_85,i,j,k,cvd_70_74)
      res_zero_75_79=cea_model_bia(pop_75_79,c_zero_p,trans_zero_76_90,trans_zero_76_90,i,j,k,cvd_75_79)
      res_zero_80_84=cea_model_bia(pop_80_84,c_zero_p,trans_zero_81_95,trans_zero_81_95,i,j,k,cvd_80_84)
      res_zero_85_89=cea_model_bia(pop_85_89,c_zero_p,trans_zero_86_100,trans_zero_86_100,i,j,k,cvd_85_89)
      
      res_sex_45_49=cea_model_bia(pop_45_49,c_sex_p,trans_zero_46_60,trans_sex_46_60,i,j,k,cvd_45_49)
      res_sex_50_54=cea_model_bia(pop_50_54,c_sex_p,trans_zero_51_65,trans_sex_51_65,i,j,k,cvd_50_54)
      res_sex_55_59=cea_model_bia(pop_55_59,c_sex_p,trans_zero_56_70,trans_sex_56_70,i,j,k,cvd_55_59)
      res_sex_60_64=cea_model_bia(pop_60_64,c_sex_p,trans_zero_61_75,trans_sex_61_75,i,j,k,cvd_60_64)
      res_sex_65_69=cea_model_bia(pop_65_69,c_sex_p,trans_zero_66_80,trans_sex_66_80,i,j,k,cvd_65_69)
      res_sex_70_74=cea_model_bia(pop_70_74,c_sex_p,trans_zero_71_85,trans_sex_71_85,i,j,k,cvd_70_74)
      res_sex_75_79=cea_model_bia(pop_75_79,c_sex_p,trans_zero_76_90,trans_sex_76_90,i,j,k,cvd_75_79)
      res_sex_80_84=cea_model_bia(pop_80_84,c_sex_p,trans_zero_81_95,trans_sex_81_95,i,j,k,cvd_80_84)
      res_sex_85_89=cea_model_bia(pop_85_89,c_sex_p,trans_zero_86_100,trans_sex_86_100,i,j,k,cvd_85_89)
      
# strategy      
      
      # temp_name=c(paste("duration=",i,",comp=",j,",cvd=",k,sep = ""),0,0)
      res_delta_45_49_temp=(res_sex_45_49+res_sex_50_54+res_sex_55_59+res_sex_60_64+res_sex_65_69+res_sex_70_74+res_sex_75_79+res_sex_80_84+res_sex_85_89)-
        (res_zero_45_49+res_zero_50_54+res_zero_55_59+res_zero_60_64+res_zero_65_69+res_zero_70_74+res_zero_75_79+res_zero_80_84+res_zero_85_89)
      res_delta_45_49_temp[,1]=seq(0,15,1)
      res_delta_45_49_temp[,4]=i
      res_delta_45_49_temp[,5]=j
      res_delta_45_49_temp[,6]=k
      colnames(res_delta_45_49_temp)=c("V1","V2","V3","V4","V5","V6")
      
      res_delta_50_54_temp=(res_sex_50_54+res_sex_55_59+res_sex_60_64+res_sex_65_69+res_sex_70_74+res_sex_75_79+res_sex_80_84+res_sex_85_89)-
        (res_zero_50_54+res_zero_55_59+res_zero_60_64+res_zero_65_69+res_zero_70_74+res_zero_75_79+res_zero_80_84+res_zero_85_89)
      res_delta_50_54_temp[,1]=seq(0,15,1)
      res_delta_50_54_temp[,4]=i
      res_delta_50_54_temp[,5]=j
      res_delta_50_54_temp[,6]=k
      colnames(res_delta_50_54_temp)=c("V1","V2","V3","V4","V5","V6")
      
      
      res_delta_55_59_temp=(res_sex_55_59+res_sex_60_64+res_sex_65_69+res_sex_70_74+res_sex_75_79+res_sex_80_84+res_sex_85_89)-
        (res_zero_55_59+res_zero_60_64+res_zero_65_69+res_zero_70_74+res_zero_75_79+res_zero_80_84+res_zero_85_89)
      res_delta_55_59_temp[,1]=seq(0,15,1)
      res_delta_55_59_temp[,4]=i
      res_delta_55_59_temp[,5]=j
      res_delta_55_59_temp[,6]=k
      colnames(res_delta_55_59_temp)=c("V1","V2","V3","V4","V5","V6")
      
      
      res_delta_60_64_temp=(res_sex_60_64+res_sex_65_69+res_sex_70_74+res_sex_75_79+res_sex_80_84+res_sex_85_89)-
        (res_zero_60_64+res_zero_65_69+res_zero_70_74+res_zero_75_79+res_zero_80_84+res_zero_85_89)
      res_delta_60_64_temp[,1]=seq(0,15,1)
      res_delta_60_64_temp[,4]=i
      res_delta_60_64_temp[,5]=j
      res_delta_60_64_temp[,6]=k
      colnames(res_delta_60_64_temp)=c("V1","V2","V3","V4","V5","V6")
      
      
      res_delta_45_49=rbind(res_delta_45_49,res_delta_45_49_temp)
      res_delta_50_54=rbind(res_delta_50_54,res_delta_50_54_temp)
      res_delta_55_59=rbind(res_delta_55_59,res_delta_55_59_temp)
      res_delta_60_64=rbind(res_delta_60_64,res_delta_60_64_temp)
    }
  }
}

write.csv(res_delta_45_49,file = "res_delta_45_49_BIA.csv",row.names = FALSE)
write.csv(res_delta_50_54,file = "res_delta_50_54_BIA.csv",row.names = FALSE)
write.csv(res_delta_55_59,file = "res_delta_55_59_BIA.csv",row.names = FALSE)
write.csv(res_delta_60_64,file = "res_delta_60_64_BIA.csv",row.names = FALSE)
# 
# res_delta<-rbind(res_delta_45_49,res_delta_50_54,res_delta_55_59,res_delta_60_64)
# write.csv(res_delta,file = "res_delta.csv")
