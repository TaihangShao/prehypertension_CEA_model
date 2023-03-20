hyp_predict=function(age,sbp,dbp){
  #baseline parameters input
  #flexible input:age,sbp,dbp
  # age=100
  sbp=156.21
  dbp=92.48
  #Deterministic input
  sex=0.45
  BMI=24.99
  WBC=6.32
  FG=106.94
  UA=5.55
  family_history=0.404
  AF=0.0103
  TC=201.22
  DM=0.066
  smoking=0.3152
  CHD=0.0263
  LVH=0.046
  heart_rate=78
  VD=0.0040
  
  ##for prehypertension
  #prehyper to hypre
  # scale_h=0.037*(sex*2+(1-sex)*0)+	-0.01*age+	-0.036*BMI+	-0.028*sbp+	-0.013*dbp+	-0.035*WBC+	-0.001*FG+	-0.038*UA+ 8.604
  # risk_h_10=1-exp(-exp((log(10)-scale_h)/0.589))
  # prob_h=1-exp(-(-(log(1-risk_h_10))/10)*1)
  
  #prehyper to stroke
  scale_s=0.073*(age-54.6)+	-0.458*((sex*1+(1-sex)*2)-1.53)+	0.016*(sbp-125.1)+	0.017*(dbp-77)+
    0.457*(family_history-0.2)+	1.247*(AF-0.01)+	0.003*(TC-197.8)+	0.081*(WBC-6.3)+	0.005*(FG-109.9)
  risk_s_10=1-0.9783^exp(scale_s)
  prob_s=1-exp(-(-(log(1-risk_s_10))/10)*1)
  
  #prehyper to HF
  scale_hf=0.0412*age+	0.9026*LVH+	0.0166*heart_rate+	0.00804*sbp+	1.6079*CHD+	0.9714*VD+	0.2244*DM -9.2087
  risk_hf_4=1/(1+exp(-scale_hf))
  prob_hf=1-exp(-(-(log(1-risk_hf_4))/4)*1)
  
  #prehyper to CHD
  scale_m=0.040129*age+	0.0229901*sbp+	0.0105563*TC+	0.1269031*BMI+	0.8426276*smoking-7
  risk_m_10=1-(0.98^(1/exp(4.43)))^exp(scale_m)
  prob_m=1-exp(-(-(log(1-risk_m_10))/10)*1)
  
  return(c(prob_s,prob_hf,prob_m))
  
}









###
#information of reference detail see the manuscript
###
