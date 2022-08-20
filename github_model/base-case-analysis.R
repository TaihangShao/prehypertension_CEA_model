ICER_outcome<-function(){
  res_base_case<-as.data.frame(matrix(ncol = 4,nrow = 5))
  colnames(res_base_case)<-c("Costs","Lifeyears","QALYs","Trans")
  res_base_case[1,]<-cea_model_uc(tp_pre_uc)
  res_base_case[2,]<-cea_model_ls(tp_pre_ls)
  res_base_case[3,]<-cea_model_sex(tp_pre_Sex)
  res_base_case[4,]<-cea_model_rex(tp_pre_Rex)
  res_base_case[5,]<-cea_model_di(tp_pre_di)
  
  res_icer<-as.data.frame(matrix(ncol = 4,nrow = 6))
  colnames(res_icer)<-c("Name","delta_cost","delta_qalys","icer")
  res_icer$Name=c("icer_uc_ls","icer_sex_uc","icer_sex_ls","icer_rex_sex","icer_di_sex","icer_di_rex")
  res_icer$icer[1]=(res_base_case$Costs[2]-res_base_case$Costs[1])/(res_base_case$QALYs[2]-res_base_case$QALYs[1])
  res_icer$icer[2]=(res_base_case$Costs[3]-res_base_case$Costs[1])/(res_base_case$QALYs[3]-res_base_case$QALYs[1])
  res_icer$icer[3]=(res_base_case$Costs[3]-res_base_case$Costs[2])/(res_base_case$QALYs[3]-res_base_case$QALYs[2])
  res_icer$icer[4]=(res_base_case$Costs[4]-res_base_case$Costs[3])/(res_base_case$QALYs[4]-res_base_case$QALYs[3])
  res_icer$icer[5]=(res_base_case$Costs[5]-res_base_case$Costs[3])/(res_base_case$QALYs[5]-res_base_case$QALYs[3])
  res_icer$icer[6]=(res_base_case$Costs[5]-res_base_case$Costs[4])/(res_base_case$QALYs[5]-res_base_case$QALYs[4])
 
  res_icer$delta_cost[1]=res_base_case$Costs[2]-res_base_case$Costs[1]
  res_icer$delta_cost[2]=res_base_case$Costs[3]-res_base_case$Costs[1]
  res_icer$delta_cost[3]=res_base_case$Costs[3]-res_base_case$Costs[2]
  res_icer$delta_cost[4]=res_base_case$Costs[4]-res_base_case$Costs[3]
  res_icer$delta_cost[5]=res_base_case$Costs[5]-res_base_case$Costs[3]
  res_icer$delta_cost[6]=res_base_case$Costs[5]-res_base_case$Costs[4]

  res_icer$delta_qalys[1]=res_base_case$QALYs[2]-res_base_case$QALYs[1]
  res_icer$delta_qalys[2]=res_base_case$QALYs[3]-res_base_case$QALYs[1]  
  res_icer$delta_qalys[3]=res_base_case$QALYs[3]-res_base_case$QALYs[2] 
  res_icer$delta_qalys[4]=res_base_case$QALYs[4]-res_base_case$QALYs[3]
  res_icer$delta_qalys[5]=res_base_case$QALYs[5]-res_base_case$QALYs[3]  
  res_icer$delta_qalys[6]=res_base_case$QALYs[5]-res_base_case$QALYs[4] 
  
  res=list(res_base_case,res_icer)
  
  return(res)
}

