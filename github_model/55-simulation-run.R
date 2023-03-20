
ICER_outcome_new<-function(){
  res_base_case<-as.data.frame(matrix(ncol = 4,nrow = 4))
  colnames(res_base_case)<-c("Costs","Lifeyears","QALYs","Trans")
  res_base_case[1,]<-cea_model_zero(tp_pre_zero)
  res_base_case[2,]<-cea_model_uc(tp_pre_uc)
  res_base_case[3,]<-cea_model_ls(tp_pre_ls)
  res_base_case[4,]<-cea_model_sex(tp_pre_sex)

  
  res_icer<-as.data.frame(matrix(ncol = 5,nrow = 6))
  colnames(res_icer)<-c("Year","Name","delta_cost","delta_qalys","icer")
  res_icer$Year=horizon
  res_icer$Name=c("icer_zero_uc","icer_zero_ls","icer_zero_sex","icer_uc_ls","icer_uc_sex","icer_ls_sex")
  res_icer$icer[1]=(res_base_case$Costs[2]-res_base_case$Costs[1])/(res_base_case$QALYs[2]-res_base_case$QALYs[1])
  res_icer$icer[2]=(res_base_case$Costs[3]-res_base_case$Costs[1])/(res_base_case$QALYs[3]-res_base_case$QALYs[1])
  res_icer$icer[3]=(res_base_case$Costs[4]-res_base_case$Costs[1])/(res_base_case$QALYs[4]-res_base_case$QALYs[1])
  res_icer$icer[4]=(res_base_case$Costs[3]-res_base_case$Costs[2])/(res_base_case$QALYs[3]-res_base_case$QALYs[2])
  res_icer$icer[5]=(res_base_case$Costs[4]-res_base_case$Costs[2])/(res_base_case$QALYs[4]-res_base_case$QALYs[2])
  res_icer$icer[6]=(res_base_case$Costs[4]-res_base_case$Costs[3])/(res_base_case$QALYs[4]-res_base_case$QALYs[3])

  res_icer$delta_cost[1]=res_base_case$Costs[2]-res_base_case$Costs[1]
  res_icer$delta_cost[2]=res_base_case$Costs[3]-res_base_case$Costs[1]
  res_icer$delta_cost[3]=res_base_case$Costs[4]-res_base_case$Costs[1]
  res_icer$delta_cost[4]=res_base_case$Costs[3]-res_base_case$Costs[2]
  res_icer$delta_cost[5]=res_base_case$Costs[4]-res_base_case$Costs[2]
  res_icer$delta_cost[6]=res_base_case$Costs[4]-res_base_case$Costs[3]

  res_icer$delta_qalys[1]=res_base_case$QALYs[2]-res_base_case$QALYs[1]
  res_icer$delta_qalys[2]=res_base_case$QALYs[3]-res_base_case$QALYs[1]
  res_icer$delta_qalys[3]=res_base_case$QALYs[4]-res_base_case$QALYs[1]  
  res_icer$delta_qalys[4]=res_base_case$QALYs[3]-res_base_case$QALYs[2] 
  res_icer$delta_qalys[5]=res_base_case$QALYs[4]-res_base_case$QALYs[2]
  res_icer$delta_qalys[6]=res_base_case$QALYs[4]-res_base_case$QALYs[3]  

  res=list(res_base_case,res_icer)
  
  return(res[[2]])
}

source("Param-input-base and DSA.R")
source("pre-cea-model.R")

# base case analysis results
res_final<-as.data.frame(matrix(ncol = 5,nrow = 6*55))
colnames(res_final)<-c("Year","Name","delta_cost","delta_qalys","icer")
for (i in 1:55) {
  horizon=i
  result_bc<-ICER_outcome_new()
  res_final[(6*(i-1)+1):(6*i),]=result_bc
}

write.csv(res_final,"res.final.csv",row.names = FALSE)

library("tidyverse")   # Mainly for plotting
library(ggsci)

ggplot(data = res_final)+geom_line(aes(x=Year,y=icer,colour=Name),size=1.5)+
  # labs(x = "WTP", y = "Probability", title = "CEAC") +
  scale_color_lancet()+
  scale_x_continuous(breaks = c(seq(from=0,to=55,by=1)))+
  # scale_y_continuous(breaks =c(seq(from=0,to=1,by=0.2)))+
  # theme(axis.title.x= element_text(size=20,color="black"))+
  # theme(axis.title.y= element_text(size=20,color="black"))+
  theme(panel.grid= element_blank())+
  theme(axis.line = element_line(colour = "black",size = 1))+
  # theme(axis.text.x= element_text(size=15, color="black"))+
  # theme(axis.text.y= element_text(size=15, color="black"))+
  # theme(title = element_text(size=20, color="black"))+
  theme(panel.background = element_blank())+
  theme(panel.border=element_blank())