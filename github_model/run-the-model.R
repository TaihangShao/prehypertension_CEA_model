# library(flexsurv)
# library(tidyverse)
# library(dplyr)
# library(ggplot2)

#########################
###   Run the model   ###
#########################

horizon=55

source("Param-input-base and DSA.R")

source("pre-cea-model.R")
source("base-case-analysis.R")

# base case analysis results
result_bc<-ICER_outcome()

# breakdown
source("break_down.R")
df1=cea_bd_zero(tp_pre_zero)
df2=rbind(df1,cea_bd_uc(tp_pre_uc))
df3=rbind(df2,cea_bd_ls(tp_pre_ls))
df4=rbind(df3,cea_bd_sex(tp_pre_sex))
df5=rbind(df4,cea_bd_rex(tp_pre_rex))
df6=rbind(df5,cea_bd_di(tp_pre_di))

# Scenario Analysis
u_h=0.6514
result_bc_s1<-ICER_outcome()
# write.csv(result_bc_s1[[1]],"result_bc_s11.csv",row.names = FALSE)
# write.csv(result_bc_s1[[2]],"result_bc_s12.csv",row.names = FALSE)

# #####################
# ###   DSA model   ###
# #####################
# 
# source("DSA_model.R")
# result_DSA<-res_dsa
# 
# #####################
# ###   PSA model   ###
# #####################
# 
# library(foreach)
# library(doParallel)
# 
# source("PSA_model.R")
# result_PSA<-Res_fe_psa

# Write results

write.csv(result_bc[[1]],"result_bc1.csv",row.names = FALSE)
write.csv(result_bc[[2]],"result_bc2.csv",row.names = FALSE)

write.csv(df6,"break_down.csv",row.names = FALSE)

# write.csv(result_DSA,"result_DSA.csv",row.names = FALSE)
# write.csv(result_PSA,"result_PSA.csv",row.names = FALSE)

