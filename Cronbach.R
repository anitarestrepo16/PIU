#set WD
setwd("J:/Healthy Brain Network/10. Staff Development/Research Projects/2018/Internalizing/R6 Project/Scripts")
#Call initial stuff script
source("Data_Cleaning.R")

#change URSI to ID
colnames(CIS_P)[1] <- "ID"
colnames(CIS_SR)[1] <- "ID"
colnames(SDS)[1] <- "ID"
colnames(PAQ_A)[1] <- "ID"
colnames(PAQ_C)[1] <- "ID"
colnames(IAT)[1] <- "ID"
colnames(PCIAT)[1] <- "ID"

#make dfs for each scale with just standard sample
CIS_P_Standard <- merge(Standard_Sample_P[,1:2], CIS_P[, 1:14], by.y = "ID", all = FALSE)
CIS_SR_Standard <- merge(Standard_Sample_SR[,1:2], CIS_SR[, 1:14], by.y = "ID", all.y = TRUE, all.x = FALSE)
SDS_Standard <- merge(Standard_Sample_P[,1:2], SDS[, 1:27], by.y = "ID", all.y = FALSE, all.x = FALSE)
PAQ_A_Standard <- merge(Standard_Sample_SR[,1:2], PAQ_A[, c(1, 29:42)], by.y = "ID", all.y = FALSE, all.x = FALSE)
PAQ_C_Standard <- merge(Standard_Sample_SR[,1:2], PAQ_C[, c(1, 29:44)], by.y = "ID", all.y = FALSE, all.x = FALSE)
IAT_Standard <- merge(Standard_Sample_SR[,1:2], IAT[, 1:21], by.y = "ID", all.y = FALSE, all.x = FALSE)
PCIAT_Standard <- merge(Standard_Sample_P[,1:2], PCIAT[, 1:21], by.y = "ID", all.y = FALSE, all.x = FALSE)

#calculate cronbach's alphas
library(psych)
alpha(CIS_P_Standard[, -c(1:2)])
alpha(CIS_SR_Standard[, -c(1:2)])
alpha(SDS_Standard[, -c(1:2)])
alpha(PAQ_A_Standard[, -c(1:2)])
alpha(PAQ_C_Standard[, -c(1:2)])
alpha(IAT_Standard[, -c(1:2)])
alpha(PCIAT_Standard[, -c(1:2)])
