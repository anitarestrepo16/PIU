#set WD
setwd("J:/Healthy Brain Network/10. Staff Development/Research Projects/2018/Internalizing/R6 Project/Scripts")
#Call initial stuff script
source("Data_Cleaning.R")

########### make df with intersection of all samples -> final sample (NOT including dimensional measures of psychoapthology) #########
#pull out data of interest (Dx_of_interest already created and saved)
IAT_of_interest <- data.frame(
  ID = IAT$URSI,
  IAT_SR = IAT$IAT_Total)

PCIAT_of_interest <- data.frame(
  ID = PCIAT$URSI,
  IAT_Parent = PCIAT$PCIAT_Total)

Demos_of_interest <- data.frame(
  ID = Basic_Demos$URSI,
  Sex = Basic_Demos$Sex,
  Age = floor(Basic_Demos$Age),
  Site = Basic_Demos$Study_Site)

Race <- data.frame(
  ID = PreInt_Demos_Fam$URSI,
  Race = PreInt_Demos_Fam$Child_Race)

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  SES = Barratt$Barratt_Total,
  Single_Caregiver = Barratt$No_C2)

Physical_of_interest <- data.frame(
  ID = Physical$URSI,
  BMI = Physical$BMI)

BIA_of_interest <- data.frame(
  ID = BIA$URSI,
  FMI = BIA$FMI)

PAQ_A_of_interest <- data.frame(
  ID = PAQ_A$URSI,
  PAQ_Total = PAQ_A$PAQ_A_Total)

PAQ_C_of_interest <- data.frame(
  ID = PAQ_C$URSI,
  PAQ_Total = PAQ_C$PAQ_C_Total)

SDS_of_interest <- data.frame(
  ID = SDS$URSI,
  SDS_Total = SDS$SDS_Total_T)

CIS_P_of_interest <- data.frame(
  ID = CIS_P$URSI,
  CIS_P_Score = CIS_P$CIS_P_Score)

CIS_SR_of_interest <- data.frame(
  ID = CIS_SR$URSI,
  CIS_SR_Score = CIS_SR$CIS_SR_Total)

#merge PAQ dataframes
PAQ_of_interest <- rbind(PAQ_A_of_interest, PAQ_C_of_interest)

#construct final sample df for both Parent and SR
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Standard_Sample_SR <- Reduce(Merge, list(IAT_of_interest,
                                         Demos_of_interest, Race, Barratt_of_interest, 
                                         Physical_of_interest, 
                                         BIA_of_interest, PAQ_of_interest, SDS_of_interest, 
                                         CIS_SR_of_interest, Dx_of_interest))
Standard_Sample_P <- Reduce(Merge, list(PCIAT_of_interest,
                                        Demos_of_interest, Race, Barratt_of_interest,
                                        Physical_of_interest, BIA_of_interest, PAQ_of_interest, 
                                        SDS_of_interest, CIS_P_of_interest, Dx_of_interest))
#remove all Na's
Standard_Sample_SR <- Standard_Sample_SR[complete.cases(Standard_Sample_SR), ]
Standard_Sample_P <- Standard_Sample_P[complete.cases(Standard_Sample_P), ]
# save files
write.csv(Standard_Sample_SR, "Data/Standard_Sample_SR.csv", row.names = FALSE)
write.csv(Standard_Sample_P, "Data/Standard_Sample_P.csv", row.names = FALSE)
#read in files
Standard_Sample_SR <- read.csv("Data/Standard_Sample_SR.csv")
Standard_Sample_P <- read.csv("Data/Standard_Sample_P.csv")