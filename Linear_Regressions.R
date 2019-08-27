#set WD
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/R6 Project/Scripts")
#Call initial stuff script
source("Initial_Stuff.R")

###################### Linear Dx Regressions ##############

#### run regressions for IAT (Dx)
#unadjusted
Reg_Dx_SR_unadjusted_ASD <- lm(IAT_SR ~ ASD, data = Standard_Sample_SR)
Reg_Dx_SR_unadjusted_Anxiety <- lm(IAT_SR ~ Anxiety, data = Standard_Sample_SR)
Reg_Dx_SR_unadjusted_Depression <- lm(IAT_SR ~ Depression, data = Standard_Sample_SR)
Reg_Dx_SR_unadjusted_ADHD_Combined <- lm(IAT_SR ~ ADHD_Combined, data = Standard_Sample_SR)
Reg_Dx_SR_unadjusted_ADHD_Inattentive <- lm(IAT_SR ~ ADHD_Inattentive, data = Standard_Sample_SR)
Reg_Dx_SR_unadjusted_Social_Anxiety <- lm(IAT_SR ~ Social_Anxiety, data = Standard_Sample_SR)
#combine B, CI, and p into one dataframe for each dx
DF_ASD <- data.frame(
  round(Reg_Dx_SR_unadjusted_ASD$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_unadjusted_ASD)[2, 1], digits = 2), round(confint(Reg_Dx_SR_unadjusted_ASD)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_unadjusted_ASD)$coefficients[2, 4], digits = 2)) 
names(DF_ASD) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Anxiety <- data.frame(
  round(Reg_Dx_SR_unadjusted_Anxiety$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_unadjusted_Anxiety)[2, 1], digits = 2), round(confint(Reg_Dx_SR_unadjusted_Anxiety)[2, 2], digits = 2),
  round(summary(Reg_Dx_SR_unadjusted_Anxiety)$coefficients[2, 4], digits = 2)) 
names(DF_Anxiety) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Depression <- data.frame(
  round(Reg_Dx_SR_unadjusted_Depression$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_unadjusted_Depression)[2, 1], digits = 2), round(confint(Reg_Dx_SR_unadjusted_Depression)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_unadjusted_Depression)$coefficients[2, 4], digits = 2)) 
names(DF_Depression) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_ADHD_Combined <- data.frame(
  round(Reg_Dx_SR_unadjusted_ADHD_Combined$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_unadjusted_ADHD_Combined)[2, 1], digits = 2), round(confint(Reg_Dx_SR_unadjusted_ADHD_Combined)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_unadjusted_ADHD_Combined)$coefficients[2, 4], digits = 2)) 
names(DF_ADHD_Combined) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_ADHD_Inattentive <- data.frame(
  round(Reg_Dx_SR_unadjusted_ADHD_Inattentive$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_unadjusted_ADHD_Inattentive)[2, 1], digits = 2), round(confint(Reg_Dx_SR_unadjusted_ADHD_Inattentive)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_unadjusted_ADHD_Inattentive)$coefficients[2, 4], digits = 2)) 
names(DF_ADHD_Inattentive) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Social_Anxiety <- data.frame(
  round(Reg_Dx_SR_unadjusted_Social_Anxiety$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_unadjusted_Social_Anxiety)[2, 1], digits = 2), round(confint(Reg_Dx_SR_unadjusted_Social_Anxiety)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_unadjusted_Social_Anxiety)$coefficients[2, 4], digits = 2)) 
names(DF_Social_Anxiety) <- c("B", "2.5% CI", "97.5% CI", "p value")
#bind dataframes into one with all dxs for export
Reg_Dx_SR_unadjusted <- rbind(DF_ASD, DF_Anxiety, DF_Depression,
                              DF_ADHD_Combined, DF_ADHD_Inattentive, DF_Social_Anxiety)
#export as csv file
write.csv(Reg_Dx_SR_unadjusted, file = "Tables/Reg_Dx_SR_unadjusted.csv")

#adjusted for covariates
Reg_Dx_SR_adj_cov_ASD <- lm(IAT_SR ~ ASD + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_SR)
Reg_Dx_SR_adj_cov_Anxiety <- lm(IAT_SR ~ Anxiety + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_SR)
Reg_Dx_SR_adj_cov_Depression <- lm(IAT_SR ~ Depression + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_SR)
Reg_Dx_SR_adj_cov_ADHD_Combined <- lm(IAT_SR ~ ADHD_Combined + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_SR)
Reg_Dx_SR_adj_cov_ADHD_Inattentive <- lm(IAT_SR ~ ADHD_Inattentive + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_SR)
Reg_Dx_SR_adj_cov_Social_Anxiety <- lm(IAT_SR ~ Social_Anxiety + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_SR)
#combine B, CI, and p into one dataframe for each dx
DF_ASD <- data.frame(
  round(Reg_Dx_SR_adj_cov_ASD$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_adj_cov_ASD)[2, 1], digits = 2), round(confint(Reg_Dx_SR_adj_cov_ASD)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_adj_cov_ASD)$coefficients[2, 4], digits = 2)) 
names(DF_ASD) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Anxiety <- data.frame(
  round(Reg_Dx_SR_adj_cov_Anxiety$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_adj_cov_Anxiety)[2, 1], digits = 2), round(confint(Reg_Dx_SR_adj_cov_Anxiety)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_adj_cov_Anxiety)$coefficients[2, 4], digits = 2)) 
names(DF_Anxiety) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Depression <- data.frame(
  round(Reg_Dx_SR_adj_cov_Depression$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_adj_cov_Depression)[2, 1], digits = 2), round(confint(Reg_Dx_SR_adj_cov_Depression)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_adj_cov_Depression)$coefficients[2, 4], digits = 2)) 
names(DF_Depression) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_ADHD_Combined <- data.frame(
  round(Reg_Dx_SR_adj_cov_ADHD_Combined$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_adj_cov_ADHD_Combined)[2, 1], digits = 2), round(confint(Reg_Dx_SR_adj_cov_ADHD_Combined)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_adj_cov_ADHD_Combined)$coefficients[2, 4], digits = 2)) 
names(DF_ADHD_Combined) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_ADHD_Inattentive <- data.frame(
  round(Reg_Dx_SR_adj_cov_ADHD_Inattentive$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_adj_cov_ADHD_Inattentive)[2, 1], digits = 2), round(confint(Reg_Dx_SR_adj_cov_ADHD_Inattentive)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_adj_cov_ADHD_Inattentive)$coefficients[2, 4], digits = 2)) 
names(DF_ADHD_Inattentive) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Social_Anxiety <- data.frame(
  round(Reg_Dx_SR_adj_cov_Social_Anxiety$coefficients[2], digits = 2), round(confint(Reg_Dx_SR_adj_cov_Social_Anxiety)[2, 1], digits = 2), round(confint(Reg_Dx_SR_adj_cov_Social_Anxiety)[2, 2], digits = 2), 
  round(summary(Reg_Dx_SR_adj_cov_Social_Anxiety)$coefficients[2, 4], digits = 2)) 
names(DF_Social_Anxiety) <- c("B", "2.5% CI", "97.5% CI", "p value")
#bind dataframes into one with all dxs for export
Reg_Dx_SR_adj_cov <- rbind(DF_ASD, DF_Anxiety, DF_Depression,
                              DF_ADHD_Combined, DF_ADHD_Inattentive, DF_Social_Anxiety)
#export as csv file
write.csv(Reg_Dx_SR_adj_cov, file = "Tables/Reg_Dx_SR_adj_cov.csv")

#adjusted for covariates + all dxs
Reg_Dx_SR_Adjusted_all <- lm(IAT_SR ~ ASD + Anxiety + Depression +
                               ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                               Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_SR)
Reg_Dx_SR_Adjusted <- data.frame(
  round(Reg_Dx_SR_Adjusted_all$coefficients[2:7], digits = 2), round(confint(Reg_Dx_SR_Adjusted_all)[2:7, 1], digits = 2),
  round(confint(Reg_Dx_SR_Adjusted_all)[2:7, 2], digits = 2), 
  round(summary(Reg_Dx_SR_Adjusted_all)$coefficients[2:7, 4], digits = 2)) 
names(Reg_Dx_SR_Adjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
#export as csv file
write.csv(Reg_Dx_SR_Adjusted, file = "Tables/Reg_Dx_SR_Adjusted.csv")

#regression for PCIAT (Dx)

#unadjusted
Reg_Dx_P_unadjusted_ASD <- lm(IAT_Parent ~ ASD, data = Standard_Sample_P)
Reg_Dx_P_unadjusted_Anxiety <- lm(IAT_Parent ~ Anxiety, data = Standard_Sample_P)
Reg_Dx_P_unadjusted_Depression <- lm(IAT_Parent ~ Depression, data = Standard_Sample_P)
Reg_Dx_P_unadjusted_ADHD_Combined <- lm(IAT_Parent ~ ADHD_Combined, data = Standard_Sample_P)
Reg_Dx_P_unadjusted_ADHD_Inattentive <- lm(IAT_Parent ~ ADHD_Inattentive, data = Standard_Sample_P)
Reg_Dx_P_unadjusted_Social_Anxiety <- lm(IAT_Parent ~ Social_Anxiety, data = Standard_Sample_P)
#combine B, CI, and p into one dataframe for each dx
DF_ASD <- data.frame(
  round(Reg_Dx_P_unadjusted_ASD$coefficients[2], digits = 2), round(confint(Reg_Dx_P_unadjusted_ASD)[2, 1], digits = 2), round(confint(Reg_Dx_P_unadjusted_ASD)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_unadjusted_ASD)$coefficients[2, 4], digits = 2)) 
names(DF_ASD) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Anxiety <- data.frame(
  round(Reg_Dx_P_unadjusted_Anxiety$coefficients[2], digits = 2), round(confint(Reg_Dx_P_unadjusted_Anxiety)[2, 1], digits = 2), round(confint(Reg_Dx_P_unadjusted_Anxiety)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_unadjusted_Anxiety)$coefficients[2, 4], digits = 2)) 
names(DF_Anxiety) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Depression <- data.frame(
  round(Reg_Dx_P_unadjusted_Depression$coefficients[2], digits = 2), round(confint(Reg_Dx_P_unadjusted_Depression)[2, 1], digits = 2), round(confint(Reg_Dx_P_unadjusted_Depression)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_unadjusted_Depression)$coefficients[2, 4], digits = 2)) 
names(DF_Depression) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_ADHD_Combined <- data.frame(
  round(Reg_Dx_P_unadjusted_ADHD_Combined$coefficients[2], digits = 2), round(confint(Reg_Dx_P_unadjusted_ADHD_Combined)[2, 1], digits = 2), round(confint(Reg_Dx_P_unadjusted_ADHD_Combined)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_unadjusted_ADHD_Combined)$coefficients[2, 4], digits = 2)) 
names(DF_ADHD_Combined) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_ADHD_Inattentive <- data.frame(
  round(Reg_Dx_P_unadjusted_ADHD_Inattentive$coefficients[2], digits = 2), round(confint(Reg_Dx_P_unadjusted_ADHD_Inattentive)[2, 1], digits = 2), round(confint(Reg_Dx_P_unadjusted_ADHD_Inattentive)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_unadjusted_ADHD_Inattentive)$coefficients[2, 4], digits = 2)) 
names(DF_ADHD_Inattentive) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Social_Anxiety <- data.frame(
  round(Reg_Dx_P_unadjusted_Social_Anxiety$coefficients[2], digits = 2), round(confint(Reg_Dx_P_unadjusted_Social_Anxiety)[2, 1], digits = 2), round(confint(Reg_Dx_P_unadjusted_Social_Anxiety)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_unadjusted_Social_Anxiety)$coefficients[2, 4], digits = 2)) 
names(DF_Social_Anxiety) <- c("B", "2.5% CI", "97.5% CI", "p value")
#bind dataframes into one with all dxs for export
Reg_Dx_P_unadjusted <- rbind(DF_ASD, DF_Anxiety, DF_Depression,
                              DF_ADHD_Combined, DF_ADHD_Inattentive, DF_Social_Anxiety)
#export as csv file
write.csv(Reg_Dx_P_unadjusted, file = "Tables/Reg_Dx_P_unadjusted.csv")

#adjusted for covariates
Reg_Dx_P_adj_cov_ASD <- lm(IAT_Parent ~ ASD + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_P)
Reg_Dx_P_adj_cov_Anxiety <- lm(IAT_Parent ~ Anxiety + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_P)
Reg_Dx_P_adj_cov_Depression <- lm(IAT_Parent ~ Depression + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_P)
Reg_Dx_P_adj_cov_ADHD_Combined <- lm(IAT_Parent ~ ADHD_Combined + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_P)
Reg_Dx_P_adj_cov_ADHD_Inattentive <- lm(IAT_Parent ~ ADHD_Inattentive + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_P)
Reg_Dx_P_adj_cov_Social_Anxiety <- lm(IAT_Parent ~ Social_Anxiety + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_P)
#combine B, CI, and p into one dataframe for each dx
DF_ASD <- data.frame(
  round(Reg_Dx_P_adj_cov_ASD$coefficients[2], digits = 2), round(confint(Reg_Dx_P_adj_cov_ASD)[2, 1], digits = 2), round(confint(Reg_Dx_P_adj_cov_ASD)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_adj_cov_ASD)$coefficients[2, 4], digits = 2)) 
names(DF_ASD) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Anxiety <- data.frame(
  round(Reg_Dx_P_adj_cov_Anxiety$coefficients[2], digits = 2), round(confint(Reg_Dx_P_adj_cov_Anxiety)[2, 1], digits = 2), round(confint(Reg_Dx_P_adj_cov_Anxiety)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_adj_cov_Anxiety)$coefficients[2, 4], digits = 2)) 
names(DF_Anxiety) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Depression <- data.frame(
  round(Reg_Dx_P_adj_cov_Depression$coefficients[2], digits = 2), round(confint(Reg_Dx_P_adj_cov_Depression)[2, 1], digits = 2), round(confint(Reg_Dx_P_adj_cov_Depression)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_adj_cov_Depression)$coefficients[2, 4], digits = 2)) 
names(DF_Depression) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_ADHD_Combined <- data.frame(
  round(Reg_Dx_P_adj_cov_ADHD_Combined$coefficients[2], digits = 2), round(confint(Reg_Dx_P_adj_cov_ADHD_Combined)[2, 1], digits = 2), round(confint(Reg_Dx_P_adj_cov_ADHD_Combined)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_adj_cov_ADHD_Combined)$coefficients[2, 4], digits = 2)) 
names(DF_ADHD_Combined) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_ADHD_Inattentive <- data.frame(
  round(Reg_Dx_P_adj_cov_ADHD_Inattentive$coefficients[2], digits = 2), round(confint(Reg_Dx_P_adj_cov_ADHD_Inattentive)[2, 1], digits = 2), round(confint(Reg_Dx_P_adj_cov_ADHD_Inattentive)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_adj_cov_ADHD_Inattentive)$coefficients[2, 4], digits = 2)) 
names(DF_ADHD_Inattentive) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Social_Anxiety <- data.frame(
  round(Reg_Dx_P_adj_cov_Social_Anxiety$coefficients[2], digits = 2), round(confint(Reg_Dx_P_adj_cov_Social_Anxiety)[2, 1], digits = 2), round(confint(Reg_Dx_P_adj_cov_Social_Anxiety)[2, 2], digits = 2), 
  round(summary(Reg_Dx_P_adj_cov_Social_Anxiety)$coefficients[2, 4], digits = 2)) 
names(DF_Social_Anxiety) <- c("B", "2.5% CI", "97.5% CI", "p value")
#bind dataframes into one with all dxs for export
Reg_Dx_P_adj_cov <- rbind(DF_ASD, DF_Anxiety, DF_Depression,
                              DF_ADHD_Combined, DF_ADHD_Inattentive, DF_Social_Anxiety)
#export as csv file
write.csv(Reg_Dx_P_adj_cov, file = "Tables/Reg_Dx_P_adj_cov.csv")

#adjusted for covariates + all dxs
Reg_Dx_P_Adjusted_all <- lm(IAT_Parent ~ ASD + Anxiety + Depression +
                               ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                               Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_P)
Reg_Dx_P_Adjusted <- data.frame(
  round(Reg_Dx_P_Adjusted_all$coefficients[2:7], digits = 2), round(confint(Reg_Dx_P_Adjusted_all)[2:7, 1], digits = 2),
  round(confint(Reg_Dx_P_Adjusted_all)[2:7, 2], digits = 2), 
  round(summary(Reg_Dx_P_Adjusted_all)$coefficients[2:7, 4], digits = 2)) 
names(Reg_Dx_P_Adjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
#export as csv file
write.csv(Reg_Dx_P_Adjusted, file = "Tables/Reg_Dx_P_Adjusted.csv")


########### Dimensional Q's Regression
#pull out data of interest
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

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  SES = Barratt$Barratt_Total,
  Single_Caregiver = Barratt$No_C2)

ASSQ_of_interest <- data.frame(
  ID = ASSQ$URSI,
  ASSQ_Total = ASSQ$ASSQ_Total)

Conners_of_interest <- data.frame(
  ID = C3SR$URSI,
  Conners_Hyperactivity_Impulsivity = C3SR$C3SR_HY_T,
  Conners_Inattention = C3SR$C3SR_IN_T)

SWAN_of_interest <- data.frame(
  ID = SWAN$URSI,
  SWAN_Inattentive = SWAN$SWAN_IN,
  SWAN_Hyperactive = SWAN$SWAN_HY)

SCARED_SR_of_interest <- data.frame(
  ID = SCARED_SR$URSI,
  Total_Anxiety_SR = SCARED_SR$SCARED_SR_Total)

SCARED_P_of_interest <- data.frame(
  ID = SCARED_P$URSI,
  Total_Anxiety_P = SCARED_P$SCARED_P_Total)

MFQ_SR_of_interest <- data.frame(
  ID = MFQ_SR$URSI,
  Depression_SR = MFQ_SR$MFQ_SR_Total)

MFQ_P_of_interest <- data.frame(
  ID = MFQ_P$URSI,
  Depression_P = MFQ_P$MFQ_P_Total)

#dataframe for dimensional q's regression
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Dimensional_Qs_SR <- Reduce(Merge, list(IAT_of_interest,
                                        Conners_of_interest, SCARED_SR_of_interest, MFQ_SR_of_interest, 
                                        Demos_of_interest, Barratt_of_interest))
Dimensional_Qs_P <- Reduce(Merge, list(PCIAT_of_interest, ASSQ_of_interest, 
                                       SWAN_of_interest, SCARED_P_of_interest,
                                        MFQ_P_of_interest, Demos_of_interest, Barratt_of_interest))
#remove all NAs
Dimensional_Qs_SR <- Dimensional_Qs_SR[complete.cases(Dimensional_Qs_SR), ]
Dimensional_Qs_P <- Dimensional_Qs_P[complete.cases(Dimensional_Qs_P), ]

#regressions for IAT (Q's)

#unadjusted
Reg_Q_SR_unadjusted_Conners_Hy_Im <- lm(IAT_SR ~ Conners_Hyperactivity_Impulsivity, data = Dimensional_Qs_SR)
Reg_Q_SR_unadjusted_Conners_In <- lm(IAT_SR ~ Conners_Inattention, data = Dimensional_Qs_SR)
Reg_Q_SR_unadjusted_SCARED <- lm(IAT_SR ~ Total_Anxiety_SR, data = Dimensional_Qs_SR)
Reg_Q_SR_unadjusted_MFQ <- lm(IAT_SR ~ Depression_SR, data = Dimensional_Qs_SR)
#combine B, CI, and p into one dataframe for each Q
DF_Conners_Hy_Im <- data.frame(
  round(Reg_Q_SR_unadjusted_Conners_Hy_Im$coefficients[2], digits = 2), round(confint(Reg_Q_SR_unadjusted_Conners_Hy_Im)[2, 1], digits = 2),
  round(confint(Reg_Q_SR_unadjusted_Conners_Hy_Im)[2, 2], digits = 2), 
  round(summary(Reg_Q_SR_unadjusted_Conners_Hy_Im)$coefficients[2, 4], digits = 2)) 
names(DF_Conners_Hy_Im) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Conners_In <- data.frame(
  round(Reg_Q_SR_unadjusted_Conners_In$coefficients[2], digits = 2), round(confint(Reg_Q_SR_unadjusted_Conners_In)[2, 1], digits =2),
  round(confint(Reg_Q_SR_unadjusted_Conners_In)[2, 2], digits = 2), 
  round(summary(Reg_Q_SR_unadjusted_Conners_In)$coefficients[2, 4], digits = 2)) 
names(DF_Conners_In) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SCARED <- data.frame(
  round(Reg_Q_SR_unadjusted_SCARED$coefficients[2], digits = 2), round(confint(Reg_Q_SR_unadjusted_SCARED)[2, 1], digits = 2), 
  round(confint(Reg_Q_SR_unadjusted_SCARED)[2, 2], digits = 2), 
  round(summary(Reg_Q_SR_unadjusted_SCARED)$coefficients[2, 4], digits = 2)) 
names(DF_SCARED) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_MFQ <- data.frame(
  round(Reg_Q_SR_unadjusted_MFQ$coefficients[2], digits = 2), round(confint(Reg_Q_SR_unadjusted_MFQ)[2, 1], digits =2), 
  round(confint(Reg_Q_SR_unadjusted_MFQ)[2, 2], digits = 2), 
  round(summary(Reg_Q_SR_unadjusted_MFQ)$coefficients[2, 4], digits = 2)) 
names(DF_MFQ) <- c("B", "2.5% CI", "97.5% CI", "p value")
#bind dataframes into one with all Qs for export
Reg_Q_SR_unadjusted <- rbind(DF_Conners_Hy_Im, DF_Conners_In,
                              DF_SCARED, DF_MFQ)
#export as csv file
write.csv(Reg_Q_SR_unadjusted, file = "Tables/Reg_Q_SR_unadjusted.csv")

#adjusted for covariates
Reg_Q_SR_adj_cov_Conners_Hy_Im <- lm(IAT_SR ~ Conners_Hyperactivity_Impulsivity + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_SR)
Reg_Q_SR_adj_cov_Conners_In <- lm(IAT_SR ~ Conners_Inattention + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_SR)
Reg_Q_SR_adj_cov_SCARED <- lm(IAT_SR ~ Total_Anxiety_SR + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_SR)
Reg_Q_SR_adj_cov_MFQ <- lm(IAT_SR ~ Depression_SR + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_SR)
#combine B, CI, and p into one dataframe for each Q
DF_Conners_Hy_Im <- data.frame(
  round(Reg_Q_SR_adj_cov_Conners_Hy_Im$coefficients[2], digits = 2), round(confint(Reg_Q_SR_adj_cov_Conners_Hy_Im)[2, 1], digits = 2),
  round(confint(Reg_Q_SR_adj_cov_Conners_Hy_Im)[2, 2], digits = 2), 
  round(summary(Reg_Q_SR_adj_cov_Conners_Hy_Im)$coefficients[2, 4], digits = 2)) 
names(DF_Conners_Hy_Im) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Conners_In <- data.frame(
  round(Reg_Q_SR_adj_cov_Conners_In$coefficients[2], digits = 2), round(confint(Reg_Q_SR_adj_cov_Conners_In)[2, 1], digits = 2),
  round(confint(Reg_Q_SR_adj_cov_Conners_In)[2, 2], digits = 2), 
  round(summary(Reg_Q_SR_adj_cov_Conners_In)$coefficients[2, 4], digits = 2)) 
names(DF_Conners_In) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SCARED <- data.frame(
  round(Reg_Q_SR_adj_cov_SCARED$coefficients[2], digits = 2), round(confint(Reg_Q_SR_adj_cov_SCARED)[2, 1], digits = 2),
  round(confint(Reg_Q_SR_adj_cov_SCARED)[2, 2], digits = 2), 
  round(summary(Reg_Q_SR_adj_cov_SCARED)$coefficients[2, 4], digits = 2)) 
names(DF_SCARED) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_MFQ <- data.frame(
  round(Reg_Q_SR_adj_cov_MFQ$coefficients[2], digits = 2), round(confint(Reg_Q_SR_adj_cov_MFQ)[2, 1], digits = 2),
  round(confint(Reg_Q_SR_adj_cov_MFQ)[2, 2], digits = 2), 
  round(summary(Reg_Q_SR_adj_cov_MFQ)$coefficients[2, 4], digits = 2)) 
names(DF_MFQ) <- c("B", "2.5% CI", "97.5% CI", "p value")
#bind dataframes into one with all Qs for export
Reg_Q_SR_adj_cov <- rbind(DF_Conners_Hy_Im, DF_Conners_In, 
                              DF_SCARED, DF_MFQ)
#export as csv file
write.csv(Reg_Q_SR_adj_cov, file = "Tables/Reg_Q_SR_adj_cov.csv")

#adjusted for covariates + all Qs
Reg_Q_SR_Adjusted <- lm(IAT_SR ~ Conners_Hyperactivity_Impulsivity + Conners_Inattention + 
                Total_Anxiety_SR +
                Depression_SR + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_SR)
#put data into one df for export
DF_Reg_Q_SR_Adjusted <- data.frame(
  round(Reg_Q_SR_Adjusted$coefficients[2:5], digits = 2), round(confint(Reg_Q_SR_Adjusted)[2:5, 1], digits = 2),
  round(confint(Reg_Q_SR_Adjusted)[2:5, 2], digits = 2), 
  round(summary(Reg_Q_SR_Adjusted)$coefficients[2:5, 4], digits = 2)) 
names(DF_Reg_Q_SR_Adjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
#export as csv file
write.csv(DF_Reg_Q_SR_Adjusted, file = "Tables/Reg_Q_SR_Adjusted.csv")

#regression for PCIAT (Q's)

#unadjusted
Reg_Q_P_unadjusted_ASD <- lm(IAT_Parent ~ ASSQ_Total, data = Dimensional_Qs_P)
Reg_Q_P_unadjusted_SWAN_Inattentive <- lm(IAT_Parent ~ SWAN_Inattentive, data = Dimensional_Qs_P)
Reg_Q_P_unadjusted_SWAN_Hyperactive <- lm(IAT_Parent ~ SWAN_Hyperactive, data = Dimensional_Qs_P)
Reg_Q_P_unadjusted_SCARED <- lm(IAT_Parent ~ Total_Anxiety_P, data = Dimensional_Qs_P)
Reg_Q_P_unadjusted_MFQ <- lm(IAT_Parent ~ Depression_P, data = Dimensional_Qs_P)
#combine B, CI, and p into one dataframe for each Q
DF_ASD <- data.frame(
  round(Reg_Q_P_unadjusted_ASD$coefficients[2], digits = 2), round(confint(Reg_Q_P_unadjusted_ASD)[2, 1], digits = 2), round(confint(Reg_Q_P_unadjusted_ASD)[2, 2], digits = 2), 
  round(summary(Reg_Q_P_unadjusted_ASD)$coefficients[2, 4], digits = 2)) 
names(DF_ASD) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SWAN_Inattentive <- data.frame(
  round(Reg_Q_P_unadjusted_SWAN_Inattentive$coefficients[2], digits = 2), round(confint(Reg_Q_P_unadjusted_SWAN_Inattentive)[2, 1], digits = 2), round(confint(Reg_Q_P_unadjusted_SWAN_Inattentive)[2, 2], digits = 2), 
  round(summary(Reg_Q_P_unadjusted_SWAN_Inattentive)$coefficients[2, 4], digits = 2)) 
names(DF_SWAN_Inattentive) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SWAN_Hyperactive <- data.frame(
  round(Reg_Q_P_unadjusted_SWAN_Hyperactive$coefficients[2], digits = 2), round(confint(Reg_Q_P_unadjusted_SWAN_Hyperactive)[2, 1], digits = 2), round(confint(Reg_Q_P_unadjusted_SWAN_Hyperactive)[2, 2], digits = 2), 
  round(summary(Reg_Q_P_unadjusted_SWAN_Hyperactive)$coefficients[2, 4], digits = 2)) 
names(DF_SWAN_Hyperactive) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SCARED <- data.frame(
  round(Reg_Q_P_unadjusted_SCARED$coefficients[2], digits = 2), round(confint(Reg_Q_P_unadjusted_SCARED)[2, 1], digits = 2),
  round(confint(Reg_Q_P_unadjusted_SCARED)[2, 2], digits = 2), 
  round(summary(Reg_Q_P_unadjusted_SCARED)$coefficients[2, 4], digits = 2)) 
names(DF_SCARED) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_MFQ <- data.frame(
  round(Reg_Q_P_unadjusted_MFQ$coefficients[2], digits = 2), round(confint(Reg_Q_P_unadjusted_MFQ)[2, 1], digits = 2),
  round(confint(Reg_Q_P_unadjusted_MFQ)[2, 2], digits = 2), 
  round(summary(Reg_Q_P_unadjusted_MFQ)$coefficients[2, 4], digits = 2))
names(DF_MFQ) <- c("B", "2.5% CI", "97.5% CI", "p value")
#bind dataframes into one with all Qs for export
Reg_Q_P_unadjusted <- rbind(DF_ASD, DF_SWAN_Inattentive, DF_SWAN_Hyperactive,
                              DF_SCARED, DF_MFQ)
#export as csv file
write.csv(Reg_Q_P_unadjusted, file = "Tables/Reg_Q_P_unadjusted.csv")

#adjusted for covariates
Reg_Q_P_adj_cov_ASD <- lm(IAT_Parent ~ ASSQ_Total + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_P)
Reg_Q_P_adj_cov_SWAN_Inattentive <- lm(IAT_Parent ~ SWAN_Inattentive + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_P)
Reg_Q_P_adj_cov_SWAN_Hyperactive <- lm(IAT_Parent ~ SWAN_Hyperactive + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_P)
Reg_Q_P_adj_cov_SCARED <- lm(IAT_Parent ~ Total_Anxiety_P + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_P)
Reg_Q_P_adj_cov_MFQ <- lm(IAT_Parent ~ Depression_P + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_P)
#combine B, CI, and p into one dataframe for each Q
DF_ASD <- data.frame(
  round(Reg_Q_P_adj_cov_ASD$coefficients[2], digits = 2), round(confint(Reg_Q_P_adj_cov_ASD)[2, 1], digits = 2), round(confint(Reg_Q_P_adj_cov_ASD)[2, 2], digits = 2), 
  round(summary(Reg_Q_P_adj_cov_ASD)$coefficients[2, 4], digits = 2)) 
names(DF_ASD) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SWAN_Inattentive <- data.frame(
  round(Reg_Q_P_adj_cov_SWAN_Inattentive$coefficients[2], digits = 2), round(confint(Reg_Q_P_adj_cov_SWAN_Inattentive)[2, 1], digits = 2), round(confint(Reg_Q_P_adj_cov_SWAN_Inattentive)[2, 2], digits = 2), 
  round(summary(Reg_Q_P_adj_cov_SWAN_Inattentive)$coefficients[2, 4], digits = 2)) 
names(DF_SWAN_Inattentive) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SWAN_Hyperactive <- data.frame(
  round(Reg_Q_P_adj_cov_SWAN_Hyperactive$coefficients[2], digits = 2), round(confint(Reg_Q_P_adj_cov_SWAN_Hyperactive)[2, 1], digits = 2), round(confint(Reg_Q_P_adj_cov_SWAN_Hyperactive)[2, 2], digits = 2), 
  round(summary(Reg_Q_P_adj_cov_SWAN_Hyperactive)$coefficients[2, 4], digits = 2)) 
names(DF_SWAN_Hyperactive) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SCARED <- data.frame(
  round(Reg_Q_P_adj_cov_SCARED$coefficients[2], digits = 2), round(confint(Reg_Q_P_adj_cov_SCARED)[2, 1], digits = 2), round(confint(Reg_Q_P_adj_cov_SCARED)[2, 2], digits = 2), 
  round(summary(Reg_Q_P_adj_cov_SCARED)$coefficients[2, 4], digits = 2)) 
names(DF_SCARED) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_MFQ <- data.frame(
  round(Reg_Q_P_adj_cov_MFQ$coefficients[2], digits = 2), round(confint(Reg_Q_P_adj_cov_MFQ)[2, 1], digits = 2), round(confint(Reg_Q_P_adj_cov_MFQ)[2, 2], digits = 2), 
  round(summary(Reg_Q_P_adj_cov_MFQ)$coefficients[2, 4], digits = 2)) 
names(DF_MFQ) <- c("B", "2.5% CI", "97.5% CI", "p value")
#bind dataframes into one with all Qs for export
Reg_Q_P_adj_cov <- rbind(DF_ASD, DF_SWAN_Inattentive, DF_SWAN_Hyperactive,
                              DF_SCARED, DF_MFQ)
#export as csv file
write.csv(Reg_Q_P_adj_cov, file = "Tables/Reg_Q_P_adj_cov.csv")

#adjusted for covariates + all Qs
Reg_Q_P_Adjusted <- lm(IAT_Parent ~ ASSQ_Total +
               SWAN_Inattentive + SWAN_Hyperactive + Total_Anxiety_P +
               Depression_P + Sex + Age + SES + Site + Single_Caregiver, data = Dimensional_Qs_P)
#put data into one df for export
DF_Reg_Q_P_Adjusted <- data.frame(
  round(Reg_Q_P_Adjusted$coefficients[2:6], digits = 2), round(confint(Reg_Q_P_Adjusted)[2:6, 1], digits = 2),
  round(confint(Reg_Q_P_Adjusted)[2:6, 2], digits = 2), 
  round(summary(Reg_Q_P_Adjusted)$coefficients[2:6, 4], digits = 2)) 
names(DF_Reg_Q_P_Adjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
#export as csv file
write.csv(DF_Reg_Q_P_Adjusted, file = "Tables/Reg_Q_P_Adjusted.csv")

############## Coping Regressions
#pull out data of interest
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

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  SES = Barratt$Barratt_Total)

CCSC_of_interest <- data.frame(
  ID = CCSC$URSI,
  Problem_Focused_Coping = CCSC$CCSC_PFC,
  Avoidance_Coping = CCSC$CCSC_AC,
  Positive_Cognitive_Restructuring = CCSC$CCSC_PCR,
  Religion = CCSC$CCSC_REL,
  Support_Seeking = CCSC$CCSC_SS)

#dataframe for regression with coping and IAT/PCIAT
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Reg_CCSC <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, CCSC_of_interest, Dx_of_interest))

# Regression with overarching subscales of CCSC predicting IAT_SR
#unadjusted
Reg_CCSC_SR_unadjusted <- lm(IAT_SR ~ Problem_Focused_Coping + Avoidance_Coping + Positive_Cognitive_Restructuring +
                               Religion + Support_Seeking, data = Reg_CCSC)
summary(Reg_CCSC_SR_unadjusted)
confint(Reg_CCSC_SR_unadjusted)
#adjusted for covariates
Reg_CCSC_SR_adj_cov <- lm(IAT_SR ~ Problem_Focused_Coping + Avoidance_Coping + Positive_Cognitive_Restructuring + Religion + Support_Seeking +
                  Sex + Age + SES + Site, data = Reg_CCSC)
summary(Reg_CCSC_SR_adj_cov)
confint(Reg_CCSC_SR_adj_cov)
#adjusted for covariates and all dxs
Reg_CCSC_SR_adjusted <- lm(IAT_SR ~ Problem_Focused_Coping + Avoidance_Coping + Positive_Cognitive_Restructuring +
                             Religion + Support_Seeking + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression +
                             ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive + Social_Anxiety, data = Reg_CCSC)
summary(Reg_CCSC_SR_adjusted)
confint(Reg_CCSC_SR_adjusted)

# Regression with overarching subscales of CCSC predicting PCIAT
#unadjusted
Reg_CCSC_P_unadjusted <- lm(IAT_Parent ~ Problem_Focused_Coping + Avoidance_Coping + Positive_Cognitive_Restructuring +
                               Religion + Support_Seeking, data = Reg_CCSC)
summary(Reg_CCSC_P_unadjusted)
confint(Reg_CCSC_P_unadjusted)
#adjusted for covariates
Reg_CCSC_P_adj_cov <- lm(IAT_Parent ~ Problem_Focused_Coping + Avoidance_Coping + Positive_Cognitive_Restructuring + Religion + Support_Seeking +
                  Sex + Age + SES + Site, data = Reg_CCSC)
summary(Reg_CCSC_P_adj_cov)
confint(Reg_CCSC_P_adj_cov)
#adjusted for covariates and all dxs
Reg_CCSC_P_adjusted <- lm(IAT_Parent ~ Problem_Focused_Coping + Avoidance_Coping + Positive_Cognitive_Restructuring +
                             Religion + Support_Seeking + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression +
                             ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive + Social_Anxiety, data = Reg_CCSC)
summary(Reg_CCSC_P_adjusted)
confint(Reg_CCSC_P_adjusted)
