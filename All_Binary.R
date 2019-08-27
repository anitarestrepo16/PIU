#set WD
setwd("J:/Healthy Brain Network/10. Staff Development/Research Projects/2018/Internalizing/R6 Project/Scripts")
#Call initial stuff script
source("Data_Cleaning.R")

#add column for PIU and non PIU
#create problematic (1) vs. non-problematic (0) based on score of 40 (Kim et al.)
Standard_Sample_SR$PIU = ifelse (Standard_Sample_SR$IAT_SR >=40, 1, 0)
Standard_Sample_P$PIU = ifelse (Standard_Sample_P$IAT_Parent >=40, 1, 0)

############## dx odds ratios ############
############SR
#get numbers for PIU and non-PIU
tab_ASD <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$ASD)
tab_Anx <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$Anxiety)
tab_Dep <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$Depression)
tab_ADHD_C <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$ADHD_Combined)
tab_ADHD_I <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$ADHD_Inattentive)
tab_Soc_Anx <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$Social_Anxiety)
#unadjusted
#create Dx list (ASD, Anxiety, Depression, ADHD-C, ADHD-I, Social Anxiety)
Diagnoses <- c(15, 17:20, 22)
# loop for running the glms for each dx
my_glms <- lapply(Diagnoses, function(x) glm(PIU ~ Standard_Sample_SR[,x], family = binomial, data = Standard_Sample_SR))
# loop for pulling out relevant info
my_summaries <- lapply(my_glms, summary)
my_coefs <- sapply(my_glms, function(x) round(exp(x$coefficients[2]), digits = 2))
my_CI_1 <- sapply(my_glms, function(x) round(exp(confint(x)[2, 1]), digits =2)) 
my_CI_2 <- sapply(my_glms, function(x) round(exp(confint(x)[2, 2]), digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Odds_Dx_SR_unadjusted <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Odds_Dx_SR_unadjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Odds_Dx_SR_unadjusted) <- c("ASD", "Anxiety", "Depression", "ADHD_Combined", "ADHD_Inattentive", "Social_Anxiety")
write.csv(Odds_Dx_SR_unadjusted, file = "Tables/Odds_Dx_SR_unadjusted.csv")

#####adjusted for covariates
Odds_Dx_SR_ASD <- glm(PIU ~ ASD + 
                     Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
Odds_Dx_SR_Anxiety <- glm(PIU ~ Anxiety + 
                         Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
Odds_Dx_SR_Depression <- glm(PIU ~ Depression +  
                            Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
Odds_Dx_SR_ADHD_C <- glm(PIU ~ ADHD_Combined + 
                               Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
Odds_Dx_SR_ADHD_I <- glm(PIU ~ ADHD_Inattentive +  
                                  Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
Odds_Dx_SR_Social_Anxiety <- glm(PIU ~ Social_Anxiety +
                                Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
#combine wanted values into dtaframes for each dx
DF_ASD <- data.frame(
  round(exp(Odds_Dx_SR_ASD$coefficients[2]), digits = 2), round(exp(confint(Odds_Dx_SR_ASD)[2, 1]), digits =2), 
  round(exp(confint(Odds_Dx_SR_ASD)[2, 2]), digits = 2), 
  round(summary(Odds_Dx_SR_ASD)$coefficients[2, 4], digits=2)) 
names(DF_ASD) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Anxiety <- data.frame(
  round(exp(Odds_Dx_SR_Anxiety$coefficients[2]), digits = 2), round(exp(confint(Odds_Dx_SR_Anxiety)[2, 1]), digits =2), 
  round(exp(confint(Odds_Dx_SR_Anxiety)[2, 2]), digits = 2), 
  round(summary(Odds_Dx_SR_Anxiety)$coefficients[2, 4], digits=2)) 
names(DF_Anxiety) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Depression <- data.frame(
  round(exp(Odds_Dx_SR_Depression$coefficients[2]), digits = 2), round(exp(confint(Odds_Dx_SR_Depression)[2, 1]), digits =2), 
  round(exp(confint(Odds_Dx_SR_Depression)[2, 2]), digits = 2), 
  round(summary(Odds_Dx_SR_Depression)$coefficients[2, 4], digits=2)) 
names(DF_Depression) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_ADHD_C <- data.frame(
  round(exp(Odds_Dx_SR_ADHD_C$coefficients[2]), digits = 2), round(exp(confint(Odds_Dx_SR_ADHD_C)[2, 1]), digits =2), 
  round(exp(confint(Odds_Dx_SR_ADHD_C)[2, 2]), digits = 2), 
  round(summary(Odds_Dx_SR_ADHD_C)$coefficients[2, 4], digits=2)) 
names(DF_ADHD_C) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_ADHD_I <- data.frame(
  round(exp(Odds_Dx_SR_ADHD_I$coefficients[2]), digits = 2), round(exp(confint(Odds_Dx_SR_ADHD_I)[2, 1]), digits =2), 
  round(exp(confint(Odds_Dx_SR_ADHD_I)[2, 2]), digits = 2), 
  round(summary(Odds_Dx_SR_ADHD_I)$coefficients[2, 4], digits=2)) 
names(DF_ADHD_I) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_Social_Anxiety <- data.frame(
  round(exp(Odds_Dx_SR_Social_Anxiety$coefficients[2]), digits = 2), round(exp(confint(Odds_Dx_SR_Social_Anxiety)[2, 1]), digits =2), 
  round(exp(confint(Odds_Dx_SR_Social_Anxiety)[2, 2]), digits = 2), 
  round(summary(Odds_Dx_SR_Social_Anxiety)$coefficients[2, 4], digits=2)) 
names(DF_Social_Anxiety) <- c("B", "2.5% CI", "97.5% CI", "p value")
#bind dataframes into one with all dxs for export
Odds_Dx_SR_Adj_Cov <- rbind(DF_ASD, DF_Anxiety, DF_Depression,
                              DF_ADHD_C, DF_ADHD_I, DF_Social_Anxiety)
#export as csv file
write.csv(Odds_Dx_SR_Adj_Cov, file = "Tables/Odds_Dx_SR_Adj_Cov.csv")

#####adjusted for covariates and all Dxs
Odds_Dx_SR_adjusted <- glm(PIU ~ ASD + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + Social_Anxiety +
                        Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
#combine wanted values into one dataframe
DF_Odds_Dx_SR_adjusted <- data.frame(
  round(exp(Odds_Dx_SR_adjusted$coefficients[2:7]), digits = 2), round(exp(confint(Odds_Dx_SR_adjusted)[2:7, 1]), digits =2), 
  round(exp(confint(Odds_Dx_SR_adjusted)[2:7, 2]), digits = 2), 
  round(summary(Odds_Dx_SR_adjusted)$coefficients[2:7, 4], digits=2)) 
names(DF_Odds_Dx_SR_adjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
#export as csv file
write.csv(DF_Odds_Dx_SR_adjusted, file = "Tables/Odds_Dx_SR_adjusted.csv")

################ P
#unadjusted
#create Dx list
Diagnoses <- c(15, 17:20, 22)
# loop for running the glms for each dx
my_glms <- lapply(Diagnoses, function(x) glm(PIU ~ Standard_Sample_P[,x], family = binomial, data = Standard_Sample_P))
# loop for pulling out relevant info
my_summaries <- lapply(my_glms, summary)
my_coefs <- sapply(my_glms, function(x) round(exp(x$coefficients[2]), digits = 2))
my_CI_1 <- sapply(my_glms, function(x) round(exp(confint(x)[2, 1]), digits =2)) 
my_CI_2 <- sapply(my_glms, function(x) round(exp(confint(x)[2, 2]), digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Odds_Dx_P_unadjusted <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Odds_Dx_P_unadjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Odds_Dx_P_unadjusted) <- c("ASD", "Anxiety", "Depression", "ADHD_Combined", "ADHD_Inattentive", "Social_Anxiety")
write.csv(Odds_Dx_P_unadjusted, file = "Tables/Odds_Dx_P_unadjusted.csv")

# Adjusted for covariates
#create Dx list
Diagnoses <- c(15, 17:20, 22)
# loop for running the glms for each dx
my_glms <- lapply(Diagnoses, function(x) glm(PIU ~ Standard_Sample_P[,x] + Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_P))
# loop for pulling out relevant info
my_summaries <- lapply(my_glms, summary)
my_coefs <- sapply(my_glms, function(x) round(exp(x$coefficients[2]), digits = 2))
my_CI_1 <- sapply(my_glms, function(x) round(exp(confint(x)[2, 1]), digits =2)) 
my_CI_2 <- sapply(my_glms, function(x) round(exp(confint(x)[2, 2]), digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Odds_Dx_P_Adj_Cov <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Odds_Dx_P_Adj_Cov) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Odds_Dx_P_Adj_Cov) <- c("ASD", "Anxiety", "Depression", "ADHD_Combined", "ADHD_Inattentive", "Social_Anxiety")
write.csv(Odds_Dx_P_Adj_Cov, file = "Tables/Odds_Dx_P_Adj_Cov.csv")

# Adjusted for covariates and all dxs
Odds_Dx_P_adjusted <- glm(PIU ~ ASD + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + Social_Anxiety +
                             Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_P)
#combine wanted values into one dataframe
DF_Odds_Dx_P_adjusted <- data.frame(
  round(exp(Odds_Dx_P_adjusted$coefficients[2:7]), digits = 2), round(exp(confint(Odds_Dx_P_adjusted)[2:7, 1]), digits =2), 
  round(exp(confint(Odds_Dx_P_adjusted)[2:7, 2]), digits = 2), 
  round(summary(Odds_Dx_P_adjusted)$coefficients[2:7, 4], digits=2)) 
names(DF_Odds_Dx_P_adjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
write.csv(DF_Odds_Dx_P_adjusted, file = "Tables/Odds_Dx_P_adjusted.csv")


###################### PIU PREDICTING Negative outcomes #######
#get standardized coefficients
library(lm.beta)

############## SR
# unadjusted
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(13, 11, 9, 10, 12)
# loop for running the lms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Standard_Sample_SR[,x] ~ PIU, data = Standard_Sample_SR))
#get standardized coefficients for each
my_standardized <- lapply(my_lms, lm.beta)
# loop for pulling out relevant info
my_summaries <- lapply(my_lms, summary)
my_coefs <- sapply(my_standardized, function(x) round(x$coefficients[2], digits = 2))
my_CI_1 <- sapply(my_lms, function(x) round(confint(x)[2, 1], digits =2)) 
my_CI_2 <- sapply(my_lms, function(x) round(confint(x)[2, 2], digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Neg_Outcomes_unadjusted_SR <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Neg_Outcomes_unadjusted_SR) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Neg_Outcomes_unadjusted_SR) <- c("CIS", "PAQ", "BMI", "FMI", "SDS")
write.csv(Neg_Outcomes_unadjusted_SR, file = "Tables/Neg_Outcomes_unadjusted_SR.csv")

# adjusted for covariates
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(13, 11, 9, 10, 12)
# loop for running the lms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Standard_Sample_SR[,x] ~ PIU + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_SR))
#get standardized coefficients for each
my_standardized <- lapply(my_lms, lm.beta)
# loop for pulling out relevant info
my_summaries <- lapply(my_lms, summary)
my_coefs <- sapply(my_standardized, function(x) round(x$coefficients[2], digits = 2))
my_CI_1 <- sapply(my_lms, function(x) round(confint(x)[2, 1], digits =2)) 
my_CI_2 <- sapply(my_lms, function(x) round(confint(x)[2, 2], digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Neg_Outcomes_adj_cov_SR <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Neg_Outcomes_adj_cov_SR) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Neg_Outcomes_adj_cov_SR) <- c("CIS", "PAQ", "BMI", "FMI", "SDS")
write.csv(Neg_Outcomes_adj_cov_SR, file = "Tables/Neg_Outcomes_adj_cov_SR.csv")

# adjusted for covariates and all dxs
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(13, 11, 9, 10, 12)
# loop for running the glms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Standard_Sample_SR[,x] ~ PIU + ASD + Anxiety + Depression +
                                                ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                                                Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_SR))
#get standardized coefficients for each
my_standardized <- lapply(my_lms, lm.beta)
# loop for pulling out relevant info
my_summaries <- lapply(my_lms, summary)
my_coefs <- sapply(my_standardized, function(x) round(x$coefficients[2], digits = 2))
my_CI_1 <- sapply(my_lms, function(x) round(confint(x)[2, 1], digits =2)) 
my_CI_2 <- sapply(my_lms, function(x) round(confint(x)[2, 2], digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Neg_Outcomes_adjusted_SR <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Neg_Outcomes_adjusted_SR) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Neg_Outcomes_adjusted_SR) <- c("CIS", "PAQ", "BMI", "FMI", "SDS")
write.csv(Neg_Outcomes_adjusted_SR, file = "Tables/Neg_Outcomes_adjusted_SR.csv")


############## P
# unadjusted
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(13, 11, 9, 10, 12)
# loop for running the lms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Standard_Sample_P[,x] ~ PIU, data = Standard_Sample_P))
#get standardized coefficients for each
my_standardized <- lapply(my_lms, lm.beta)
# loop for pulling out relevant info
my_summaries <- lapply(my_lms, summary)
my_coefs <- sapply(my_standardized, function(x) round(x$coefficients[2], digits = 2))
my_CI_1 <- sapply(my_lms, function(x) round(confint(x)[2, 1], digits =2)) 
my_CI_2 <- sapply(my_lms, function(x) round(confint(x)[2, 2], digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Neg_Outcomes_unadjusted_P <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Neg_Outcomes_unadjusted_P) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Neg_Outcomes_unadjusted_P) <- c("CIS", "PAQ", "BMI", "FMI", "SDS")
write.csv(Neg_Outcomes_unadjusted_P, file = "Tables/Neg_Outcomes_unadjusted_P.csv")

# adjusted for covariates
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(13, 11, 9, 10, 12)
# loop for running the lms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Standard_Sample_P[,x] ~ PIU + Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_P))
#get standardized coefficients for each
my_standardized <- lapply(my_lms, lm.beta)
# loop for pulling out relevant info
my_summaries <- lapply(my_lms, summary)
my_coefs <- sapply(my_standardized, function(x) round(x$coefficients[2], digits = 2))
my_CI_1 <- sapply(my_lms, function(x) round(confint(x)[2, 1], digits =2)) 
my_CI_2 <- sapply(my_lms, function(x) round(confint(x)[2, 2], digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Neg_Outcomes_adj_cov_P <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Neg_Outcomes_adj_cov_P) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Neg_Outcomes_adj_cov_P) <- c("CIS", "PAQ", "BMI", "FMI", "SDS")
write.csv(Neg_Outcomes_adj_cov_P, file = "Tables/Neg_Outcomes_adj_cov_P.csv")

# adjusted for covariates and all dxs
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(13, 11, 9, 10, 12)
# loop for running the lms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Standard_Sample_P[,x] ~ PIU + ASD + Anxiety + Depression +
                                                ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                                                Sex + Age + SES + Site + Single_Caregiver, data = Standard_Sample_P))
#get standardized coefficients for each
my_standardized <- lapply(my_lms, lm.beta)
# loop for pulling out relevant info
my_summaries <- lapply(my_lms, summary)
my_coefs <- sapply(my_standardized, function(x) round(x$coefficients[2], digits = 2))
my_CI_1 <- sapply(my_lms, function(x) round(confint(x)[2, 1], digits =2)) 
my_CI_2 <- sapply(my_lms, function(x) round(confint(x)[2, 2], digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Neg_Outcomes_adjusted_P <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Neg_Outcomes_adjusted_P) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Neg_Outcomes_adjusted_P) <- c("CIS", "PAQ", "BMI", "FMI", "SDS")
write.csv(Neg_Outcomes_adjusted_P, file = "Tables/Neg_Outcomes_adjusted_P.csv")

