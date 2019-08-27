#################### Import all data ##############
setwd("J:/Healthy Brain Network/10. Staff Development/Research Projects/2018/Internalizing/R6 Project/Data")
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
setwd("J:/Healthy Brain Network/10. Staff Development/Research Projects/2018/Internalizing/R6 Project")
#turn NAs into zeros only for IAT and PCIAT because of scoring errors.
IAT[is.na(IAT)] <- 0
PCIAT[is.na(PCIAT)] <- 0
#rescore PCIAT and IAT
IAT$IAT_Total <- rowSums(IAT[2:21])
PCIAT$PCIAT_Total <- rowSums(PCIAT[2:21])
#create dummy variable for 1 caregiver only based on Barratt responses
Barratt$No_C2 <- ifelse(is.na(Barratt$Barratt_P2_Edu) & 
                          is.na(Barratt$Barratt_P2_Occ), 1, 0)
#pull out data of interest (Dx_of_interest already created above)
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

##################### Odds Ratios ###############
#create df
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
OR_Max_SR <- Reduce(Merge, list(IAT_of_interest,
                                         Demos_of_interest, Barratt_of_interest, 
                                         Dx_of_interest))
OR_Max_P <- Reduce(Merge, list(PCIAT_of_interest,
                                        Demos_of_interest, Barratt_of_interest,
                                        Dx_of_interest))
#remove NAs
OR_Max_SR <- OR_Max_SR[complete.cases(OR_Max_SR),]
OR_Max_P <- OR_Max_P[complete.cases(OR_Max_P),]
#add column for PIU and non PIU
#create problematic (1) vs. non-problematic (0) based on score of 40 (Kim et al.)
OR_Max_SR$PIU = ifelse (OR_Max_SR$IAT_SR >=40, 1, 0)
OR_Max_P$PIU = ifelse (OR_Max_P$IAT_Parent >=40, 1, 0)

# dx odds ratios
############SR
#unadjusted
#create Dx list (ASD, Anxiety, Depression, ADHD-C, ADHD-I, Social Anxiety)
Diagnoses <- c(9, 11:14, 16)
# loop for running the glms for each dx
my_glms <- lapply(Diagnoses, function(x) glm(PIU ~ OR_Max_SR[,x], family = binomial, data = OR_Max_SR))
# loop for pulling out relevant info
my_summaries <- lapply(my_glms, summary)
my_coefs <- sapply(my_glms, function(x) round(exp(x$coefficients[2]), digits = 2))
my_CI_1 <- sapply(my_glms, function(x) round(exp(confint(x)[2, 1]), digits =2)) 
my_CI_2 <- sapply(my_glms, function(x) round(exp(confint(x)[2, 2]), digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Full_Sample_Odds_Dx_SR_unadjusted <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Full_Sample_Odds_Dx_SR_unadjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Full_Sample_Odds_Dx_SR_unadjusted) <- c("ASD", "Anxiety", "Depression", "ADHD_Combined", "ADHD_Inattentive", "Social_Anxiety")
write.csv(Full_Sample_Odds_Dx_SR_unadjusted, file = "Tables/Full_Sample_Odds_Dx_SR_unadjusted.csv")

#####adjusted for covariates
Odds_Dx_SR_ASD <- glm(PIU ~ ASD + 
                        Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = OR_Max_SR)
Odds_Dx_SR_Anxiety <- glm(PIU ~ Anxiety + 
                            Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = OR_Max_SR)
Odds_Dx_SR_Depression <- glm(PIU ~ Depression +  
                               Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = OR_Max_SR)
Odds_Dx_SR_ADHD_C <- glm(PIU ~ ADHD_Combined + 
                           Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = OR_Max_SR)
Odds_Dx_SR_ADHD_I <- glm(PIU ~ ADHD_Inattentive +  
                           Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = OR_Max_SR)
Odds_Dx_SR_Social_Anxiety <- glm(PIU ~ Social_Anxiety +
                                   Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = OR_Max_SR)
#combine wanted values into dataframes for each dx
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
write.csv(Odds_Dx_SR_Adj_Cov, file = "Tables/Full_Sample_Odds_Dx_SR_Adj_Cov.csv")

#####adjusted for covariates and all Dxs
Odds_Dx_SR_adjusted <- glm(PIU ~ ASD + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + Social_Anxiety +
                             Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = OR_Max_SR)
#combine wanted values into one dataframe
DF_Odds_Dx_SR_adjusted <- data.frame(
  round(exp(Odds_Dx_SR_adjusted$coefficients[2:7]), digits = 2), round(exp(confint(Odds_Dx_SR_adjusted)[2:7, 1]), digits =2), 
  round(exp(confint(Odds_Dx_SR_adjusted)[2:7, 2]), digits = 2), 
  round(summary(Odds_Dx_SR_adjusted)$coefficients[2:7, 4], digits=2)) 
names(DF_Odds_Dx_SR_adjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
#export as csv file
write.csv(DF_Odds_Dx_SR_adjusted, file = "Tables/Full_Sample_Odds_Dx_SR_adjusted.csv")

################ P
#unadjusted
#create Dx list
Diagnoses <- c(9, 11:14, 16)
# loop for running the glms for each dx
my_glms <- lapply(Diagnoses, function(x) glm(PIU ~ OR_Max_P[,x], family = binomial, data = OR_Max_P))
# loop for pulling out relevant info
my_summaries <- lapply(my_glms, summary)
my_coefs <- sapply(my_glms, function(x) round(exp(x$coefficients[2]), digits = 2))
my_CI_1 <- sapply(my_glms, function(x) round(exp(confint(x)[2, 1]), digits =2)) 
my_CI_2 <- sapply(my_glms, function(x) round(exp(confint(x)[2, 2]), digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))
# combine and save into one df
Full_Sample_Odds_Dx_P_unadjusted <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Full_Sample_Odds_Dx_P_unadjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Full_Sample_Odds_Dx_P_unadjusted) <- c("ASD", "Anxiety", "Depression", "ADHD_Combined", "ADHD_Inattentive", "Social_Anxiety")
write.csv(Full_Sample_Odds_Dx_P_unadjusted, file = "Tables/Full_Sample_Odds_Dx_P_unadjusted.csv")

# Adjusted for covariates
#create Dx list
Diagnoses <- c(9, 11:14, 16)
# loop for running the glms for each dx
my_glms <- lapply(Diagnoses, function(x) glm(PIU ~ OR_Max_P[,x] + Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = OR_Max_P))
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
write.csv(Odds_Dx_P_Adj_Cov, file = "Tables/Full_Sample_Odds_Dx_P_Adj_Cov.csv")

# Adjusted for covariates and all dxs
Odds_Dx_P_adjusted <- glm(PIU ~ ASD + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + Social_Anxiety +
                            Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = OR_Max_P)
#combine wanted values into one dataframe
DF_Odds_Dx_P_adjusted <- data.frame(
  round(exp(Odds_Dx_P_adjusted$coefficients[2:7]), digits = 2), round(exp(confint(Odds_Dx_P_adjusted)[2:7, 1]), digits =2), 
  round(exp(confint(Odds_Dx_P_adjusted)[2:7, 2]), digits = 2), 
  round(summary(Odds_Dx_P_adjusted)$coefficients[2:7, 4], digits=2)) 
names(DF_Odds_Dx_P_adjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
write.csv(DF_Odds_Dx_P_adjusted, file = "Tables/Full_Sample_Odds_Dx_P_adjusted.csv")

###################### PIU PREDICTING Negative outcomes #######
#create Dfs
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Neg_Max_SR <- Reduce(Merge, list(IAT_of_interest,
                                Demos_of_interest, Barratt_of_interest, 
                                Dx_of_interest, Physical_of_interest, 
                                BIA_of_interest, PAQ_of_interest, SDS_of_interest, 
                                CIS_SR_of_interest))
Neg_Max_P <- Reduce(Merge, list(PCIAT_of_interest,
                               Demos_of_interest, Barratt_of_interest,
                               Dx_of_interest, Physical_of_interest, 
                               BIA_of_interest, PAQ_of_interest, SDS_of_interest, 
                               CIS_P_of_interest))
#remove NAs
Neg_Max_SR <- Neg_Max_SR[complete.cases(Neg_Max_SR),]
Neg_Max_P <- Neg_Max_P[complete.cases(Neg_Max_P),]
#add column for PIU and non PIU
#create problematic (1) vs. non-problematic (0) based on score of 40 (Kim et al.)
Neg_Max_SR$PIU = ifelse (Neg_Max_SR$IAT_SR >=40, 1, 0)
Neg_Max_P$PIU = ifelse (Neg_Max_P$IAT_Parent >=40, 1, 0)

library(lm.beta)
############## SR
# unadjusted
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(21, 19, 17, 18, 20)
# loop for running the lms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Neg_Max_SR[,x] ~ PIU, data = Neg_Max_SR))
#get standardized coefficients
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
write.csv(Neg_Outcomes_unadjusted_SR, file = "Tables/Full_Sample_Neg_Outcomes_unadjusted_SR.csv")

# adjusted for covariates
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(21, 19, 17, 18, 20)
# loop for running the lms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Neg_Max_SR[,x] ~ PIU + Sex + Age + SES + Site + Single_Caregiver, data = Neg_Max_SR))
#get standardized coefficients
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
write.csv(Neg_Outcomes_adj_cov_SR, file = "Tables/Full_Sample_Neg_Outcomes_adj_cov_SR.csv")

# adjusted for covariates and all dxs
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(21, 19, 17, 18, 20)
# loop for running the glms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Neg_Max_SR[,x] ~ PIU + ASD + Anxiety + Depression +
                                            ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                                            Sex + Age + SES + Site + Single_Caregiver, data = Neg_Max_SR))
#get standardized coefficients
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
write.csv(Neg_Outcomes_adjusted_SR, file = "Tables/Full_Sample_Neg_Outcomes_adjusted_SR.csv")


############## P
# unadjusted
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(21, 19, 17, 18, 20)
# loop for running the lms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Neg_Max_P[,x] ~ PIU, data = Neg_Max_P))
#get standardized coefficients
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
write.csv(Neg_Outcomes_unadjusted_P, file = "Tables/Full_Sample_Neg_Outcomes_unadjusted_P.csv")

# adjusted for covariates
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(21, 19, 17, 18, 20)
# loop for running the lms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Neg_Max_P[,x] ~ PIU + Sex + Age + SES + Site + Single_Caregiver, data = Neg_Max_P))
#get standardized coefficients
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
write.csv(Neg_Outcomes_adj_cov_P, file = "Tables/Full_Sample_Neg_Outcomes_adj_cov_P.csv")

# adjusted for covariates and all dxs
#create variable list (CIS, PAQ, BMI, FMI, SDS)
Outcomes <- c(21, 19, 17, 18, 20)
# loop for running the lms for each predictor
my_lms <- lapply(Outcomes, function(x) lm(Neg_Max_P[,x] ~ PIU + ASD + Anxiety + Depression +
                                            ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                                            Sex + Age + SES + Site + Single_Caregiver, data = Neg_Max_P))
#get standardized coefficients
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
write.csv(Neg_Outcomes_adjusted_P, file = "Tables/Full_Sample_Neg_Outcomes_adjusted_P.csv")


###################### Linear Dx Regressions ##############

#create df
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
OR_Max_SR <- Reduce(Merge, list(IAT_of_interest,
                                Demos_of_interest, Barratt_of_interest, 
                                Dx_of_interest))
OR_Max_P <- Reduce(Merge, list(PCIAT_of_interest,
                               Demos_of_interest, Barratt_of_interest,
                               Dx_of_interest))
#remove NAs
OR_Max_SR <- OR_Max_SR[complete.cases(OR_Max_SR),]
OR_Max_P <- OR_Max_P[complete.cases(OR_Max_P),]
#### run regressions for IAT (Dx)
#unadjusted
Reg_Dx_SR_unadjusted_ASD <- lm(IAT_SR ~ ASD, data = OR_Max_SR)
Reg_Dx_SR_unadjusted_Anxiety <- lm(IAT_SR ~ Anxiety, data = OR_Max_SR)
Reg_Dx_SR_unadjusted_Depression <- lm(IAT_SR ~ Depression, data = OR_Max_SR)
Reg_Dx_SR_unadjusted_ADHD_Combined <- lm(IAT_SR ~ ADHD_Combined, data = OR_Max_SR)
Reg_Dx_SR_unadjusted_ADHD_Inattentive <- lm(IAT_SR ~ ADHD_Inattentive, data = OR_Max_SR)
Reg_Dx_SR_unadjusted_Social_Anxiety <- lm(IAT_SR ~ Social_Anxiety, data = OR_Max_SR)
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
write.csv(Reg_Dx_SR_unadjusted, file = "Tables/Full_Sample_Reg_Dx_SR_unadjusted.csv")

#adjusted for covariates
Reg_Dx_SR_adj_cov_ASD <- lm(IAT_SR ~ ASD + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_SR)
Reg_Dx_SR_adj_cov_Anxiety <- lm(IAT_SR ~ Anxiety + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_SR)
Reg_Dx_SR_adj_cov_Depression <- lm(IAT_SR ~ Depression + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_SR)
Reg_Dx_SR_adj_cov_ADHD_Combined <- lm(IAT_SR ~ ADHD_Combined + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_SR)
Reg_Dx_SR_adj_cov_ADHD_Inattentive <- lm(IAT_SR ~ ADHD_Inattentive + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_SR)
Reg_Dx_SR_adj_cov_Social_Anxiety <- lm(IAT_SR ~ Social_Anxiety + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_SR)
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
write.csv(Reg_Dx_SR_adj_cov, file = "Tables/Full_Sample_Reg_Dx_SR_adj_cov.csv")

#adjusted for covariates + all dxs
Reg_Dx_SR_Adjusted_all <- lm(IAT_SR ~ ASD + Anxiety + Depression +
                               ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                               Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_SR)
Reg_Dx_SR_Adjusted <- data.frame(
  round(Reg_Dx_SR_Adjusted_all$coefficients[2:7], digits = 2), round(confint(Reg_Dx_SR_Adjusted_all)[2:7, 1], digits = 2),
  round(confint(Reg_Dx_SR_Adjusted_all)[2:7, 2], digits = 2), 
  round(summary(Reg_Dx_SR_Adjusted_all)$coefficients[2:7, 4], digits = 2)) 
names(Reg_Dx_SR_Adjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
#export as csv file
write.csv(Reg_Dx_SR_Adjusted, file = "Tables/Full_Sample_Reg_Dx_SR_Adjusted.csv")

#regression for PCIAT (Dx)

#unadjusted
Reg_Dx_P_unadjusted_ASD <- lm(IAT_Parent ~ ASD, data = OR_Max_P)
Reg_Dx_P_unadjusted_Anxiety <- lm(IAT_Parent ~ Anxiety, data = OR_Max_P)
Reg_Dx_P_unadjusted_Depression <- lm(IAT_Parent ~ Depression, data = OR_Max_P)
Reg_Dx_P_unadjusted_ADHD_Combined <- lm(IAT_Parent ~ ADHD_Combined, data = OR_Max_P)
Reg_Dx_P_unadjusted_ADHD_Inattentive <- lm(IAT_Parent ~ ADHD_Inattentive, data = OR_Max_P)
Reg_Dx_P_unadjusted_Social_Anxiety <- lm(IAT_Parent ~ Social_Anxiety, data = OR_Max_P)
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
write.csv(Reg_Dx_P_unadjusted, file = "Tables/Full_Sample_Reg_Dx_P_unadjusted.csv")

#adjusted for covariates
Reg_Dx_P_adj_cov_ASD <- lm(IAT_Parent ~ ASD + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_P)
Reg_Dx_P_adj_cov_Anxiety <- lm(IAT_Parent ~ Anxiety + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_P)
Reg_Dx_P_adj_cov_Depression <- lm(IAT_Parent ~ Depression + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_P)
Reg_Dx_P_adj_cov_ADHD_Combined <- lm(IAT_Parent ~ ADHD_Combined + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_P)
Reg_Dx_P_adj_cov_ADHD_Inattentive <- lm(IAT_Parent ~ ADHD_Inattentive + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_P)
Reg_Dx_P_adj_cov_Social_Anxiety <- lm(IAT_Parent ~ Social_Anxiety + Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_P)
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
write.csv(Reg_Dx_P_adj_cov, file = "Tables/Full_Sample_Reg_Dx_P_adj_cov.csv")

#adjusted for covariates + all dxs
Reg_Dx_P_Adjusted_all <- lm(IAT_Parent ~ ASD + Anxiety + Depression +
                              ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                              Sex + Age + SES + Site + Single_Caregiver, data = OR_Max_P)
Reg_Dx_P_Adjusted <- data.frame(
  round(Reg_Dx_P_Adjusted_all$coefficients[2:7], digits = 2), round(confint(Reg_Dx_P_Adjusted_all)[2:7, 1], digits = 2),
  round(confint(Reg_Dx_P_Adjusted_all)[2:7, 2], digits = 2), 
  round(summary(Reg_Dx_P_Adjusted_all)$coefficients[2:7, 4], digits = 2)) 
names(Reg_Dx_P_Adjusted) <- c("B", "2.5% CI", "97.5% CI", "p value")
#export as csv file
write.csv(Reg_Dx_P_Adjusted, file = "Tables/Full_Sample_Reg_Dx_P_Adjusted.csv")

