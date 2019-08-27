#################### Import all data ##############
setwd("J:/Healthy Brain Network/10. Staff Development/Research Projects/2018/Internalizing/R6 Project/Data")
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
setwd("J:/Healthy Brain Network/10. Staff Development/Research Projects/2018/Internalizing/R6 Project")
################## Clean data #########################

#remove participants with no data
Physical <- Physical[complete.cases(Physical$BMI), ]
PCIAT <- PCIAT[-c(958, 1132, 1491), ]
C3SR <- C3SR[-c(653, 1163), ]
FTND <- FTND[-c(58), ]
MFQ_P <- MFQ_P[-c(918, 1296), ]
MFQ_SR <- MFQ_SR[-c(918), ]
SCARED_SR <- SCARED_SR[-c(727), ]
YFAS_C <- YFAS_C[-c(425,450, 1346), ]
ConsensusDx <- ConsensusDx[ConsensusDx$NoDX != 3, ]

#turn NAs into zeros only for IAT and PCIAT because of scoring errors.
IAT[is.na(IAT)] <- 0
PCIAT[is.na(PCIAT)] <- 0

#rescore PCIAT and IAT
IAT$IAT_Total <- rowSums(IAT[2:21])
PCIAT$PCIAT_Total <- rowSums(PCIAT[2:21])

#create dummy variable for 1 caregiver only based on Barratt responses
Barratt$No_C2 <- ifelse(is.na(Barratt$Barratt_P2_Edu) & 
                          is.na(Barratt$Barratt_P2_Occ), 1, 0)
