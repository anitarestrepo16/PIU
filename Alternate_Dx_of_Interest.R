setwd("J:/Healthy Brain Network/10. Staff Development/Research Projects/2018/Internalizing/R6 Project/Scripts")
#Call initial stuff script
source("Data_Cleaning.R")

#create dataframe "Alternate_Dx_of_interest"
# For each Dx, one variable where 1 = that dx, 0 = no_dx, 2 = a different dx
Alternate_Dx_of_interest <- data.frame(
  ID = ConsensusDx$URSI,
  No_Dx = ifelse(ConsensusDx$NoDX == 1, 1, 0),
  ASD = ifelse ((ConsensusDx$DX_01_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_02_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_03_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_04_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_05_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_06_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_07_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_08_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_09_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_10_Sub == "Autism Spectrum Disorder"), 1, ifelse(ConsensusDx$NoDX == 1, 0, 2)),
  Anxiety = ifelse (ConsensusDx$DX_01_Cat == "Anxiety Disorders" | ConsensusDx$DX_02_Cat == "Anxiety Disorders" | ConsensusDx$DX_03_Cat == "Anxiety Disorders" | ConsensusDx$DX_04_Cat == "Anxiety Disorders" | ConsensusDx$DX_05_Cat == "Anxiety Disorders" | ConsensusDx$DX_06_Cat == "Anxiety Disorders" | ConsensusDx$DX_07_Cat == "Anxiety Disorders" | ConsensusDx$DX_08_Cat == "Anxiety Disorders" | ConsensusDx$DX_09_Cat == "Anxiety Disorders" | ConsensusDx$DX_10_Cat == "Anxiety Disorders", 1, ifelse(ConsensusDx$NoDX == 1, 0, 2)),
  Depression = ifelse (ConsensusDx$DX_01_Cat == "Depressive Disorders" | ConsensusDx$DX_02_Cat == "Depressive Disorders" | ConsensusDx$DX_03_Cat == "Depressive Disorders" | ConsensusDx$DX_04_Cat == "Depressive Disorders" | ConsensusDx$DX_05_Cat == "Depressive Disorders" | ConsensusDx$DX_06_Cat == "Depressive Disorders" | ConsensusDx$DX_07_Cat == "Depressive Disorders" | ConsensusDx$DX_08_Cat == "Depressive Disorders" | ConsensusDx$DX_09_Cat == "Depressive Disorders" | ConsensusDx$DX_10_Cat == "Depressive Disorders", 1, ifelse(ConsensusDx$NoDX == 1, 0, 2)),
  ADHD_Combined = ifelse (ConsensusDx$DX_01 == "ADHD-Combined Type" | ConsensusDx$DX_02 == "ADHD-Combined Type" | ConsensusDx$DX_03 == "ADHD-Combined Type" | ConsensusDx$DX_04 == "ADHD-Combined Type" | ConsensusDx$DX_05 == "ADHD-Combined Type" | ConsensusDx$DX_06 == "ADHD-Combined Type" | ConsensusDx$DX_07 == "ADHD-Combined Type" | ConsensusDx$DX_08 == "ADHD-Combined Type" | ConsensusDx$DX_09 == "ADHD-Combined Type" | ConsensusDx$DX_10 == "ADHD-Combined Type", 1, ifelse(ConsensusDx$NoDX == 1, 0, 2)),
  ADHD_Inattentive = ifelse (ConsensusDx$DX_01 == "ADHD-Inattentive Type" | ConsensusDx$DX_02 == "ADHD-Inattentive Type" | ConsensusDx$DX_03 == "ADHD-Inattentive Type" | ConsensusDx$DX_04 == "ADHD-Inattentive Type" | ConsensusDx$DX_05 == "ADHD-Inattentive Type" | ConsensusDx$DX_06 == "ADHD-Inattentive Type" | ConsensusDx$DX_07 == "ADHD-Inattentive Type" | ConsensusDx$DX_08 == "ADHD-Inattentive Type" | ConsensusDx$DX_09 == "ADHD-Inattentive Type" | ConsensusDx$DX_10 == "ADHD-Inattentive Type", 1, ifelse(ConsensusDx$NoDX == 1, 0, 2)),
  Social_Anxiety = ifelse (ConsensusDx$DX_01 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_02 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_03 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_04 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_05 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_06 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_07 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_08 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_09 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_10 == "Social Anxiety (Social Phobia)", 1, ifelse(ConsensusDx$NoDX == 1, 0, 2))
)
# save file and turn all na into 0
write.csv(Dx_of_interest, "Data/Dx_of_interest.csv", row.names = FALSE, na="0")
#read in new Dx_of_interest
Dx_of_interest <- read.csv("Data/Dx_of_interest.csv")
