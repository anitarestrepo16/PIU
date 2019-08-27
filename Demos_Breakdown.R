#set WD
setwd("J:/Healthy Brain Network/10. Staff Development/Research Projects/2018/Internalizing/R6 Project/Scripts")
#Call initial stuff script
source("Data_Cleaning.R")

#for in-text demos (All SR):
#descriptives for age
summary(Standard_Sample_SR$Age)
sd(Standard_Sample_SR$Age)
#number female
table(Standard_Sample_SR$Sex)

#get numbers for PIU and non-PIU (all SR)
tab_Sex <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$Sex)
tab_Age <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$Age_Cat)
tab_SES <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$SES_Cat)
tab_Race <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$Race_Cat)
tab_Site <- table(Standard_Sample_SR$PIU, Standard_Sample_SR$Site_Cat)

#for table 1 (all SR)
#new categories to use first level of variable as reference group
#create problematic (1) vs. non-problematic (0) based on score of 40 (Kim et al.)
Standard_Sample_SR$PIU = ifelse (Standard_Sample_SR$IAT_SR >=40, 1, 0)
#create age categories (0 = 7-9; 1 = 10-12; 2 = 13-15)
Standard_Sample_SR$Age_Cat = ifelse(Standard_Sample_SR$Age <10.00, 0, 
                                    ifelse(Standard_Sample_SR$Age >=10.00 & 
                                             Standard_Sample_SR$Age<13.00, 1,
                                           ifelse(Standard_Sample_SR$Age >=13.00 & 
                                                    Standard_Sample_SR$Age < 16.00, 2, 3)))
# SES categories (0 = Low SES; 1 = Middle SES; 2 = High SES)
Standard_Sample_SR$SES_Cat = ifelse (Standard_Sample_SR$SES < 28, 0, 
                                     ifelse(Standard_Sample_SR$SES >= 28 &
                                              Standard_Sample_SR$SES < 47, 1, 2))
#Race categories (0 = white; 1 = black; 2 = hispanic; 3 = Asian; 4 = Other)
Standard_Sample_SR$Race_Cat = ifelse (Standard_Sample_SR$Race == 0, 0, 
                                      ifelse (Standard_Sample_SR$Race == 1, 1,
                                              ifelse(Standard_Sample_SR$Race == 2, 2,
                                                     ifelse(Standard_Sample_SR$Race == 3, 3, 4))))
#Site categories (0 = SI, 1 = Midtown, 3 = MRV)
Standard_Sample_SR$Site_Cat = ifelse(Standard_Sample_SR$Site == 1, 0,
                                     ifelse (Standard_Sample_SR$Site == 3, 1, 2))

library(epiR)
#run odds ratios
#create variable list (Sex, Age, SES, Race, Site, Dxs)
Vars <- c(3, 24:27, 15, 17:20, 22)
# loop for creating relevant tables for each variable
my_tables <- lapply(Vars, function(x) table(Standard_Sample_SR$PIU, 
                                            Standard_Sample_SR[,x]))
#find table within my_tables with largest number of columns
longest <- my_tables[which.max(sapply(my_tables,ncol))]
#generate numbers to create index vector
odd <- lapply(longest, function(x) seq(3, by = 2, len = ncol(x)-1))
even <- lapply(longest, function(x) seq(4, by = 2, len = ncol(x)-1))
#turn lists into vectors
odd <- unlist(odd, use.names = FALSE)
even <- unlist(even, use.names = FALSE)
#pair odd and even numbers by position in vector
odd_even <- mapply(c, odd, even, SIMPLIFY = FALSE)
#input the pair into the numbers for matrix
my_minisubtables <- lapply(odd_even, function(x) c(1, 2, x))
#create the subtables
my_subtables <- lapply(my_tables, function(z) lapply(my_minisubtables, function(x) z[x]))
#remove one level of list
my_subtables <- unlist(my_subtables, use.names = FALSE, recursive = FALSE)
#remove NAs
my_subtables <- Filter(Negate(anyNA), my_subtables)
#create matrices
my_matrices <- lapply(my_subtables, function(x) matrix(x, nrow = 2))
# loop for running ORs for each matrix
my_epis <- lapply(my_matrices, function(x) epi.2by2(x, method="cohort.count", conf.level=0.95))
# loop for pulling out relevant info
my_No_PIU <- sapply(my_matrices, function(x) x[1, 2])
my_Yes_PIU <- sapply(my_matrices, function(x) x[2, 2])
my_values <- sapply(my_epis, function(x) round(x$res$OR.strata.score, digits = 2))
my_ORs <- unlist(my_values[1,], use.names = FALSE)
lower_CI <- unlist(my_values[2, ], use.names = FALSE)
upper_CI <- unlist(my_values[3, ], use.names = FALSE)
my_ps <- sapply(my_epis, function(x) round(x$res$chisq.strata[ , 3], digits = 2))
# combine and save into one df
All_Demos_ORs <- data.frame(my_No_PIU, my_Yes_PIU, my_ORs, lower_CI, 
                            upper_CI, my_ps, row.names = NULL)
colnames(All_Demos_ORs) <- c("No_PIU", "Yes_PIU", "OR", "2.5% CI", "97.5% CI", "p")
rownames(All_Demos_ORs) <- c("Female", "10-12", "13-15", "Middle SES", "High SES", "Black",
                             "Hispanic", "Asian", "Other", "Midtown", "ASD", "Anxiety", 
                             "Depression", "ADHD-C", "ADHD-I", "Social_Anxiety")
#get percentages
All_Demos_ORs$Percent_No = round(All_Demos_ORs$No_PIU/nrow(Standard_Sample_SR)*100, digits = 2)
All_Demos_ORs$Percent_Yes = round(All_Demos_ORs$Yes_PIU/nrow(Standard_Sample_SR)*100, digits = 2)
#save as csv
write.csv(All_Demos_ORs, file = "Tables/All_Demos_ORs.csv")
