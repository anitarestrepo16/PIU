#set WD
setwd("J:/Healthy Brain Network/10. Staff Development/Research Projects/2018/Internalizing/R6 Project/Scripts")
#Call initial stuff script
source("Data_Cleaning.R")

#####################Basic histograms
#SR
pdf(file="IAT_Histogram.pdf", width = 50, height = 50)
par(mar = c(25, 25, 16, 8)+0.4, mgp = c(15, 7, 0))
IAT_Histogram <- hist(IAT_age_sex$IAT_SR,
                      main = "Distribution of IAT Scores (Self-Report)",
                      xlab = "IAT Scores (Self-Report)",
                      ylab = "Frequency",
                      xlim = c(0, 100),
                      ylim = c(0, 250),
                      breaks=10,
                      border = "dark blue",
                      col = "light blue",
                      cex.lab = 10,
                      cex.main = 13,
                      cex.axis = 10)
#abline(v=c(30, 50, 80), col="red", lty="dotted")
dev.off()
#Parent report
pdf(file="PCIAT_Histogram.pdf", width = 50, height=50)
par(mar = c(25, 25, 16, 8)+0.4, mgp = c(15, 7, 0))
PCIAT_Histogram <- hist(IAT_age_sex$IAT_Parent,
                        main = "Distribution of IAT Scores (Parent Report)",
                        xlab = "IAT Scores (Parent Report)",
                        ylab = "Frequency",
                        xlim = c(0, 100),
                        ylim = c(0, 250),
                        breaks=10,
                        border = "dark blue",
                        col = "light blue",
                        cex.lab = 10,
                        cex.main = 13,
                        cex.axis = 10)
#abline(v=c(30, 50, 80), col="red", lty="dotted")
dev.off()

#############stacked bar plots with age
#create age categories - SR
Standard_Sample_SR$Age_Group[Standard_Sample_SR$Age <10.00] <- 1
Standard_Sample_SR$Age_Group[Standard_Sample_SR$Age >=10.00 & Standard_Sample_SR$Age<13.00] <- 2
Standard_Sample_SR$Age_Group[Standard_Sample_SR$Age >=13.00 & Standard_Sample_SR$Age < 16.00] <- 3
#create IAT Score categories (break into 10 groups) - SR
Standard_Sample_SR$IAT_Range[Standard_Sample_SR$IAT_SR <10.00] <- 1
Standard_Sample_SR$IAT_Range[Standard_Sample_SR$IAT_SR >=10.00 & Standard_Sample_SR$IAT_SR<20.00] <- 2
Standard_Sample_SR$IAT_Range[Standard_Sample_SR$IAT_SR >=20.00 & Standard_Sample_SR$IAT_SR < 30.00] <- 3
Standard_Sample_SR$IAT_Range[Standard_Sample_SR$IAT_SR >=30.00 & Standard_Sample_SR$IAT_SR<40.00] <- 4
Standard_Sample_SR$IAT_Range[Standard_Sample_SR$IAT_SR >=40.00 & Standard_Sample_SR$IAT_SR < 50.00] <- 5
Standard_Sample_SR$IAT_Range[Standard_Sample_SR$IAT_SR >=50.00 & Standard_Sample_SR$IAT_SR<60.00] <- 6
Standard_Sample_SR$IAT_Range[Standard_Sample_SR$IAT_SR >=60.00 & Standard_Sample_SR$IAT_SR < 70.00] <- 7
Standard_Sample_SR$IAT_Range[Standard_Sample_SR$IAT_SR >=70.00 & Standard_Sample_SR$IAT_SR<80.00] <- 8
Standard_Sample_SR$IAT_Range[Standard_Sample_SR$IAT_SR >=80.00 & Standard_Sample_SR$IAT_SR < 90.00] <- 9
Standard_Sample_SR$IAT_Range[Standard_Sample_SR$IAT_SR >= 90.00] <- 10
#create age categories - parent report
Standard_Sample_P$Age_Group[Standard_Sample_P$Age <10.00] <- 1
Standard_Sample_P$Age_Group[Standard_Sample_P$Age >=10.00 & Standard_Sample_P$Age<13.00] <- 2
Standard_Sample_P$Age_Group[Standard_Sample_P$Age >=13.00 & Standard_Sample_P$Age < 16.00] <- 3
#create IAT Score categories (break into 10 groups) - parent report
Standard_Sample_P$PCIAT_Range[Standard_Sample_P$IAT_Parent <10.00] <- 1
Standard_Sample_P$PCIAT_Range[Standard_Sample_P$IAT_Parent >=10.00 & Standard_Sample_P$IAT_Parent<20.00] <- 2
Standard_Sample_P$PCIAT_Range[Standard_Sample_P$IAT_Parent >=20.00 & Standard_Sample_P$IAT_Parent < 30.00] <- 3
Standard_Sample_P$PCIAT_Range[Standard_Sample_P$IAT_Parent >=30.00 & Standard_Sample_P$IAT_Parent<40.00] <- 4
Standard_Sample_P$PCIAT_Range[Standard_Sample_P$IAT_Parent >=40.00 & Standard_Sample_P$IAT_Parent < 50.00] <- 5
Standard_Sample_P$PCIAT_Range[Standard_Sample_P$IAT_Parent >=50.00 & Standard_Sample_P$IAT_Parent<60.00] <- 6
Standard_Sample_P$PCIAT_Range[Standard_Sample_P$IAT_Parent >=60.00 & Standard_Sample_P$IAT_Parent < 70.00] <- 7
Standard_Sample_P$PCIAT_Range[Standard_Sample_P$IAT_Parent >=70.00 & Standard_Sample_P$IAT_Parent<80.00] <- 8
Standard_Sample_P$PCIAT_Range[Standard_Sample_P$IAT_Parent >=80.00 & Standard_Sample_P$IAT_Parent < 90.00] <- 9
Standard_Sample_P$PCIAT_Range[Standard_Sample_P$IAT_Parent >= 90.00] <- 10

#stacked bar plots with each group of ten scores broken down by age categories and sex (side by side)
#pdf
#SR
pdf(file="Figures/IAT_Combined_Hist.pdf", width = 80, height = 50)
par(mar = c(25, 25, 35, 13)+0.4, mgp = c(18, 7, 0), mfrow = c(1, 2), family="serif")
counts <- table(Standard_Sample_SR$Age_Group, Standard_Sample_SR$IAT_Range)
barplot(counts,
        family = "serif",
        main ="Distribution of IAT Scores\nby Age Group (Self-Report)\n",
        xlab ="IAT Scores (Self-Report)", 
        ylab = "Frequency",
        ylim = c(0, 150),
        xlim = c(0, 12),
        col=c("darkcyan", "darkturquoise","darkseagreen2", "darkolivegreen4"),
        names = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"),
        cex.lab = 10,
        cex.main = 13,
        cex.axis = 10,
        cex.names = 7)
legend(x = 7.5, y = 100, c("Ages 7-9", "Ages 10-12", "Ages 13-15"), 
       fill = c("darkcyan", "darkturquoise","darkseagreen2"), cex = 8)
counts <- table(Standard_Sample_SR$Sex, Standard_Sample_SR$IAT_Range)
barplot(counts,
        family = "serif",
        main ="Distribution of IAT Scores\nby Sex (Self-Report)\n",
        xlab ="IAT Scores (Self-Report)", 
        ylab = "Frequency",
        ylim = c(0, 150),
        xlim = c(0, 12),
        col=c("darkcyan", "darkturquoise"),
        names = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"),
        cex.lab = 10,
        cex.main = 13,
        cex.axis = 10,
        cex.names = 7)
legend(x = 9, y = 100, c("Male", "Female"), 
       fill = c("darkcyan", "darkturquoise"), cex = 8)
dev.off()
#P
pdf(file="Figures/PCIAT_Combined_Hist.pdf", width = 80, height = 50)
par(mar = c(25, 25, 35, 13)+0.4, mgp = c(18, 7, 0), mfrow = c(1, 2), family = "serif")
counts <- table(Standard_Sample_P$Age_Group, Standard_Sample_P$PCIAT_Range)
barplot(counts,
        family = "serif",
        main ="Distribution of IAT Scores\nby Age Group (Parent-Report)\n",
        xlab ="IAT Scores (Parent-Report)", 
        ylab = "Frequency",
        ylim = c(0, 150),
        xlim = c(0, 12),
        col=c("darkcyan", "darkturquoise","darkseagreen2", "darkolivegreen4"),
        names = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"),
        cex.lab = 10,
        cex.main = 13,
        cex.axis = 10,
        cex.names = 7)
legend(x = 7.5, y = 100, c("Ages 7-9", "Ages 10-12", "Ages 13-15"), 
       fill = c("darkcyan", "darkturquoise","darkseagreen2"), cex = 8)
counts <- table(Standard_Sample_P$Sex,Standard_Sample_P$PCIAT_Range)
barplot(counts,
        family = "serif",
        main ="Distribution of IAT Scores\nby Sex (Parent-Report)\n",
        xlab ="IAT Scores (Parent-Report)", 
        ylab = "Frequency",
        ylim = c(0, 150),
        xlim = c(0, 12),
        col=c("darkcyan", "darkturquoise"),
        names = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"),
        cex.lab = 10,
        cex.main = 13,
        cex.axis = 10,
        cex.names = 7)
legend(x = 9, y = 100, c("Male", "Female"), 
       fill = c("darkcyan", "darkturquoise"), cex = 8)
dev.off()

#svg
#SR
svg(file="Figures/IAT_Combined_Hist.svg", width = 20, height = 10)
par(mar = c(6, 6, 8, 2)+0.1, mgp = c(3, 1, 0), mfrow = c(1, 2), family="serif")
counts <- table(Standard_Sample_SR$Age_Group, Standard_Sample_SR$IAT_Range)
barplot(counts,
        family = "serif",
        main ="Distribution of IAT Scores\nby Age Group (Self-Report)\n",
        xlab ="IAT Scores (Self-Report)", 
        ylab = "Frequency",
        ylim = c(0, 150),
        xlim = c(0, 12),
        col=c("darkcyan", "darkturquoise","darkseagreen2", "darkolivegreen4"),
        names = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"),
        cex.lab = 2,
        cex.main = 2,
        cex.axis = 2,
        cex.names = 2)
legend(x = 6.5, y = 120, c("Ages 7-9", "Ages 10-12", "Ages 13-15"), 
       fill = c("darkcyan", "darkturquoise","darkseagreen2"), cex = 2)
counts <- table(Standard_Sample_SR$Sex, Standard_Sample_SR$IAT_Range)
barplot(counts,
        family = "serif",
        main ="Distribution of IAT Scores\nby Sex (Self-Report)\n",
        xlab ="IAT Scores (Self-Report)", 
        ylab = "Frequency",
        ylim = c(0, 150),
        xlim = c(0, 12),
        col=c("darkcyan", "darkturquoise"),
        names = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"),
        cex.lab = 2,
        cex.main = 2,
        cex.axis = 2,
        cex.names = 2)
legend(x = 6.5, y = 120, c("Male", "Female"), 
       fill = c("darkcyan", "darkturquoise"), cex = 2)
dev.off()
#P
svg(file="Figures/PCIAT_Combined_Hist.svg", width = 20, height = 10)
par(mar = c(6, 6, 8, 2)+0.1, mgp = c(3, 1, 0), mfrow = c(1, 2), family = "serif")
counts <- table(Standard_Sample_P$Age_Group, Standard_Sample_P$PCIAT_Range)
barplot(counts,
        family = "serif",
        main ="Distribution of IAT Scores\nby Age Group (Parent-Report)\n",
        xlab ="IAT Scores (Parent-Report)", 
        ylab = "Frequency",
        ylim = c(0, 150),
        xlim = c(0, 12),
        col=c("darkcyan", "darkturquoise","darkseagreen2", "darkolivegreen4"),
        names = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"),
        cex.lab = 2,
        cex.main = 2,
        cex.axis = 2,
        cex.names = 2)
legend(x = 6.5, y = 120, c("Ages 7-9", "Ages 10-12", "Ages 13-15"), 
       fill = c("darkcyan", "darkturquoise","darkseagreen2"), cex = 2)
counts <- table(Standard_Sample_P$Sex,Standard_Sample_P$PCIAT_Range)
barplot(counts,
        family = "serif",
        main ="Distribution of IAT Scores\nby Sex (Parent-Report)\n",
        xlab ="IAT Scores (Parent-Report)", 
        ylab = "Frequency",
        ylim = c(0, 150),
        xlim = c(0, 12),
        col=c("darkcyan", "darkturquoise"),
        names = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"),
        cex.lab = 2,
        cex.main = 2,
        cex.axis = 2,
        cex.names = 2)
legend(x = 6.5, y = 120, c("Male", "Female"), 
       fill = c("darkcyan", "darkturquoise"), cex = 2)
dev.off()
#################stacked bar plots with sex
#SR
pdf(file="IAT_Hist_Sex.pdf", width = 80, height = 50)
par(mar = c(25, 25, 35, 13)+0.4, mgp = c(18, 7, 0))
counts <- table(IAT_age_sex$Sex, IAT_age_sex$IAT_Range)
barplot(counts, 
        main ="Distribution of IAT Scores\nby Sex (Self-Report)\n",
        xlab ="IAT Scores (Self-Report)", 
        ylab = "Frequency",
        ylim = c(0, 250),
        xlim = c(0, 12),
        col=c("darkcyan", "darkturquoise"),
        names = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"),
        cex.lab = 10,
        cex.main = 13,
        cex.axis = 10,
        cex.names = 7)
legend(x = 9, y = 200, c("Male", "Female"), 
       fill = c("darkcyan", "darkturquoise"), cex = 8)
dev.off()
#P
pdf(file="PCIAT_Hist_Sex.pdf", width = 80, height = 50)
par(mar = c(25, 25, 35, 13)+0.4, mgp = c(18, 7, 0))
counts <- table(IAT_age_sex$Sex, IAT_age_sex$PCIAT_Range)
barplot(counts, 
        main ="Distribution of IAT Scores\nby Sex (Parent-Report)\n",
        xlab ="IAT Scores (Parent-Report)", 
        ylab = "Frequency",
        ylim = c(0, 250),
        xlim = c(0, 12),
        col=c("darkcyan", "darkturquoise"),
        names = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"),
        cex.lab = 10,
        cex.main = 13,
        cex.axis = 10,
        cex.names = 7)
legend(x = 9, y = 200, c("Male", "Female"), 
       fill = c("darkcyan", "darkturquoise"), cex = 8)
dev.off()
