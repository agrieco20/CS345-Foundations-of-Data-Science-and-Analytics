# Anthony Grieco
# Foundations of Data Science
# 2/21/2024

library(tidyverse)

# Read in the Data
BillsRnk <- read.table("BillsData.csv", sep = ",")

# Declares the header columns of BillsRnk
colnames(BillsRnk)<- BillsRnk[1,]
BillsRnk <- BillsRnk[-1,]

# Resets Columns so that all the Data they hold are automatically stored as integers instead of as characters
BillsRnk$W <- as.numeric(BillsRnk$W)
BillsRnk$L <- as.numeric(BillsRnk$L)
BillsRnk$Off_PtsScoredRnk <- as.numeric(BillsRnk$Off_PtsScoredRnk)
BillsRnk$Def_PtsAllowedRnk <- as.numeric(BillsRnk$Def_PtsAllowedRnk)

# Calculates the Offense and Defense Rank Differential between the Buffalo Bills from 1973 - 2023
# (A Negative Number means that the Bills' Offense was considered to be the stronger unit in the given year. A positive number means that the Bills' Defense was considered to be the stronger unit in the given year. The further the differential, the severity as to how much stronger the given unit was over its compliment)
BillsRnk <- BillsRnk %>% mutate(RnkDiff = BillsRnk$Off_PtsScoredRnk - BillsRnk$Def_PtsAllowedRnk)

# Calculates the Win Percentage for the Bills in the given year (1973 - 2023) 
BillsRnk <- BillsRnk %>% mutate(WinPerc = as.double(BillsRnk$W / (BillsRnk$W + BillsRnk$L)))

# Calculates the Mean Number of Regular Season Wins that the Buffalo Bills organization has had a year from 1973 to 2023
mean(BillsRnk$W)

# Splits the Number of Regular Season Wins that the Buffalo Bills organization has has from 1973 to 2023 into Quartiles 
quantile(BillsRnk$W, prob = c(0,0.25,0.5,0.75,1))

# Creates Scatterplot of the Bills' yearly Win Percentage compared to how their Offensive and Defensive Unit Ranking line up with one another in an attempt to see whether it has been more important for the Bills to have a stronger Offense or Defense in order to win games historically
BillsPlot <- ggplot(data=BillsRnk, aes(x=BillsRnk$WinPerc, y=BillsRnk$RnkDiff)) + 
  geom_point()
BillsPlot

# Calculates the Covariance of the values being measured in the above Scatterplot
cov(BillsRnk$WinPerc, BillsRnk$RnkDiff)

# Calculates the Correlation of the values being measured in the above Scatterplot
cor(BillsRnk$WinPerc, BillsRnk$RnkDiff)

# Creates a Linear Model using the data being studied in the Scatterplot (above)
BillsPlot + geom_smooth(method=lm, se=FALSE) + labs(x= "Bills Win Percentage", y="Offense vs Defense Point Rank Differential")

# Returns the Coefficients Used in the above the Linear Model
regCo <- lm(BillsRnk$RnkDiff ~ BillsRnk$WinPerc)
regCo$coefficients

#Conclusions: Although not it doesn't have a significant correlation, the data would appear to indicate that when the Bills (1973-2023) had a stronger ranked Offense (scored more points) rather than a stronger ranked Defense (gave up fewer points) they were more likely to win games