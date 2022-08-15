# Package info
install.packages('tidyverse')
install.packages('ggpubr')
install.packages('rstatix')
install.packages('rcompanion')
library(rcompanion)
library(multcompView)
library(tidyverse)
library(ggpubr)
library(rstatix)

#################### 
### CTmax Table ####
####################

#Import .csv and call 'CTmaxTable'

# Find all the groups in the dataset using 'unique' with your column
unique(CTmaxTable$Ramping)
# Check what class of data you have using class, this tells you if it's a number of a factor or a character or others
class(CTmaxTable$Treatment_Ramping)

# Set your data to a factor
CTmaxTable$Ramping <- factor(CTmaxTable$Ramping, levels = c("0.06", "0.25", "0.5"))
CTmaxTable$Treatment_Ramping <- factor(CTmaxTable$Treatment_Ramping, levels = c("Control_0.06","AcuteTRT18_0.06","AcuteTRT22_0.06","ChronicTRT18_0.06",
                                                                                "ChronicTRT22_0.06","variable_0.06","Control_0.25","AcuteTRT18_0.25", 
                                                                                "AcuteTRT22_0.25", "ChronicTRT18_0.25", "ChronicTRT22_0.25", 
                                                                                "variable_0.25","Control_0.5","AcuteTRT18_0.5","AcuteTRT22_0.5","ChronicTRT18_0.5",
                                                                                "ChronicTRT22_0.5","variable_0.5","Dessication_0.06","Dessication_0.25","Dessication_0.5"))

#To look at individual group comparison statistics with Wilcoxon
pwc <- CTmaxTable %>%
  wilcox_test(CTmax ~ Treatment_Ramping, paired = TRUE, p.adjust.method = "bonferroni")
WilcoxonScores <- pwc
write.csv(WilcoxonScores, "CTmaxWilcoxonscores.csv")

# To find multiple comparrison letter groupings
CTmax <- CTmaxTable$CTmax
Treatment_Ramping <- CTmaxTable$Treatment_Ramping
Table = suppressWarnings(pairwise.wilcox.test(CTmax, Treatment_Ramping))
Table2 = Table$p.value
Table2
Table3 = fullPTable(Table2)
Table3
multcompLetters(Table3)
write.csv(Table3, "CTMaxTableWilcoxonTextComparisons.csv")

# Box plot with everything together
p <- ggplot(CTmaxTable, aes(x=Treatment, y=CTmax, fill=Ramping))
p + geom_boxplot() + theme_bw()


##################
### HKDT Table ###
##################

#Import .csv and call 'HKDTTable'

# Set your data to a factor
HKDTTable$Temperature <- as.numeric()
HKDTTable$Treatment <- factor(HKDTTable$Treatment, levels = c("Control",
                                                              "AcuteTRT18",
                                                              "AcuteTRT22",
                                                              "ChronicTRT18",
                                                              "ChronicTRT22",
                                                              "Variable",
                                                              "Dessication"))

#To look at individual group comparison statistics with Wilcoxon
pwc <- HKDTTable %>%
  wilcox_test(HKDT ~ Treatment, paired = TRUE, p.adjust.method = "bonferroni")
WilcoxonScores <- pwc
write.csv(WilcoxonScores, "HKDTWilcoxonscores.csv")

# To find multiple comparison letter groupings
HKDT <- HKDTTable$HKDT
HKDT_Treatment <- HKDTTable$Treatment
Table = suppressWarnings(pairwise.wilcox.test(HKDT, HKDT_Treatment))
Table2 = Table$p.value
Table2
Table3 = fullPTable(Table2)
Table3
multcompLetters(Table3)
write.csv(Table3, "HKDT_TableWilcoxonTextComparisons.csv")

# Box plot with everything together
p <- ggplot(HKDTTable, aes(x=Treatment, y=HKDT))
p + geom_boxplot() + theme_bw()


##################################
### TREATMENT GROUP STATISTICS ###
##################################

# Subset data by RAMPING rates
Ramp0.06 <- subset(CTmaxTable, Ramping == 0.06) 
Ramp0.25 <- subset(CTmaxTable, Ramping == 0.25) 
Ramp0.5 <- subset(CTmaxTable, Ramping == 0.5) 

# To find multiple comparison letter groupings FOR 0.06 RAMPING ONLY
CTmax <- Ramp0.06$CTmax
Treatment_Ramping <- Ramp0.06$Treatment_Ramping
Table = suppressWarnings(pairwise.wilcox.test(CTmax, Treatment_Ramping))
Table2 = Table$p.value
Table2
Table3 = fullPTable(Table2)
Table3
multcompLetters(Table3)

# To find multiple comparison letter groupings FOR 0.25 RAMPING ONLY
CTmax <- Ramp0.25$CTmax
Treatment_Ramping <- Ramp0.25$Treatment_Ramping
Table = suppressWarnings(pairwise.wilcox.test(CTmax, Treatment_Ramping))
Table2 = Table$p.value
Table2
Table3 = fullPTable(Table2)
Table3
multcompLetters(Table3)

# To find multiple comparison letter groupings FOR 0.25 RAMPING ONLY
CTmax <- Ramp0.5$CTmax
Treatment_Ramping <- Ramp0.5$Treatment_Ramping
Table = suppressWarnings(pairwise.wilcox.test(CTmax, Treatment_Ramping))
Table2 = Table$p.value
Table2
Table3 = fullPTable(Table2)
Table3
multcompLetters(Table3)


############################### 
### RAMPING RATE STATISTICS ###
###############################

# Subset your data by TREATMENT 
AT18 <- subset(CTmaxTable, Treatment == "AcuteTRT18")  
AT22 <- subset(CTmaxTable, Treatment == "AcuteTRT22") 
CT18 <- subset(CTmaxTable, Treatment == "ChronicTRT18") 
CT22 <- subset(CTmaxTable, Treatment == "ChronicTRT22") 
CTRL <- subset(CTmaxTable, Treatment == "Control") 
VAR <- subset(CTmaxTable, Treatment == "variable")

# To find multiple comparison letter groupings FOR 0.06 RAMPING ONLY
CTmax <- AT22$CTmax
Treatment_Ramping <- AT22$Treatment_Ramping
Table = suppressWarnings(pairwise.wilcox.test(CTmax, Treatment_Ramping))
Table2 = Table$p.value
Table2
Table3 = fullPTable(Table2)
Table3
multcompLetters(Table3)
