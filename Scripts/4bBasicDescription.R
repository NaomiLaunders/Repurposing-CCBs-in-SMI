####Set trial####

#Call start script
source("./Scripts/1aSetUp.R")

####Load trials####
load(paste0(ProjectDir, "/Data/Trial_analysis.rdata"))

####Basic descriptives####
names(Trial)

#Table 1 - Cohort basics
MyVars<-c(names(Trial))
MyVars<-c(MyVars[c(3:68)])

Table1<-CreateTableOne(vars=MyVars,  data=Trial, includeNA = TRUE)
print(Table1, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = TRUE)

Table1Exp <- print(Table1, printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = TRUE, showAllLevels = TRUE)

write.csv(Table1Exp, file = "Outputs/TrialBasic.csv")

#Table 1 - stratified by arm
Table2<-CreateTableOne(vars=MyVars,  strata = "Arm", data=Trial, includeNA = TRUE)
print(Table2, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = TRUE)

Table2Exp <- print(Table2, printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = TRUE, showAllLevels = TRUE)

write.csv(Table2Exp, file = "Outputs/TrialBasicStrat.csv")

#Table 1 - stratified by drug
Table3<-CreateTableOne(vars=MyVars,  strata = "Drug", data=Trial, includeNA = TRUE)
print(Table3, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = TRUE)

Table3Exp <- print(Table3, printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = TRUE, showAllLevels = TRUE)

write.csv(Table3Exp, file = "Outputs/TrialBasicDrug.csv")
