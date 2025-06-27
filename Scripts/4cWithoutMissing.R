####Sort out missing data####

#Call start script
source("./Scripts/1aSetUp.R")

####Load trials####
load(paste0(ProjectDir, "/Data/Trial_analysis.rdata"))

####If CCB dose is NA use median####
length(which(is.na(Trial$ExactDose)))

Median<-Trial%>%
  group_by(Drug)%>%
  summarise(Median = median(ExactDose, na.rm=TRUE))
    
tapply(Median$Median,Median$Drug, summary)

Median$Group[Median$Drug=="Amlodipine"]<-"Starting"
  Median$Group[Median$Drug=="Felodipine"]<-"Starting"
  Median$Group[Median$Drug=="Lacidipine"]<-"Starting"
  Median$Group[Median$Drug=="Lercanidipine"]<-"Starting"
  Median$Group[Median$Drug=="Nicardipine"]<-"Maintenance"
  Median$Group[Median$Drug=="Nifedipine"]<-"Starting"

Trial<-merge(x=Trial, y=Median, by="Drug", all.x=TRUE, all.y=FALSE) 

Trial$ExactDose[is.na(Trial$ExactDose)]<-Trial$Median[is.na(Trial$ExactDose)]
Trial$AntiHypDose[is.na(Trial$AntiHypDose)]<-Trial$Group[is.na(Trial$AntiHypDose)]
Trial<-select(Trial, -Median, -Group)

####If time since hypertension is NA set to zero####
summary(Trial$TimeSinceHyp)
summary(Trial$TimeSinceHighBP)
summary(Trial$TimeSinceAnyHyp)

Trial$TimeSinceAnyHyp[is.na(Trial$TimeSinceAnyHyp)]<-0

summary(Trial$TimeSinceAnyHyp)

####If ethnicity is unknown set to missing####
table(Trial$ethnicity,useNA="ifany")
Trial$ethnicity[Trial$ethnicity=="Unknown"]<-NA
Trial$ethnicity<-as.character(Trial$ethnicity)
Trial$ethnicity<-as.factor(Trial$ethnicity)

####IMD - if patient level missing take practice level####
Trial$FullIMD<-Trial$PatIMD
Trial$FullIMD[is.na(Trial$FullIMD)]<-Trial$PracIMD[is.na(Trial$FullIMD)]

table(Trial$FullIMD, useNA="ifany")

prop.table(table(Trial$FullIMD, useNA="ifany"))*100

save(Trial, file=paste0(ProjectDir, "/Data/Trial_final.rdata"))
