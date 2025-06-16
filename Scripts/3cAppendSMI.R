####Append SMI diagnoses to the main cohort####

#Call start script
source("./Scripts/1aSetUp.R")

####Load 2023 data####
load(paste0(ProjectDir, "/Data/CPRD_DiabDem.rdata"))

#Load new obs
load(paste0(ProjectDir, "/StatinExtracts/SMI2023.Rdata"))

#Check unique patients
Unique<-select(PatSMI, patid)
Unique<-distinct(Unique, patid)
length(which(Unique$patid %in% CPRD$patid))

#Remove "died" and recalculate for this study
CPRD<-select(CPRD, -died)

#Check eligible diagnosis dates and index dates
summary(CPRD$diagnosis_date)
summary(CPRD$regstartdate)
summary(CPRD$regenddate)
tapply(CPRD$regenddate, CPRD$source, summary)

####Generate new fields/missing data####

#dob: Create a date of birth from year. Set at 01/01 so technically including patients who were diagnosed in the year that they turned 18.
CPRD$dob<-paste0("01/01/", CPRD$yob)
CPRD$dob<-as.Date(CPRD$dob, "%d/%m/%Y")

#Date turned 18 and 100
CPRD$Date100<-CPRD$dob+years(100)
CPRD$Date18<-CPRD$dob+years(18)

#FirstEvidenceofSMI
length(which(year(CPRD$diagnosis_date)-CPRD$yob<18))
length(which(year(CPRD$FirstPsychDate)-CPRD$yob<18))

Dates<-select(CPRD, patid, diagnosis_date, FirstPsychDate, yob)

SMI<-merge(x=PatSMI, y=Dates, by="patid", all.x=TRUE, all.y=FALSE)

#Drop any that are before 18, but keep those patients and only include their diagnosis that they receive at 18
#Note, this is a slight change from the statins study to include more patients

SMI<-SMI%>%
  mutate(AgeAtThis = year(eventdate)-yob)%>%
  subset(AgeAtThis>=18)%>%
  group_by(patid)%>%
  mutate(FirstSMI = min(eventdate, na.rm=TRUE))%>%
  ungroup()

length(unique(SMI$patid))

Check<-subset(SMI, diagnosis_date!=FirstSMI)
Check<-subset(Check, FirstSMI==eventdate)

SMI<-select(SMI, patid, FirstSMI)
SMI<-distinct(SMI)

CPRD<-merge(x=CPRD, y=SMI, by="patid", all.x=TRUE, all.y=TRUE)

#If evidence before 18, just take diagnosis
CPRD<-CPRD%>%
  mutate(diagnosis_date = FirstSMI)%>%
  select(-FirstSMI)%>%
  mutate(SMIEv = case_when(is.na(diagnosis_date) ~ NA,
                           year(FirstPsychDate) - yob <=18 ~ diagnosis_date,
                           TRUE ~ pmin(diagnosis_date, FirstPsychDate, na.rm=TRUE)))


#Note, some are now NA - that's fine they will be dropped later
length(which(is.na(CPRD$SMIEv)))
length(which(year(CPRD$SMIEv)-CPRD$yob<18))

#Create a enter, start and end date for each patient: For this study 01/01/00 to 31/12/19
CPRD$enter<-pmax(CPRD$regstartdate, CPRD$SMIEv, as.Date("2000-01-01"), na.rm=TRUE)
CPRD$end<-pmin(as.Date("2019-12-31"), CPRD$regenddate, CPRD$deathdate, CPRD$lcd, CPRD$Date100, na.rm=TRUE)

Dates<-subset(CPRD, enter>end)

Dates<-select(Dates, patid, regstartdate,Date18, diagnosis_date, regenddate, deathdate, lcd, Date100, enter, end, yob)

summary(CPRD$enter)
summary(CPRD$end)

summary(CPRD$lcd)
summary(CPRD$regenddate)

#Age at enter
CPRD$AgeAtEnter<-as.numeric(CPRD$enter-CPRD$dob)/365.25
summary(CPRD$AgeAtEnter)

#Age at end
CPRD$AgeAtEnd<-as.numeric(CPRD$end-CPRD$dob)/365.25
summary(CPRD$AgeAtEnd)

#Age at antihyp
CPRD$AgeAtAntiHyp<-as.numeric(CPRD$FirstAntiHypDate-CPRD$dob)/365.25
summary(CPRD$AgeAtAntiHyp)

#Age at SMI diagnosis
CPRD$AgeAtDiag<-as.numeric(CPRD$diagnosis_date-CPRD$dob)/365.25
summary(CPRD$AgeAtDiag)

#Age at first psych prescription
CPRD$AgeAtPsych<-as.numeric(CPRD$FirstPsychDate-CPRD$dob)/365.25
summary(CPRD$AgeAtPsych)

#Age at SMI
CPRD$AgeAtSMIEv<-as.numeric(CPRD$SMIEv-CPRD$dob)/365.25
summary(CPRD$AgeAtSMIEv)

save(CPRD, file=paste0(ProjectDir, "/Data/CPRD_Clean.rdata"))  
