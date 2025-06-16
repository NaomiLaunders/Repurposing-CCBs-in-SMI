####First run of cleaning the cohort####

#Call start script
source("./Scripts/1aSetUp.R")

#Load full clean data

load(paste0(ProjectDir, "/Data/CPRD_Clean.rdata"))

####Check dates####

####Drop invalid data####
#Create a table to store the results of exclusions so can easily create a flow chart for pubs
Consort<-data.frame("Count" = length(CPRD$patid), "Description" = c("Original"))

##1/ Age
#Drop those under 18 at diagnosis 
CPRD<-subset(CPRD,!is.na(CPRD$diagnosis_date))
Consort1<-data.frame("Count" =length(CPRD$patid), "Description" = c("Under 18 at SMI diagnosis"))
Consort<- rbind(Consort,Consort1)
#Drop those over 100 at index
CPRD<-subset(CPRD,(year(CPRD$SMIEv)-CPRD$yob<=100))
Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Over 100 at first SMI evidence"))
Consort<- rbind(Consort,Consort1)

##2/ End before index date
CPRD<-subset(CPRD,(end>=SMIEv))
Consort1<-data.frame("Count" =length(CPRD$patid), "Description" = c("Exit before first SMI evidence"))
Consort<- rbind(Consort,Consort1)

##3/Ends before 2000
CPRD<-subset(CPRD, CPRD$end>=as.Date("2000-01-01"))
Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Ends before 2000"))
Consort<- rbind(Consort,Consort1)

##4/starts after 2019
CPRD<-subset(CPRD, CPRD$enter<=as.Date("2019-12-31"))
               Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Enters cohort after 2019"))
               Consort<- rbind(Consort,Consort1)

##5/ Drop any that are remaining with start after end 
StartEnd<-subset(CPRD, CPRD$enter>CPRD$end)
StartEnd<-select(StartEnd, patid, enter, end, lcd, regenddate, Date100, deathdate, regstartdate, Date18, diagnosis_date)
CPRD<-subset(CPRD, CPRD$enter<=CPRD$end)
Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Main start after Main end"))
Consort<- rbind(Consort,Consort1)

##6/ Drop those who never have an antihypertensive before end of follow up
CPRD<-subset(CPRD, !is.na(FirstAntiHypDate) & FirstAntiHypDate<=end)
Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Never prescribed anti-hypertensives"))
Consort<- rbind(Consort,Consort1)

##7/ Drop those never prescribed psychotropic medications before end of follow up
CPRD<-subset(CPRD, !is.na(FirstPsychDate) & FirstPsychDate<=end)
Consort1<- data.frame("Count" =length(CPRD$patid), "Description" = c("Never prescribed psychotropics"))
Consort<- rbind(Consort,Consort1)


####Save numbers for flow chart####

#Generate consort
Consort
Consort<-Consort %>%
  mutate (Lag = lag(Count), Diff = Lag-Count)
Consort<-select(Consort, -Lag)

save(Consort, file = "Outputs/FlowChart.Rdata")
write.csv(Consort, file = "Outputs/FlowChart.csv")

####What is the time period of the study?####
summary(CPRD$enter)
summary(CPRD$end)
summary(CPRD$diagnosis_date)
summary(CPRD$FirstAntiHypertensiveDate)
summary(CPRD$SMIEv)
summary(CPRD$FirstPsychDate)

table(CPRD$source)
table(CPRD$FirstDiag)
table(CPRD$LastDiag)

####Other useful variables####
CPRD$TotalFU<-as.numeric(CPRD$end-CPRD$enter)/365.25
summary(CPRD$TotalFU)

CPRD$StudyFU<-as.numeric(CPRD$end-CPRD$FirstAntiHypDate)/365.25
summary(CPRD$StudyFU)

CPRD$StudyBaseline<-as.numeric(CPRD$FirstAntiHypDate-CPRD$enter)/365.25
summary(CPRD$StudyBaseline)

CPRD$TimeSinceDiag<-CPRD$end-CPRD$diagnosis_date
CPRD$TimeSinceDiag<-as.numeric(CPRD$TimeSinceDiag/365.25)
summary(CPRD$TimeSinceDiag)

CPRD$TimeSincePsych<-CPRD$end-CPRD$FirstPsychDate
CPRD$TimeSincePsych<-as.numeric(CPRD$TimeSincePsych/365.25)
summary(CPRD$TimeSincePsych)

CPRD$TimeSinceAntiHyp<-CPRD$end-CPRD$FirstAntiHypDate
CPRD$TimeSinceAntiHyp<-as.numeric(CPRD$TimeSinceAntiHyp/365.25)
summary(CPRD$TimeSinceAntiHyp)

####Sort deaths and check dates####
summary(CPRD$deathdate)

#index dates
summary(CPRD$diagnosis_date)
summary(CPRD$enter)
summary(CPRD$end)
summary(CPRD$deathdate)

#Save clean file
save(CPRD, file = paste0(ProjectDir, "/Data/CleanCPRD.Rdata"))

#Clear environment

rm(list = ls(all.names = TRUE))
