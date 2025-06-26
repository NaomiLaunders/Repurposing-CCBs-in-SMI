####Define antihypertensive dose####

#Call start script
source("./Scripts/1aSetUp.R")


####Load 2023 data####
load(paste0(ProjectDir, "/Data/Trial_SMI.rdata"))
load(paste0(ProjectDir,"/Data/ValidCCB.Rdata"))

#Limit to those in our cohort
Valid<-subset(Valid, patid %in% Trial$patid)

#Bring in dates
Dates<-select(Trial, patid, FirstCCBARB)
Valid<-merge(x=Valid, y=Dates, by="patid", all.x=TRUE, all.y=FALSE)

#Find valid of first CCB trial
CCB<-Valid%>%
  subset(issuedate==FirstCCBARB)%>%
  group_by(patid)%>%
  select(patid, Name, AntiHypDose, ExactDose=TotalDose)%>%
  distinct()%>%
  mutate(n=n())

length(which(CCB$n>1))
table(CCB$Name)

Check<-subset(CCB, n>1)

CCB<-select(CCB, -n, -Name)
CCB<-distinct(CCB)

Trial<-merge(x=Trial, y=CCB, by="patid", all.x=TRUE, all.y=FALSE)

prop.table(table(Trial$AntiHypDose, Trial$Drug, useNA="ifany"),2)

####Investigate missing Nifedipine dosage####
load(paste0(ProjectDir, "/Extracts/CCBDose_interim.Rdata"))
#Find valid of first CCB trial
Check<-Dur%>%
  subset(patid %in% Trial$patid)%>%
  subset(Name =="Nifedipine")

Check<-merge(x=Check, y=Dates, by="patid", all.x=TRUE, all.y=FALSE)

Check<-Check%>%
  subset(issuedate==FirstCCBARB)%>%
  distinct()%>%
  mutate(n=n())

ValidTrialDose<-subset(Trial, !is.na(ExactDose))
Check$Valid<-0
Check$Valid[Check$patid %in% ValidTrialDose$patid]<-1

Check<-subset(Check, Valid==0)

####CHeck against others####
Other<-Dur%>%
  subset(patid %in% Trial$patid)%>%
  subset(Name !="Nifedipine")

Other<-merge(x=Other, y=Dates, by="patid", all.x=TRUE, all.y=FALSE)

Other<-Other%>%
  subset(issuedate==FirstCCBARB)%>%
  distinct()%>%
  mutate(n=n())

ValidTrialDose<-subset(Trial, !is.na(ExactDose))
Other$Valid<-0
Other$Valid[Other$patid %in% ValidTrialDose$patid]<-1

Other<-subset(Other, Valid==0)

####It is only nifedipine which has low dose prescriptions - assume that these are probably for Raynauds?####
load("Outputs/FlowChartTrial.Rdata")
FinalConsort<-select(FinalConsort, -Diff)

#No Raynauds - exclude those at low dose
LowDose<-subset(Check, TotalDose1!=0 & TotalDose1<=15|TotalDose2!=0 & TotalDose2<=15|TotalDose3!=0 & TotalDose3<=15)
Trial<-subset(Trial, !(patid %in% LowDose$patid))
Consort1a<-data.frame("Count" =length(Trial$patid), "Description" = c("Suspected raynauds based on prescription dose"))
FinalConsort<- rbind(FinalConsort,Consort1a)

FinalConsort<-FinalConsort %>%
  mutate (Lag = lag(Count), Diff = Lag-Count)%>%
  select(-Lag)

save(FinalConsort, file = "Outputs/FlowChartTrial_final.Rdata")
write.csv(FinalConsort, file = "Outputs/FlowChartTrial_final.csv")

save(Trial, file=paste0(ProjectDir, "/Data/Trial_CCB.rdata"))
