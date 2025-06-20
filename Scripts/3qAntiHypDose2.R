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

#Make the one with both, moderate
CCB$AntiHypDose[CCB$n>1]<-"Med"

CCB<-CCB%>%
  group_by(patid)%>%
  mutate(ExactDose=case_when(n>1 ~ 10,
                                  TRUE ~ ExactDose))
Check<-subset(CCB, n>1)

CCB<-select(CCB, -n, -Name)
CCB<-distinct(CCB)

Trial<-merge(x=Trial, y=CCB, by="patid", all.x=TRUE, all.y=FALSE)

save(Trial, file=paste0(ProjectDir, "/Data/Trial_CCB.rdata"))
