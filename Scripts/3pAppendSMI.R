####Define SMI at trial start date####

#Call start script
source("./Scripts/1aSetUp.R")

####Load 2023 data####
load(paste0(ProjectDir, "/Data/CPRD_BMI.rdata"))

load(paste0(ProjectDir, "/StatinExtracts/SMI2023.Rdata"))

PatSMI<-subset(PatSMI, patid %in% Trial$patid)

Dates<-select(Trial, patid, FirstCCBARB)
PatSMI<-merge(x=PatSMI, y=Dates, by="patid", all.x=TRUE, all.y=FALSE)
length(unique(PatSMI$patid))

PatSMI<-PatSMI%>%
  subset(eventdate<=FirstCCBARB)%>%
  mutate(Schiz=case_when(Group=="schizophrenia" ~ 1,
                         TRUE ~ 0),
         Bipolar=case_when(Group=="bipolar" ~ 1,
                         TRUE ~ 0),
         Other=case_when(Group=="other psychosis" ~ 1,
                           TRUE ~ 0))%>%
  group_by(patid)%>%
  mutate(EverSchiz=sum(Schiz, na.rm=TRUE), EverBipolar=sum(Bipolar, na.rm=TRUE), EverOther=sum(Other, na.rm=TRUE))%>%
  mutate(SMIDiag=case_when(EverSchiz>0 ~ "Schiz",
                                 EverBipolar>0 ~ "Bipolar",
                                 EverOther>0 ~ "Other",
                                 TRUE ~ "Undefined"))
  
PatSMI<-select(PatSMI, patid, SMIDiag)
PatSMI<-distinct(PatSMI)
length(unique(PatSMI$patid))
table(PatSMI$SMIDiag)

Trial<-merge(x=Trial, y=PatSMI, by="patid", all.x=TRUE, all.y=FALSE)

save(Trial, file=paste0(ProjectDir, "/Data/Trial_SMI.rdata"))  
