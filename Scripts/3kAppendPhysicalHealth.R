####Append physical health####

#Call start script
source("./Scripts/1aSetUp.R")

####Load 2023 data####
load(paste0(ProjectDir, "/Data/Trial_ethn.rdata"))

load(paste0(ProjectDir, "/StatinExtracts/PhysicalAll.Rdata"))

PatPhysicalAll<-subset(PatPhysicalAll, patid %in% Trial$patid)

#Did they occur before date of birth
YoB<-select(Trial, patid, yob)
PatPhysicalAll<-merge(x=PatPhysicalAll, y=YoB, by="patid", all.x=TRUE, all.y=FALSE)
PatPhysicalAll<-subset(PatPhysicalAll, yob<year(eventdate))

table(PatPhysicalAll$cat, useNA="ifany")

#Already got diabetes and excluded so ignore
MI<-subset(PatPhysicalAll, cat=="MI")
Cerebrovascular<-subset(PatPhysicalAll, cat=="Cerebrovascular")
CHF<-subset(PatPhysicalAll, cat=="CHF")

#create first diagnosis variables
MI<-MI%>%
  group_by(patid)%>%
  summarise(FirstMI=min(eventdate, na.rm=TRUE))

Cerebrovascular<-Cerebrovascular%>%
  group_by(patid)%>%
  summarise(FirstCerebrovascular=min(eventdate, na.rm=TRUE))

CHF<-CHF%>%
  group_by(patid)%>%
  summarise(FirstCHF=min(eventdate, na.rm=TRUE))

#Append first diagnosis dates to trial table
Trial<-merge(x=Trial, y=MI, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=Cerebrovascular, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=CHF, by="patid", all.x=TRUE, all.y=FALSE)

#Create variables for first diagnosis at statin or at AP
Trial<-Trial%>%
  mutate(GPMI = case_when(FirstMI<=FirstCCBARB  ~ 1,
                              TRUE ~ 0),
         GPCerebrovascular = case_when(FirstCerebrovascular<=FirstCCBARB ~ 1,
                              TRUE ~ 0),
         GPCHF = case_when(FirstCHF<=FirstCCBARB ~ 1,
                              TRUE ~ 0))

Trial<-Trial%>%
  mutate(AllMI = case_when(GPMI==1|HospMI==1  ~ 1,
                          TRUE ~ 0),
         AllCerebrovascular = case_when(GPCerebrovascular==1|HospCereb==1 ~ 1,
                                       TRUE ~ 0),
         AllCHF = case_when(GPCHF==1|HospHF==1 ~ 1,
                           TRUE ~ 0))


table(Trial$AllMI)
table(Trial$AllCerebrovascular)
table(Trial$AllCHF)

save(Trial, file = paste0(ProjectDir, "/Data/CPRD_Phys.rdata"))

         




