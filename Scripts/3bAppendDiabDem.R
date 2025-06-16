####Append diabetes and dementia to the main cohort####

#Call start script
source("./Scripts/1aSetUp.R")


####Load 2023 data####
load(paste0(ProjectDir, "/Data/CPRD_AP.Rdata"))

load(paste0(ProjectDir, "/StatinExtracts/PhysicalAll.Rdata"))

PatPhysicalAll<-subset(PatPhysicalAll, patid %in% CPRD$patid)

#Did they occur before date of birth
YoB<-select(CPRD, patid, yob)
PatPhysicalAll<-merge(x=PatPhysicalAll, y=YoB, by="patid", all.x=TRUE, all.y=FALSE)
PatPhysicalAll<-subset(PatPhysicalAll, yob<year(eventdate))

table(PatPhysicalAll$cat, useNA="ifany")

#Sort heirarchy of diabetes

Diab<-subset(PatPhysicalAll, grepl("diab", cat, ignore.case=TRUE))

Diab<-Diab%>%
  group_by(patid)%>%
  mutate(EverT2=sum(cat=="Diabetes_Type 2"), EverT1 = sum(cat=="Diabetes_Type 1"), EverGest=sum(grepl("gest", cat, ignore.case=TRUE)), EverOther=sum(cat=="Diabetes_Other"))%>%
  mutate(Type=case_when(EverT2>0 ~ "T2",
                        EverT1>0 ~ "T1",
                        EverGest>0 ~ "Gest",
                        EverOther>0 ~ "Other",
                        TRUE ~ "Unknown"))

table(Diab$Type, useNA="ifany")

Diab<-subset(Diab, Type=="T2"|Type=="Unknown")

#create first diagnosis variables
Diab<-Diab%>%
  group_by(patid)%>%
  summarise(FirstDiab=min(eventdate, na.rm=TRUE))

#Append first diagnosis dates to trial table
CPRD<-merge(x=CPRD, y=Diab, by="patid", all.x=TRUE, all.y=FALSE)

####Dementia obs####

load(paste0(ProjectDir, "/Extracts/DementiaAll.Rdata"))

#create first diagnosis variables
Dementia<-PatDementiaAll%>%
  group_by(patid)%>%
  summarise(FirstDementia=min(eventdate, na.rm=TRUE))

#Append first diagnosis dates to trial table
CPRD<-merge(x=CPRD, y=Dementia, by="patid", all.x=TRUE, all.y=FALSE)

length(unique(CPRD$patid))

save(CPRD, file = paste0(ProjectDir, "/Data/CPRD_DiabDem.rdata"))

         




