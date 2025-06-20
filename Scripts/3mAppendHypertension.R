####Append hypertension####

#Call start script
source("./Scripts/1aSetUp.R")

####Load 2023 data####
load(paste0(ProjectDir, "/Data/CPRD_Phys.rdata"))

load(paste0(ProjectDir, "/Extracts/BPValues.Rdata"))
load(paste0(ProjectDir, "/Extracts/BPCat.Rdata"))

load(paste0(ProjectDir, "/Extracts/HistoricBP.Rdata"))
load(paste0(ProjectDir, "/Extracts/PregBP.Rdata"))
load(paste0(ProjectDir, "/Extracts/OtherBP.Rdata"))

#Find high BP by value
HighVal<-subset(FinalBP, (AmbClin == "Clinical" & Diastolic>=90 & Systolic>=140) |
                  (AmbClin == "Ambulatory" & Diastolic>=80 & Systolic>=135))

HighCat<-subset(AllBPCat, Category == "high bp")

Hypertension<-subset(AllBPCat, Category == "hypertension")

#Did they occur before date of birth
YoB<-select(Trial, patid, yob)
HighVal<-merge(x=HighVal, y=YoB, by="patid", all.x=TRUE, all.y=FALSE)
HighVal<-subset(HighVal, yob<year(eventdate))

HighCat<-merge(x=HighCat, y=YoB, by="patid", all.x=TRUE, all.y=FALSE)
HighCat<-subset(HighCat, yob<year(eventdate))

Hypertension<-merge(x=Hypertension, y=YoB, by="patid", all.x=TRUE, all.y=FALSE)
Hypertension<-subset(Hypertension, yob<year(eventdate))

HighCat<-select(HighCat, patid, eventdate)
HighCat$Cat<-"HighCat"
HighVal<-select(HighVal, patid, eventdate)
HighVal$Cat<-"HighVal"
Hypertension<-select(Hypertension, patid, eventdate)
Hypertension$Cat<-"Hypertension"

AnyHigh<-rbind(HighCat, HighVal, Hypertension)
HighCatVal<-rbind(HighCat, HighVal)

#First any
AnyHigh<-AnyHigh%>%
  group_by(patid)%>%
  summarise(FirstAnyHighBPDiag = min(eventdate))%>%
  distinct()

#First high value
HighCatVal<-HighCatVal%>%
  group_by(patid)%>%
  summarise(FirstHighBPCatValue = min(eventdate, na.rm=TRUE))%>%
  distinct()

#First hypertension
Hypertension<-Hypertension%>%
  group_by(patid)%>%
  summarise(FirstHypDiag = min(eventdate, na.rm=TRUE))%>%
  distinct()

#Append first diagnosis dates to trial table
Trial<-merge(x=Trial, y=AnyHigh, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=HighCatVal, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=Hypertension, by="patid", all.x=TRUE, all.y=FALSE)

#Create variables for first diagnosis before CCB
Trial<-Trial%>%
  mutate(PriorHyp = case_when(FirstHypDiag<=FirstCCBARB ~ 1,
                              TRUE ~ 0),
         PriorHighBP = case_when(FirstHighBPCatValue<=FirstCCBARB ~ 1,
                              TRUE ~ 0),
         TimeSinceHyp = case_when(FirstCCBARB-FirstHypDiag>=0 ~ as.numeric(FirstCCBARB-FirstHypDiag),
                                  TRUE ~ 0))

table(Trial$PriorHyp)
table(Trial$PriorHighBP)
summary(Trial$TimeSinceHyp)

#Most recent BP
CCB<-select(Trial, patid, FirstCCBARB)
FinalBP<-merge(x=FinalBP, y=CCB, by="patid", all.x=TRUE, all.y=FALSE)
FinalBP<-subset(FinalBP, FirstCCBARB-eventdate>=0 & FirstCCBARB-eventdate<=(365.25*3))
AmbBP<-FinalBP%>%
  group_by(patid)%>%
  subset(AmbClin=="Ambulatory")%>%
  mutate(maxBP=max(eventdate, na.rm=TRUE))%>%
  subset(eventdate==maxBP)%>%
  mutate(n=n())

length(unique(AmbBP$patid))

ClinBP<-FinalBP%>%
  group_by(patid)%>%
  subset(AmbClin=="Clinical")%>%
  mutate(maxBP=max(eventdate, na.rm=TRUE))%>%
  subset(eventdate==maxBP)%>%
  mutate(n=n())

length(unique(ClinBP$patid))

AmbBP<-subset(AmbBP, ! patid %in% ClinBP$patid)

BP<-rbind(ClinBP, AmbBP)
BP<-select(BP, patid, Systolic, Diastolic)
Trial<-merge(x=Trial, y=BP, by="patid", all.x=TRUE, all.y=FALSE)

save(Trial, file = paste0(ProjectDir, "/Data/CPRD_Hyp.rdata"))

         




