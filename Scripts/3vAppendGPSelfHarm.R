####Append GP self harm####

#Call start script
source("./Scripts/1aSetUp.R")

load(paste0(ProjectDir, "/Data/Trial_Consults.rdata"))

load(paste0(ProjectDir, "/StatinExtracts/SelfHarm.Rdata"))
PatSelfHarmAll<-subset(PatSelfHarmAll, patid %in% Trial$patid)

length(which(Trial$patid %in% PatSelfHarmAll$patid))

####If it has suicide; did they have a date of death within 30 days?####
length(unique(PatSelfHarmAll$patid))
table(PatSelfHarmAll$Group)

Suicide<-subset(PatSelfHarmAll, grepl("suicide", PatSelfHarmAll$term, ignore.case=TRUE) & grepl("and|or|\\+", PatSelfHarmAll$term, ignore.case=TRUE))
table(Suicide$term)

Death<-select(Trial, patid, deathdate)
Suicide<-merge(x=Suicide, y=Death, by="patid", all.x=TRUE, all.y=FALSE)

Suicide$TimeToDeath<-as.numeric(Suicide$deathdate - Suicide$eventdate)
summary(Suicide$TimeToDeath)

#There are none within 30 days suggesting no suicides

####Self harm in the 6 months prior####
Date<-select(Trial, patid, FirstCCBARB, end)
PatSelfHarmAll<-merge(x=PatSelfHarmAll, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

#Only include if before end
PatSelfHarmAll<-subset(PatSelfHarmAll, eventdate<=end)

PriorSHGPStat<-PatSelfHarmAll%>%
  subset(eventdate<=FirstCCBARB & FirstCCBARB-eventdate<=182.625)%>%
  group_by(patid)%>%
  summarise(PriorSHGPStatAll=n(),PriorSHGPStatIntent=sum(Group=="unknown intent"), PriorSHGPStatSH=sum(Group=="self harm"))

length(unique(PriorSHGPStat$patid))

table(PriorSHGPStat$PriorSHGPStatIntent, PriorSHGPStat$PriorSHGPStatSH)
table(PriorSHGPStat$PriorSHGPStatAll)

Trial$PriorSHGP<-0
Trial$PriorSHGP[Trial$patid %in% PriorSHGPStat$patid]<-1

####Outcomes: statin####
OutcomeGPSH3<-PatSelfHarmAll%>%
  subset(eventdate>FirstCCBARB & eventdate-FirstCCBARB<=84)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGP3=n())

OutcomeGPSH6<-PatSelfHarmAll%>%
  subset(eventdate>FirstCCBARB & eventdate-FirstCCBARB<=182.625)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGP6=n())

OutcomeGPSH12<-PatSelfHarmAll%>%
  subset(eventdate>FirstCCBARB & eventdate-FirstCCBARB<=365.25)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGP12=n())

OutcomeGPSH24<-PatSelfHarmAll%>%
  subset(eventdate>FirstCCBARB & eventdate-FirstCCBARB<=730.5)%>%
  group_by(patid)%>%
  summarise(OutcomeSHGP24=n(), FirstSHGP24=min(eventdate))


Trial<-merge(x=Trial, y=OutcomeGPSH3, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=OutcomeGPSH6, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=OutcomeGPSH12, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=OutcomeGPSH24, by="patid", all.x=TRUE, all.y=FALSE)

save(Trial, file = paste0(ProjectDir, "/Data/SH.rdata"))
  

