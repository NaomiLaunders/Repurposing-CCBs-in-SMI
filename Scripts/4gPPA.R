####Per protocol analysis####

#Call start script
source("./Scripts/4dAnalysisPackages.R")

####Load trials####
load(paste0(ProjectDir, "/Data/Trial_toimpute.rdata"))
load(paste0(ProjectDir, "/Extracts/AllCCBs.Rdata"))

CCBDate<-select(Trial, patid, FirstCCBARB, Arm, end)

#Only include statins within the two year follow up...
CCBDate$Yr2<-as.Date(CCBDate$FirstCCBARB+730.5)
CCBDate$EndDate<-pmin(CCBDate$end, CCBDate$Yr2)

TrialCCB<-subset(AllCCB, patid %in% Trial$patid)
TrialCCB<-merge(x=TrialCCB, y=CCBDate, by="patid", all.x=TRUE, all.y=FALSE)

#Per protocol analysis so need to account for switching and discontinuation

#Switched statin:Treatment: First non-simvastatin prescription
TrialCCB<-subset(TrialCCB, !is.na(FirstCCBARB))
TrialCCB<-subset(TrialCCB, issuedate<=EndDate & issuedate-FirstCCBARB>=0)

table(TrialCCB$Group)
table(TrialCCB$Arm)
table(TrialCCB$Class)
table(TrialCCB$Name)

#Remove exclusions 
TrialCCB<-subset(TrialCCB, Group!="6 month exclusion")

#First switch: Active arm (remove any that switch to a different CCB, ACE inhibitor, or ARB or combo of the three)
T1AS<-subset(TrialCCB, Name!="Amlodipine" & Arm=="NBBP_CCB")
T1AS<-T1AS%>%
  group_by(patid)%>%
  mutate(FirstSwitch=min(issuedate))%>%
  subset(FirstSwitch==issuedate)%>%
  select(patid, FirstSwitch)%>%
  distinct()
  
#Switched statin:Comparator: Switched to amlodipine or started another group
T1CS<-subset(TrialCCB, (Name=="Amlodipine"|!(Class=="CCB" & Group=="Mono")) & Arm=="BBP_CCB")

T1CS<-T1CS%>%
  group_by(patid)%>%
  mutate(FirstSwitch=min(issuedate))%>%
  subset(FirstSwitch==issuedate)%>%
  select(patid, FirstSwitch)%>%
  distinct()

CCBSwitch<-rbind(T1AS, T1CS)

length(unique(CCBSwitch$patid))

#Discontinued CCB: Gap of more than 90 days
CCBDis<-TrialCCB%>%
  group_by(patid)%>%
  arrange(patid, issuedate)%>%
  mutate(TimeToNext=as.numeric(lead(issuedate)-issuedate))%>%
  mutate(TimeToNext=case_when(is.na(TimeToNext) ~ as.numeric(EndDate-issuedate),
                              TRUE ~ TimeToNext))%>%
  subset(TimeToNext>90)%>%
  mutate(Min=min(issuedate))%>%
  subset(issuedate==Min)%>%
  mutate(Discont=issuedate+90)%>%
  select(patid, Discont)%>%
  distinct()

length(unique(CCBDis$patid))

PPA<-merge(x=CCBDis, y=CCBSwitch, by="patid", all.x=TRUE, all.y=TRUE)
PPA$PPADate<-pmin(PPA$Discont, PPA$FirstSwitch, na.rm=TRUE)
PPA<-select(PPA, patid, PPADate)

#####Now bind back on to trial####
Trial<-merge(x=Trial, y=PPA, by="patid", all.x=TRUE, all.y=TRUE)
Trial$Yr2<-as.Date(Trial$FirstCCBARB+730.5)

Trial$PPAEnd<-pmin(Trial$end, Trial$Yr2, Trial$PPADate, na.rm=TRUE)
Trial$PPAFU<-as.numeric(Trial$PPAEnd-Trial$FirstCCBARB)
summary(Trial$PPAFU)

#Set binary measures to only happen if they are before the new end date
for (i in (1:length(Outcome))) {
  Trial[[Outcome[i]]]<-case_when(Trial[[Time[i]]]>Trial$PPAFU ~ 0, TRUE ~ Trial[[Outcome[i]]])
  Trial[[Time[i]]]=case_when(Trial[[Time[i]]]>Trial$PPAFU ~ Trial$PPAFU, TRUE ~ Trial[[Time[i]]])
}

length(which(Trial$MHBin24==1 & Trial$TimeToMH24>Trial$PPAFU))
summary(Trial$TimeToMH24)
table(Trial$MHBin24)

NewFields<-select(Trial, patid, all_of(Outcome), all_of(Time), PPAFU)

#Load imputed data set and bind them on
load(paste0(ProjectDir, "/Data/ImputedData.rdata"))
summary(datlist[[1]]$TimeToMH24)
table(datlist[[1]]$MHBin24)

datlist<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    select(-all_of(Outcome), -all_of(Time))
Data<-merge(x=Data, y=NewFields, by="patid", all.x=TRUE, all.y=TRUE)  
  })

#Run the analysis
#Do it as a loop

UnadjResult<-data.frame()

for (i in (1:length(Outcome))) {
  Unadj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Arm+strata(pracid)")), data=data)
  })
  Unadj<-summary(mice::pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
  Unadj$Outcome<-Outcome[i]
  UnadjResult<-rbind(Unadj, UnadjResult)
}

TimePoint<-c(replicate(3,"3"),replicate(3,"6"),replicate(3,"24"),replicate(3,"12"))
Group<-c("All","MH", "SH")
Group<-c(replicate(4, Group))
UnadjResult<-cbind(UnadjResult, TimePoint, Group) 
UnadjResult$Result<-paste0(format(round(UnadjResult$estimate, 2), nsmall=2, scientific = FALSE), " (", format(round(UnadjResult$`2.5 %`, 2), nsmall=2, scientific = FALSE), "-", format(round(UnadjResult$`97.5 %`, 2), nsmall=2, scientific = FALSE), ")")

#Adjusted

#Do the first one to create data
AdjResult<-data.frame()

#Then bind them on

for (i in (1:length(Outcome))) {
  Adj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ (Arm+SMIDiag+", (paste0(Adjust, "+strata(pracid))")))), data=data)
  })
  Adj<-summary(mice::pool(Adj), conf.int=TRUE, exponentiate=TRUE)
  Adj$Outcome<-Outcome[i]
  AdjResult<-rbind(AdjResult, Adj)
}

TimePoint<-c(replicate(129,"12"),replicate(129,"24"),replicate(129,"6"),replicate(129,"3"))
Group<-c(replicate(43, "MH"), replicate(43,"SH"), replicate(43,"All"))
Group<-c(replicate(4, Group))
AdjResult<-cbind(AdjResult, TimePoint, Group) 
AdjResult$Result<-paste0(format(round(AdjResult$estimate, 2), nsmall=2, scientific = FALSE), " (", format(round(AdjResult$`2.5 %`, 2), nsmall=2, scientific = FALSE), "-", format(round(AdjResult$`97.5 %`, 2), nsmall=2, scientific = FALSE), ")")
AdjResult$Result<-str_squish(AdjResult$Result)

write.csv(UnadjResult, "Outputs/TrialUnadjPPA.csv")
write.csv(AdjResult, "Outputs/TrialAdjPPA.csv")

save(UnadjResult, file=paste0(ProjectDir, "/Data/PPAUnadj.rdata"))
save(AdjResult, file=paste0(ProjectDir, "/Data/PPAAdj.rdata")) 

save(datlist, file = paste0(ProjectDir, "/Data/ImputedDataPPA.rdata"))

  
  
  
  
  


