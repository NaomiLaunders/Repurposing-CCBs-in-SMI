####Individual study inclusion####

#Call start script
source("./Scripts/1aSetUp.R")

load(paste0(ProjectDir, "/Data/HospRay.rdata"))

#####Set arms before selection#####
HospAll<-HospAll%>%
  mutate(Arm = case_when(FirstNBBP_CCBDate==HospAll$FirstCCBARB ~ "NBBP_CCB",
                         FirstBBP_CCBDate==HospAll$FirstCCBARB ~ "BBP_CCB",
                         TRUE ~ "DROP"))

table(HospAll$Arm, useNA="ifany")
Check<-subset(HospAll, is.na(HospAll$Arm))

####Trial one - entry = CCB prescription####

Consort1<-data.frame("Count" = length(HospAll$patid), "Description" = c("No HES linkage"))

Trial1<-subset(HospAll, !is.na(HospAll$FirstCCBARB))
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Combined prescription for CCB/ARB/ACE"))
Consort1<- rbind(Consort1,Consort1a)

#Drop any that have multiple arms
Trial1<-subset(Trial1, FirstCCBARB<FirstComboDate | is.na(FirstComboDate)) 

Excl<-Trial1%>%
  select(patid, FirstCCBARB, FirstNBBP_ACEDate,FirstBBP_ACEDate,FirstNBBP_CCBDate, FirstBBP_CCBDate, FirstVerapamilDate, FirstNBBP_ARBDate, FirstBBP_ARBDate)%>%
  pivot_longer(cols=c(3:9), names_to="Type", values_to = "Date")%>%
  subset(!is.na(Date) & Date==FirstCCBARB)%>%
  group_by(patid)%>%
  mutate(n=n())%>%
  subset(n>1)

Trial1<-subset(Trial1, !(patid %in% Excl$patid))
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Multiple first CCB/ACE/ARB"))
Consort1<- rbind(Consort1,Consort1a)

#First prescribed ARB or ACE
Trial1<-subset(Trial1, Arm!="DROP")
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("ACE/ARB first"))
Consort1<- rbind(Consort1,Consort1a)

####Same day prescription of blocker or diuretic####
Trial1<-subset(Trial1, (FirstAntiHypDate!=FirstDiuretic_BBP_ACEDate | is.na(FirstDiuretic_BBP_ACEDate)) & 
                 (FirstAntiHypDate!=FirstDiuretic_BBP_ARBDate | is.na(FirstDiuretic_BBP_ARBDate))&
                 (FirstAntiHypDate!=FirstBlocker_BBP_CCBDate | is.na(FirstBlocker_BBP_CCBDate))&
                 (FirstAntiHypDate!=FirstDiuretic_NBBP_ACEDate | is.na(FirstDiuretic_NBBP_ACEDate))&
                 (FirstAntiHypDate!=FirstDiuretic_NBBP_ARBDate | is.na(FirstDiuretic_NBBP_ARBDate)))

load(paste0(ProjectDir, "/Extracts/AllBlockers.Rdata"))

Blocker<-AllBlocker%>%
  group_by(patid)%>%
  mutate(FirstBlockerDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstBlockerDate)%>%
  distinct()
Blocker<-subset(Blocker, patid %in% Trial1$patid)
Trial1<-merge(x=Trial1, y=Blocker, by="patid", all.x=TRUE, all.y=FALSE)
Trial1<-subset(Trial1, FirstCCBARB!=FirstBlockerDate | is.na(FirstBlockerDate))

Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Prescribed b-blocker, diuretic or a-blocker on the same day"))
Consort1<- rbind(Consort1,Consort1a)

####Prior prescription of an ACE####
#Trial1<-subset(HospAll, FirstCCBARB - FirstACEDate>91.3125 | FirstCCBARB - FirstACEDate<0| is.na(FirstACEDate))
#Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Prescribed ACE in previous 3 months"))
#Consort1<- rbind(Consort1,Consort1a)

####CCB before 31/12/2018####
Trial1<-subset(Trial1, FirstCCBARB<=as.Date("2018-12-31"))
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("After 2018"))
Consort1<- rbind(Consort1,Consort1a)

####CCB after exit####
Trial1<-subset(Trial1, FirstCCBARB<=end)
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("After end"))
Consort1<- rbind(Consort1,Consort1a)

#First CCB before entry to cohort
Trial1<-subset(Trial1, FirstCCBARB>=enter)
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("First CCB/ARB/ACE before entry to cohort"))
Consort1<- rbind(Consort1,Consort1a)

#Prescribed antihypertensive non-oral CCB in the 3 months before
load(paste0(ProjectDir, "/Extracts/AllCCBs.Rdata"))
Excl<-subset(AllCCB, Group=="6 month exclusion")
table(Excl$Class)

Excl<-Excl%>%
  group_by(patid)%>%
  select(patid, issuedate)%>%
  distinct()

Excl<-merge(x=Excl, y=Trial1, by="patid", all.x=FALSE, all.y=FALSE)
Excl<-subset(Excl,FirstCCBARB-issuedate<91.3125 & FirstCCBARB-issuedate>=0)

Trial1<-subset(Trial1, !(patid %in% Excl$patid))
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Non-oral or non-hypertension in the 3 months before CCB/ARB of interest"))
Consort1<- rbind(Consort1,Consort1a)

#Drop those with evidence of antihypertensive use before first presc.
#Trial1<-subset(Trial1, FirstCCBARB - FirstEvidenceAntiHypDate >91.3125 | FirstCCBARB - FirstEvidenceAntiHypDate<0 | is.na(FirstEvidenceAntiHypDate))
#Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("Evidence of anti-hypertensives in the three months prior to prescription"))
#Consort1<- rbind(Consort1,Consort1a)

Trial1<-subset(Trial1, FirstCCBARB<=FirstEvidenceAntiHypDate | is.na(FirstEvidenceAntiHypDate))
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("Evidence of anti-hypertensives prior to prescription"))
Consort1<- rbind(Consort1,Consort1a)

#SMI prior to entry
Trial1<-subset(Trial1, FirstCCBARB>=diagnosis_date)
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("No diagnosis of SMI before entry"))

#Trial1<-subset(Trial1, FirstCCBARB-diagnosis_date>-30)
#Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Diagnosis of SMI before entry or + 30 days"))
Consort1<- rbind(Consort1,Consort1a)

#Less than 6 months baseline
Trial1<-subset(Trial1, Trial1$FirstCCBARB - Trial1$regstartdate>182.625)
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Less than 6 months baseline"))
Consort1<- rbind(Consort1,Consort1a)

#Pull in APs
load(paste0(ProjectDir, "/StatinExtracts/APProd.Rdata"))
PatAPAll<-subset(PatAPAll, patid %in% Trial1$patid)
CCBARBDate<-select(Trial1, patid, FirstCCBARB)
PatAPAll<-merge(x=PatAPAll, y=CCBARBDate, by="patid", all.x=FALSE, all.y=FALSE)

#Received injectables in the 3 months before CCB
Inj<-subset(PatAPAll, Route=="Injection") # 
Inj<-subset(Inj, FirstCCBARB-eventdate<=91.3125 & FirstCCBARB-eventdate>=0)#Have injection in 3 months before CCB/ARBs

Trial1<-subset(Trial1, !(patid %in% Inj$patid))
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("Injectables in the 3 months before CCB/ARB/ACE use"))
Consort1<- rbind(Consort1,Consort1a)

#No evidence of psychotropic medication prior to CCB use
Trial1<-subset(Trial1, FirstCCBARB>FirstPsychDate)
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("No evidence of psychotropics use prior to CCB/ARB/ACE use"))
Consort1<- rbind(Consort1,Consort1a)

#Didnt receive oral psychotropic in the 6 months prior to CCB therapy
Pat6mo<-subset(PatAPAll, FirstCCBARB-eventdate<=182.625 & FirstCCBARB-eventdate>=0) #Have a prescription in 6 months before CCB
Oral<-subset(Pat6mo, Route=="Oral")

Trial1<-subset(Trial1, (patid %in% Oral$patid))
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("No psychotropics in the 6 months before CCB"))
Consort1<- rbind(Consort1,Consort1a)

#At least one in those six months at therapeutic dose
load(paste0(ProjectDir, "/Extracts/BP_dose_CCB.Rdata"))
load(paste0(ProjectDir,"/Extracts/AP_dose_CCB.Rdata"))

BPDose<-select(BPDoseFinal, patid, eventdate, AP)
APDose<-select(apDoseFinal, patid, eventdate, AP)
AllDose<-rbind(BPDose, APDose)
AllDose<-distinct(AllDose)
AllDose<-merge(x=AllDose, y=CCBARBDate, by="patid", all.x=FALSE, all.y=FALSE)

AllDose6mo<-subset(AllDose, FirstCCBARB-eventdate<=182.625 & FirstCCBARB-eventdate>=0) #Have a prescription in 6 months before CCB

Trial1<-subset(Trial1, patid %in% AllDose6mo$patid)
Consort1a<- data.frame("Count" =length(Trial1$patid), "Description" = c("Not at therapeutic dose"))
Consort1<- rbind(Consort1,Consort1a)


####General exclusions####

#No diabetes - include those on the same day
Trial1<-subset(Trial1, FirstCCBARB<=FirstDiab | is.na(FirstDiab))
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("GP diabetes at baseline"))
Consort1<- rbind(Consort1,Consort1a)

Trial1<-subset(Trial1, HospDiabetes==0)
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Hospital diabetes at baseline"))
Consort1<- rbind(Consort1,Consort1a)

#No dementia - include those on the same day
Trial1<-subset(Trial1, FirstCCBARB<=FirstDementia |is.na(FirstDementia))
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("GP dementia at baseline"))
Consort1<- rbind(Consort1,Consort1a)

Trial1<-subset(Trial1, HospDementia==0)
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("Hospital dementia at baseline"))
Consort1<- rbind(Consort1,Consort1a)

#No Raynauds - exclude those on the same day
Trial1<-subset(Trial1, FirstCCBARB<FirstRaynauds |is.na(FirstRaynauds))
Consort1a<-data.frame("Count" =length(Trial1$patid), "Description" = c("GP Raynauds at baseline"))
Consort1<- rbind(Consort1,Consort1a)

#Sort out consort

load("Outputs/FlowChart.Rdata")
Consort<-select(Consort, -Diff)

FinalConsort<-rbind(Consort, Consort1)

FinalConsort<-FinalConsort %>%
  mutate (Lag = lag(Count), Diff = Lag-Count)%>%
  select(-Lag)

save(FinalConsort, file = "Outputs/FlowChartTrial.Rdata")
write.csv(FinalConsort, file = "Outputs/FlowChartTrial.csv")

####Check####
table(Trial1$Arm)

Arms<-as.data.frame(table(Trial1$Arm))
write.csv(Arms, file = "Outputs/Arms.csv")

#Check which drugs
load(paste0(ProjectDir, "/Extracts/AllCCBs.Rdata"))
AllCCB<-subset(AllCCB, patid %in% Trial1$patid)
Date<-select(Trial1, patid, FirstCCBARB)
AllCCB<-merge(x=AllCCB, y=Date, by="patid", all.x=TRUE, all.y=TRUE)
CCB<-subset(AllCCB, FirstCCBARB == issuedate)

length(unique(CCB$patid))

CCB<-select(CCB, patid, issuedate, Group, Name, Class)
CCB<-distinct(CCB)

CCB<-CCB%>%
  group_by(patid)%>%
  mutate(n=n())

Check<-subset(CCB, n>1)

CCB$Name[CCB$n>1]<-"Lisinopril/Ramipril"
CCB<-distinct(CCB)
CCB<-select(CCB, patid, Drug=Name)

Trial1<-merge(x=Trial1, y=CCB, by="patid", all.x=TRUE, all.y=TRUE)
table(Trial1$Arm, Trial1$Drug)


Trial<-Trial1
save(Trial, file = paste0(ProjectDir, "/Data/StudyPops.rdata"))
