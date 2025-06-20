####Number of consultations in the 6 months prior to diagnosis####

#Call start script
source("./Scripts/1aSetUp.R")

load(paste0(ProjectDir, "/Data/Trial_IMD.rdata"))

#~~~Create patient lookup
Patients<-select(Trial, patid, source, FirstCCBARB)
PatientsAurum<-subset(Trial, source=="Aurum")
PatientsGold<-subset(Trial, source=="Gold")

####AURUM####

#~~~Choose only observations for study patients
Observation_files <- list.files(path=paste0(DataDir, "/Aurum/Consultation"), pattern = ("\\.txt$"))
Observation_files<-paste0(DataDir, "/Aurum/Consultation/", Observation_files)

#Select consultations

PatObsAurum<-data.frame()

for (i in 1:(length(Observation_files))) {
  FileName<-Observation_files[i]
  PatObs<-read.table(FileName, header = TRUE, sep = "\t", dec = ".", quote="", fill=TRUE, colClasses=c(consmedcodeid="character", patid="character"))
  PatObs$patid<-paste0(PatObs$patid, "-A")
  PatObs<-subset(PatObs, patid %in% PatientsAurum$patid)
  PatObsAurum<-rbind(PatObsAurum, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

####GOLD####

#~~~Choose only observations for study patients
#Generate file numbers
Observation_files <- list.files(path=paste0(DataDir, "/GOLD/Consultation"), pattern = ("\\.txt$"))
Observation_files<-paste0(DataDir, "/GOLD/Consultation/", Observation_files)

#Merge to patient file
PatObsGold<-data.frame()

for (i in 1:(length(Observation_files))) {
  FileName<-Observation_files[i]
  PatObs<-ReadGoldMedObs(FileName)
  PatObs$patid<-paste0(PatObs$patid, "-G")
  PatObs<-subset(PatObs, patid %in% PatientsGold$patid)
  PatObsGold<-rbind(PatObsGold, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

####Only keep if face to face####
AurumMed<-ReadAurumMedCodelist(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt"))
GoldMed<-ReadGoldMedCodelist(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGOLD/medical.txt"))

Aurum<-PatObsAurum%>%
  group_by(consmedcodeid)%>%
  summarise(n=n())

AurumMedCode<-merge(x=Aurum, y=AurumMed, by.x="consmedcodeid", by.y="MedCodeId", all.x=TRUE, all.y=FALSE)
AurumMedCode<-select(AurumMedCode, consmedcodeid, n, Term)

AurumMedCode<-AurumMedCode%>%
  mutate(Group = case_when(grepl("request|document|result|online|note|other|mail|text|Admin|video|non|report|tele|await|hospital|discuss|multimedia|outbound|without|repeat|case", Term, ignore.case=TRUE) ~ "Other",
                           is.na(Term) ~ "NA",
                           grepl("Face|consult|group|walk|GP|team|inbound|seen", Term, ignore.case=TRUE) ~ "F2F",
                           TRUE ~ "F2F"))

AurumLookup<-ReadGeneral(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/ConsSource.txt"))

AurumCons<-PatObsAurum%>%
  group_by(conssourceid)%>%
  summarise(n=n())

AurumConsCode<-merge(x=AurumCons, y=AurumLookup, by.x="conssourceid", by.y="Id", all.x=TRUE, all.y=FALSE)

AurumConsCode<-AurumConsCode%>%
  mutate(Group = case_when(Description=="External Encounter" ~ "F2F",
    grepl("Not Present|Insert|Radiology|Office|outpat|Post|record|scanner|school|screen|Summary|referral|Phone|discharge|dna|docman|failed|info|path|inpatient|lab|jog|contact|letter|medi|memo|else|external|message|comment|communicat|conv|corr|data|bulk|did not|unbooked|no attendance|casualty|added|accident|request|document|result|online|note|other|mail|text|Admin|video|non|report|tele|await|hospital|discuss|multimedia|outbound|without|repeat|case", Description, ignore.case=TRUE) ~ "Other",
                           is.na(Description) ~ "NA",
                           Description=="D.N.A."|Description=="Not Actually Seen"|Description=="Not Known" ~ "Other",
    grepl("Out of Hours|Nursing|visit|surgery|emergency|appointment|clinic|Face|consult|group|walk|GP|team|inbound|seen|acute|attendance", Description, ignore.case=TRUE) ~ "F2F",
                           TRUE ~ "F2F"))

PatObsAurum<-merge(x=PatObsAurum, y=AurumConsCode, by="conssourceid", all.x=TRUE, all.y=FALSE)
PatObsAurum<-merge(x=PatObsAurum, y=AurumMedCode, by="consmedcodeid", all.x=TRUE, all.y=FALSE)

length(which((PatObsAurum$Group.x=="NA" |is.na(PatObsAurum$Group.x)) & (PatObsAurum$Group.y=="NA" | is.na(PatObsAurum$Group.y))))

PatObsAurum<-PatObsAurum%>%
  mutate(Group=case_when(Group.x=="F2F" | Group.y=="F2F" ~ "F2F",
                         (Group.x=="NA" |is.na(PatObsAurum$Group.x)) & (PatObsAurum$Group.y=="NA" | is.na(PatObsAurum$Group.y)) ~ "F2F",
                         TRUE ~ "Other"))
F2FAurum<-subset(PatObsAurum, Group=="F2F")
F2FAurum$LookUp<-paste0(F2FAurum$patid,"-", F2FAurum$consid, "_", F2FAurum$eventdate)
length(unique(F2FAurum$LookUp))

####Gold F2F###
Gold<-PatObsGold%>%
  group_by(constype)%>%
  summarise(n=n())

GoldLookup<-ReadGeneral(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGold/TXTFILES/COT.txt"))

GoldConsCode<-merge(x=Gold, y=GoldLookup, by.x="constype", by.y="Code", all.x=TRUE, all.y=FALSE)

GoldConsCode<-GoldConsCode%>%
  mutate(Group = case_when(Consultation.Type=="External Encounter"|Consultation.Type=="Medicine Management"|Consultation.Type=="Out of hours, Non Practice" ~ "F2F",
                           grepl("Not Present|Template|Insert|Radiology|Office|outpat|Post|record|scanner|school|screen|Summary|referral|Phone|discharge|dna|docman|failed|info|path|inpatient|lab|jog|contact|letter|medi|memo|else|external|message|comment|communicat|conv|corr|data|bulk|did not|unbooked|no attendance|casualty|added|accident|request|document|result|online|note|other|mail|text|Admin|video|non|report|tele|await|hospital|discuss|multimedia|outbound|without|repeat|case", Consultation.Type, ignore.case=TRUE) ~ "Other",
                           is.na(Consultation.Type) ~ "NA",
                           Consultation.Type=="D.N.A."|Consultation.Type=="Not Actually Seen"|Consultation.Type=="Not Known" ~ "Other",
                           grepl("Out of Hours|Nursing|visit|surgery|emergency|appointment|clinic|Face|consult|group|walk|GP|team|inbound|seen|acute|attendance", Consultation.Type, ignore.case=TRUE) ~ "F2F",
                           TRUE ~ "F2F"))

PatObsGold<-merge(x=PatObsGold, y=GoldConsCode, by="constype", all.x=TRUE, all.y=FALSE)

F2FGold<-subset(PatObsGold, Group=="F2F")
F2FGold$LookUp<-paste0(F2FGold$patid,"-", F2FGold$consid, "_", F2FGold$eventdate)
length(unique(F2FGold$LookUp))

####Merge Aurum and Gold####
F2FGold<-select(F2FGold, patid, consdate=eventdate, enterdate=sysdate)
F2FAurum<-select(F2FAurum, patid, consdate, enterdate)
AllF2FCons<-rbind(F2FGold, F2FAurum)
AllF2FCons$consdate<-as.Date(AllF2FCons$consdate, format= "%d/%m/%Y")
AllF2FCons$enterdate<-as.Date(AllF2FCons$enterdate, format= "%d/%m/%Y")

####Combine####
length(which(is.na(AllF2FCons$consdate)))
length(which(is.na(AllF2FCons$enterdate)))

length(which(AllF2FCons$consdate<="1900/01/01"))
length(which(AllF2FCons$enterdate<="1900/01/01"))

AllF2FCons$consdate[AllF2FCons$consdate<="1900/01/01"]<-NA
AllF2FCons$enterdate[AllF2FCons$enterdate<="1900/01/01"]<-NA

AllF2FCons$consdate[is.na(AllF2FCons$consdate)]<-AllF2FCons$enterdate[is.na(AllF2FCons$consdate)]

####Primary care consultations in the year prior to index####

length(unique(AllF2FCons$patid))

#Dont include the date of index because they had to attend!
Dates<-select(Trial, patid, FirstCCBARB)
AllCons<-merge(x=AllF2FCons, y=Dates, by="patid", all.x=FALSE, all.y=FALSE)

PriorGP<-AllCons%>%
  subset(FirstCCBARB-consdate>0&FirstCCBARB-consdate<=182.625)%>%
  group_by(patid)%>%
  summarise(PriorGP=n())

Trial<-merge(x=Trial, y=PriorGP, by="patid", all.x=TRUE, all.y=FALSE)
Trial$PriorGP[is.na(Trial$PriorGP)]<-0

save(Trial, file=paste0(ProjectDir, "/Data/Trial_Consults.rdata"))





