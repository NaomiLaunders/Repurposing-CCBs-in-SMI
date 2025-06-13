####CCB and ARB observations####

#Call start script
source("./Scripts/1aSetUp.R")

####Load product dictionaries####
AurumCCB<-ReadAurumProdCodelist("Codelists/CCBAurum.txt")
GoldCCB<-ReadGoldProdCodelist("Codelists/CCBGold.txt")

####GOLD product####

#Generate file numbers

Observation_files <- list.files(path=paste0(DataDir, "/GOLD/Therapy"), pattern = ("\\.txt$"))
Observation_files<-paste0(DataDir, "/GOLD/Therapy/", Observation_files)

#~~~Select only observations that are CCB based
PatObsGold<-data.frame()

for (i in 1:(length(Observation_files))) {
  FileName<-Observation_files[i]
  PatObs<-ReadGoldProdObs(FileName)
  PatObs<-select(PatObs, -bnfcode)
  PatObs<-merge(x=GoldCCB, y=PatObs, by ="prodcode", all.y = FALSE, all.x=FALSE)
  PatObs<-select(PatObs, patid, prodcode,eventdate, sysdate, dosageid, bnfcode, qty, numdays, numpacks, packtype, issueseq, productname, drugsubstance, strength, bnfchapter, route, Group, Name, Class)
  PatObsGold<-rbind(PatObsGold, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

save(PatObsGold, file=paste0(ProjectDir, "/Extracts/CCBGold.Rdata"))

####Aurum product#####
Observation_files <- list.files(path=paste0(DataDir, "/Aurum/DrugIssue"), pattern = ("\\.txt$"))
Observation_files<-paste0(DataDir, "/Aurum/DrugIssue/", Observation_files)

PatObsAurum<-data.frame()

#~~~Select only observations that are CCB based
for (i in (1:(length(Observation_files)))) {
  FileName<-Observation_files[i]
  PatObs<-ReadAurumProdObs(FileName)
  PatObs<-merge(x=AurumCCB, y=PatObs, by.x="ProdCodeId", by.y="prodcodeid", all.y = FALSE, all.x=FALSE)
  PatObsAurum<-rbind(PatObsAurum, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

save(PatObsAurum, file=paste0(ProjectDir, "/Extracts/CCBAurum.Rdata"))

####Sort out look ups####
QuantG<-ReadGeneral(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGold/packtype.txt"))
QuantA<-ReadGeneral(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/QuantUnit.txt"))
DoseG<-ReadGeneral(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGold/common_dosages.txt"))
DoseA<-ReadGeneral(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/common_dosages.txt"))

GoldCCB<-merge(x=PatObsGold, y=QuantG, by="packtype", all.x=TRUE, all.y=FALSE)
rm(PatObsGold)
GoldCCB<-merge(x=GoldCCB, y=DoseG, by="dosageid", all.x=TRUE, all.y=FALSE)

save(GoldCCB, file=paste0(ProjectDir, "/Extracts/CCBGold.Rdata"))

rm(QuantG, DoseG, GoldCCB)

AurumCCB<-merge(x=PatObsAurum, y=QuantA, by="quantunitid", all.x=TRUE, all.y=FALSE)
rm(PatObsAurum)
AurumCCB<-merge(x=AurumCCB, y=DoseA, by="dosageid", all.x=TRUE, all.y=FALSE)

rm(QuantA, DoseA)

####Look ups for Aurum####
AurumCCB<-AurumCCB%>%
  select(patid, issuedate, enterdate, quantity, packtype=Description, duration,  productname=ProductName, 
         ingredient=DrugSubstanceName, strength=SubstanceStrength, bnfchapter=BNFChapter, Route=RouteOfAdministration, Formulation, dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
         dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration, prodcode=ProdCodeId, Group, Name, Class, Term.from.EMIS)

AurumCCB$issuedate<-as.Date(AurumCCB$issuedate,  format= "%d/%m/%Y")
AurumCCB$enterdate<-as.Date(AurumCCB$enterdate,  format= "%d/%m/%Y")
summary(AurumCCB$issuedate)
AurumCCB$issuedate[year(AurumCCB$issuedate)<1901|year(AurumCCB$issuedate)>2023]<-AurumCCB$enterdate[year(AurumCCB$issuedate)<1901|year(AurumCCB$issuedate)>2023]

AurumCCB$issuedate[AurumCCB$issuedate<="1900/01/01"]<-NA
AurumCCB$enterdate[AurumCCB$sysdate<="1900/01/01"]<-NA

AurumCCB$issuedate[is.na(AurumCCB$issuedate)]<-AurumCCB$enterdate[is.na(AurumCCB$issuedate)]

save(AurumCCB, file=paste0(ProjectDir, "/Extracts/CCBAurum.Rdata"))

####Look ups for Gold###

load(paste0(ProjectDir, "/Extracts/CCBGold.Rdata"))

GoldCCB<-GoldCCB%>%
  select(patid, issuedate=eventdate, enterdate=sysdate, quantity=qty, packtype,  duration=numdays, productname,
         ingredient=drugsubstance, strength, bnfchapter, Route=route, numpacks, issueseq,     bnfchapter, packtype_desc, dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
         dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration, prodcode, Group, Name, Class)

####Work out what is missing####
GoldCCB$issuedate<-as.Date(GoldCCB$issuedate,  format= "%d/%m/%Y")
GoldCCB$enterdate<-as.Date(GoldCCB$enterdate,  format= "%d/%m/%Y")
summary(GoldCCB$issuedate)

#If issue date <1901 or >2019 take enterdate
GoldCCB$issuedate[year(GoldCCB$issuedate)<1901|year(GoldCCB$issuedate)>2023]<-GoldCCB$enterdate[year(GoldCCB$issuedate)<1901|year(GoldCCB$issuedate)>2023]

GoldCCB$issuedate[GoldCCB$issuedate<="1900/01/01"]<-NA
GoldCCB$enterdate[GoldCCB$sysdate<="1900/01/01"]<-NA

GoldCCB$issuedate[is.na(GoldCCB$issuedate)]<-GoldCCB$enterdate[is.na(GoldCCB$issuedate)]

save(GoldCCB, file=paste0(ProjectDir, "/Extracts/CCBGold.Rdata"))

AurumCCB<-select(AurumCCB, -Formulation)
AurumCCB$patid<-paste0(AurumCCB$patid, "-A")
GoldCCB<-select(GoldCCB, -numpacks, -issueseq, -packtype_desc)
GoldCCB$patid<-paste0(GoldCCB$patid, "-G")
GoldCCB$Term.from.EMIS<-NA

AllCCB<-rbind(AurumCCB, GoldCCB)

save(AllCCB, file=paste0(ProjectDir, "/Extracts/AllCCBs.Rdata"))

rm(AllCCB, AurumCCB, GoldCCB)

####Look for med codes####

GoldCCBCode<-ReadGoldMedCodelist("Codelists/CCBGoldmed.txt")
AurumCCBCode<-ReadAurumMedCodelist("Codelists/CCBAurummed.txt")

####Gold med####
Observation_files <- list.files(path=paste0(DataDir, "/GOLD/Clinical"), pattern = ("\\.txt$"))
Observation_files<-paste0(DataDir, "/GOLD/Clinical/", Observation_files)

Observation_files1 <- list.files(path=paste0(DataDir, "/GOLD/Referral"), pattern = ("\\.txt$"))
Observation_files1<-paste0(DataDir, "/GOLD/Referral/", Observation_files1)

Observation_files2 <- list.files(path=paste0(DataDir, "/GOLD/Test"), pattern = ("\\.txt$"))
Observation_files2<-paste0(DataDir, "/GOLD/Test/", Observation_files2)

Observation_files<-c(Observation_files, Observation_files1, Observation_files2)

#Select those that are CCBs 
PatObsGold<-data.frame()

for (i in 1:(length(Observation_files))) {
  FileName<-Observation_files[i]
  PatObs<-ReadGoldMedObs(FileName)
  PatObs<-merge(x=GoldCCBCode, y=PatObs, by ="medcode", all.y = FALSE, all.x=FALSE)
  PatObs<-select(PatObs, patid, medcode, desc, eventdate, sysdate, Type)
  PatObsGold<-rbind(PatObsGold, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

PatObsGold$eventdate<-as.Date(PatObsGold$eventdate, format="%d/%m/%Y")
PatObsGold$sysdate<-as.Date(PatObsGold$sysdate, format="%d/%m/%Y")

length(which(is.na(PatObsGold$eventdate)))
length(which(is.na(PatObsGold$sysdate)))

length(which(PatObsGold$eventdate<="1900/01/01"))
length(which(PatObsGold$sysdate<="1900/01/01"))

PatObsGold$eventdate[PatObsGold$eventdate<="1900/01/01"]<-NA
PatObsGold$sysdate[PatObsGold$sysdate<="1900/01/01"]<-NA

PatObsGold$eventdate[is.na(PatObsGold$eventdate)]<-PatObsGold$sysdate[is.na(PatObsGold$eventdate)]

save(PatObsGold, file=paste0(ProjectDir, "/Extracts/CCBMedGold.Rdata"))

####Aurum medical####

Observation_files <- list.files(path=paste0(DataDir, "/Aurum/Observation"), pattern = ("\\.txt$"))
Observation_files<-paste0(DataDir, "/Aurum/Observation/", Observation_files)

#Select those that are CCBS 
PatObsAurum<-data.frame()

AurumCCBCode<-rename(AurumCCBCode, medcodeid = MedCodeId)

for (i in 1:(length(Observation_files))) {
  FileName<-Observation_files[i]
  PatObs<-ReadAurumMedObs(FileName)
  PatObs<-merge(x=AurumCCBCode, y=PatObs, by ="medcodeid", all.y = FALSE, all.x=FALSE)
  PatObs<-select(PatObs, patid, medcodeid, Term, eventdate=obsdate, sysdate=enterdate, Type)
  PatObsAurum<-rbind(PatObsAurum, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

PatObsAurum<-rename(PatObsAurum, medcode=medcodeid)

PatObsAurum$eventdate<-as.Date(PatObsAurum$eventdate, format="%d/%m/%Y")
PatObsAurum$sysdate<-as.Date(PatObsAurum$sysdate, format="%d/%m/%Y")

length(which(is.na(PatObsAurum$eventdate)))
length(which(is.na(PatObsAurum$sysdate)))

length(which(PatObsAurum$eventdate<="1900/01/01"))
length(which(PatObsAurum$sysdate<="1900/01/01"))

PatObsAurum$eventdate[PatObsAurum$eventdate<="1900/01/01"]<-NA
PatObsAurum$sysdate[PatObsAurum$sysdate<="1900/01/01"]<-NA

PatObsAurum$eventdate[is.na(PatObsAurum$eventdate)]<-PatObsAurum$sysdate[is.na(PatObsAurum$eventdate)]

save(PatObsAurum, file=paste0(ProjectDir, "/Extracts/CCBMedAurum.Rdata"))


####Merge to one file####

PatObsAurum$patid<-paste0(PatObsAurum$patid, "-A")
PatObsGold$patid<-paste0(PatObsGold$patid, "-G")

names(PatObsAurum)
names(PatObsGold)
PatObsGold<-rename(PatObsGold, Term=desc)

PatCCBMed<-rbind(PatObsAurum, PatObsGold)

save(PatCCBMed, file=paste0(ProjectDir, "/Extracts/CCBMedAll.Rdata"))
