####Beta blocker and diuretics Observations####

#Call start script
source("./Scripts/1aSetUp.R")


####Load product dictionaries####
AurumBlocker<-ReadAurumProdCodelist("Codelists/BlockersDiureticsAurum.txt")
GoldBlocker<-ReadGoldProdCodelist("Codelists/BlockersDiureticsGold.txt")

####GOLD product####

#~~~Select only observations that are Blocker based
Observation_files <- list.files(path=paste0(DataDir, "/GOLD/Therapy"), pattern = ("\\.txt$"))
Observation_files<-paste0(DataDir, "/GOLD/Therapy/", Observation_files)

PatObsGold<-data.frame()

for (i in 1:(length(Observation_files))) {
  FileName<-Observation_files[i]
  PatObs<-ReadGoldProdObs(FileName)
  PatObs<-select(PatObs, -bnfcode)
  PatObs<-merge(x=GoldBlocker, y=PatObs, by ="prodcode", all.y = FALSE, all.x=FALSE)
  PatObs<-select(PatObs, patid, prodcode,eventdate, sysdate, dosageid, bnfcode, qty, numdays, numpacks, packtype, issueseq, productname, drugsubstance, strength, bnfchapter, route, Group)
  PatObsGold<-rbind(PatObsGold, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

save(PatObsGold, file=paste0(ProjectDir, "/Extracts/BlockerGold.Rdata"))

####Aurum product#####
Observation_files <- list.files(path=paste0(DataDir, "/Aurum/DrugIssue"), pattern = ("\\.txt$"))
Observation_files<-paste0(DataDir, "/Aurum/DrugIssue/", Observation_files)

PatObsAurum<-data.frame()

#~~~Select only observations that are beta-blocker based
for (i in (1:(length(Observation_files)))) {
  FileName<-Observation_files[i]
  PatObs<-ReadAurumProdObs(FileName)
  PatObs<-merge(x=AurumBlocker, y=PatObs, by.x="ProdCodeId", by.y="prodcodeid", all.y = FALSE, all.x=FALSE)
  PatObsAurum<-rbind(PatObsAurum, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

save(PatObsAurum, file=paste0(ProjectDir, "/Extracts/BlockerAurum.Rdata"))

####Sort out look ups####
QuantG<-ReadGeneral(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGold/packtype.txt"))
QuantA<-ReadGeneral(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/QuantUnit.txt"))
DoseG<-ReadGeneral(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGold/common_dosages.txt"))
DoseA<-ReadGeneral(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/common_dosages.txt"))

GoldBlocker<-merge(x=PatObsGold, y=QuantG, by="packtype", all.x=TRUE, all.y=FALSE)
rm(PatObsGold)
GoldBlocker<-merge(x=GoldBlocker, y=DoseG, by="dosageid", all.x=TRUE, all.y=FALSE)
save(GoldBlocker, file=paste0(ProjectDir, "/Extracts/BlockerGold.Rdata"))

rm(QuantG, DoseG)

AurumBlocker<-merge(x=PatObsAurum, y=QuantA, by="quantunitid", all.x=TRUE, all.y=FALSE)
rm(PatObsAurum)
AurumBlocker<-merge(x=AurumBlocker, y=DoseA, by="dosageid", all.x=TRUE, all.y=FALSE)

rm(QuantA, DoseA)

####Look ups for Aurum####
AurumBlocker<-AurumBlocker%>%
  select(patid, issuedate, enterdate, quantity, packtype=Description, duration,  productname=ProductName, 
         ingredient=DrugSubstanceName, strength=SubstanceStrength, bnfchapter=BNFChapter, Route=RouteOfAdministration, Formulation, dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
         dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration, prodcode=ProdCodeId, Group, Term.from.EMIS)

AurumBlocker$issuedate<-as.Date(AurumBlocker$issuedate,  format= "%d/%m/%Y")
AurumBlocker$enterdate<-as.Date(AurumBlocker$enterdate,  format= "%d/%m/%Y")
summary(AurumBlocker$issuedate)
AurumBlocker$issuedate[year(AurumBlocker$issuedate)<1901|year(AurumBlocker$issuedate)>2023]<-AurumBlocker$enterdate[year(AurumBlocker$issuedate)<1901|year(AurumBlocker$issuedate)>2023]

AurumBlocker$issuedate[AurumBlocker$issuedate<="1900/01/01"]<-NA
AurumBlocker$enterdate[AurumBlocker$sysdate<="1900/01/01"]<-NA

AurumBlocker$issuedate[is.na(AurumBlocker$issuedate)]<-AurumBlocker$enterdate[is.na(AurumBlocker$issuedate)]

save(AurumBlocker, file=paste0(ProjectDir, "/Extracts/AurumBlocker.Rdata"))

####Look ups for Gold###
GoldBlocker<-GoldBlocker%>%
  select(patid, issuedate=eventdate, enterdate=sysdate, quantity=qty, packtype,  duration=numdays, productname,
         ingredient=drugsubstance, strength, bnfchapter, Route=route, numpacks, issueseq,     bnfchapter, packtype_desc, dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
         dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration, prodcode, Group)

####Work out what is missing####
GoldBlocker$issuedate<-as.Date(GoldBlocker$issuedate,  format= "%d/%m/%Y")
GoldBlocker$enterdate<-as.Date(GoldBlocker$enterdate,  format= "%d/%m/%Y")
summary(GoldBlocker$issuedate)

#If issue date <1901 or >2023 take enterdate
GoldBlocker$issuedate[year(GoldBlocker$issuedate)<1901|year(GoldBlocker$issuedate)>2023]<-GoldBlocker$enterdate[year(GoldBlocker$issuedate)<1901|year(GoldBlocker$issuedate)>2023]

GoldBlocker$issuedate[GoldBlocker$issuedate<="1900/01/01"]<-NA
GoldBlocker$enterdate[GoldBlocker$sysdate<="1900/01/01"]<-NA

GoldBlocker$issuedate[is.na(GoldBlocker$issuedate)]<-GoldBlocker$enterdate[is.na(GoldBlocker$issuedate)]

save(GoldBlocker, file=paste0(ProjectDir, "/Extracts/GoldBlocker.Rdata"))

AurumBlocker<-select(AurumBlocker, -Formulation)
AurumBlocker$patid<-paste0(AurumBlocker$patid, "-A")
GoldBlocker<-select(GoldBlocker, -numpacks, -issueseq, -packtype_desc)
GoldBlocker$patid<-paste0(GoldBlocker$patid, "-G")

GoldBlocker$Term.from.EMIS<-NA

AllBlocker<-rbind(AurumBlocker, GoldBlocker)

save(AllBlocker, file=paste0(ProjectDir, "/Extracts/AllBlockers.Rdata"))

rm(AllBlocker, AurumBlocker, GoldBlocker)


