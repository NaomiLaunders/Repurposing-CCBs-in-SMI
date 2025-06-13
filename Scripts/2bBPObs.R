####BP Observations####

#Call start script
source("./Scripts/1aSetUp.R")

AurumBP<-ReadAurumMedCodelist("Codelists/BPAurum.txt")
GoldBP<-ReadGoldMedCodelist("Codelists/BPGold.txt")

####Gold med####
Observation_files <- list.files(path=paste0(DataDir, "/GOLD/Clinical"), pattern = ("\\.txt$"))
Observation_files<-paste0(DataDir, "/GOLD/Clinical/", Observation_files)

Observation_files1 <- list.files(path=paste0(DataDir, "/GOLD/Referral"), pattern = ("\\.txt$"))
Observation_files1<-paste0(DataDir, "/GOLD/Referral/", Observation_files1)

Observation_files2 <- list.files(path=paste0(DataDir, "/GOLD/Test"), pattern = ("\\.txt$"))
Observation_files2<-paste0(DataDir, "/GOLD/Test/", Observation_files2)

#Select those that are BP 
PatObsGoldClin<-data.frame()

for (i in 1:(length(Observation_files))) {
  FileName<-Observation_files[i]
  PatObs<-ReadGoldMedObs(FileName)
  PatObs<-select(PatObs, patid, medcode, eventdate, sysdate, enttype, adid)
  PatObs<-merge(x=GoldBP, y=PatObs, by ="medcode", all.y = FALSE, all.x=FALSE)
  PatObsGoldClin<-rbind(PatObsGoldClin, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

PatObsGoldRef<-data.frame()

for (i in 1:(length(Observation_files1))) {
  FileName<-Observation_files1[i]
  PatObs<-ReadGoldMedObs(FileName)
  PatObs<-select(PatObs, patid, medcode, eventdate, sysdate)
  PatObs<-merge(x=GoldBP, y=PatObs, by ="medcode", all.y = FALSE, all.x=FALSE)
  PatObsGoldRef<-rbind(PatObsGoldRef, PatObs)
  print(Observation_files1[i])
}
rm(PatObs)

PatObsGoldTest<-data.frame()

for (i in 1:(length(Observation_files2))) {
  FileName<-Observation_files2[i]
  PatObs<-ReadGoldMedObs(FileName)
  PatObs<-select(PatObs, patid, medcode, eventdate, sysdate, enttype, data1, data2, data3, data4, data5, data6, data7, data8)
  PatObs<-merge(x=GoldBP, y=PatObs, by ="medcode", all.y = FALSE, all.x=FALSE)
  PatObsGoldTest<-rbind(PatObsGoldTest, PatObs)
  print(Observation_files2[i])
}
rm(PatObs)

#Check referals
PatObsGoldRef$adid<-NA
PatObsGoldRef$enttype<-NA
PatObsGoldRef$patdate<-paste0(PatObsGoldRef$patid, PatObsGoldRef$eventdate)
PatObsGoldClin$patdate<-paste0(PatObsGoldClin$patid, PatObsGoldClin$eventdate)
PatObsGoldTest$patdate<-paste0(PatObsGoldTest$patid, PatObsGoldTest$eventdate)

PatObsGoldRef<-subset(PatObsGoldRef, !(patdate %in% PatObsGoldClin$patdate))
PatObsGoldRef<-subset(PatObsGoldRef, !(patdate %in% PatObsGoldTest$patdate))

length(unique(PatObsGoldRef$patid))
length(which(PatObsGoldRef$patid %in% PatObsGoldClin$patid))
length(which(PatObsGoldRef$patid %in% PatObsGoldTest$patid))

PatObsGoldClin<-rbind(PatObsGoldClin, PatObsGoldRef)

####Clean and save####
PatObsGoldClin$eventdate<-as.Date(PatObsGoldClin$eventdate, format="%d/%m/%Y")
PatObsGoldClin$sysdate<-as.Date(PatObsGoldClin$sysdate, format="%d/%m/%Y")

length(which(is.na(PatObsGoldClin$eventdate)))
length(which(is.na(PatObsGoldClin$sysdate)))

length(which(PatObsGoldClin$eventdate<="1900/01/01"))
length(which(PatObsGoldClin$sysdate<="1900/01/01"))

PatObsGoldClin$eventdate[PatObsGoldClin$eventdate<="1900/01/01"]<-NA
PatObsGoldClin$sysdate[PatObsGoldClin$sysdate<="1900/01/01"]<-NA

PatObsGoldClin$eventdate[is.na(PatObsGoldClin$eventdate)]<-PatObsGoldClin$sysdate[is.na(PatObsGoldClin$eventdate)]
PatObsGoldClin$patid<-paste0(PatObsGoldClin$patid, "-G")

save(PatObsGoldClin, file=paste0(ProjectDir,"/Extracts/BPGoldClinical.Rdata"))

PatObsGoldTest$eventdate<-as.Date(PatObsGoldTest$eventdate, format="%d/%m/%Y")
PatObsGoldTest$sysdate<-as.Date(PatObsGoldTest$sysdate, format="%d/%m/%Y")

length(which(is.na(PatObsGoldTest$eventdate)))
length(which(is.na(PatObsGoldTest$sysdate)))

length(which(PatObsGoldTest$eventdate<="1900/01/01"))
length(which(PatObsGoldTest$sysdate<="1900/01/01"))

PatObsGoldTest$eventdate[PatObsGoldTest$eventdate<="1900/01/01"]<-NA
PatObsGoldTest$sysdate[PatObsGoldTest$sysdate<="1900/01/01"]<-NA

PatObsGoldTest$eventdate[is.na(PatObsGoldTest$eventdate)]<-PatObsGoldTest$sysdate[is.na(PatObsGoldTest$eventdate)]
PatObsGoldTest$patid<-paste0(PatObsGoldTest$patid, "-G")

save(PatObsGoldTest, file=paste0(ProjectDir,"/Extracts/BPGoldTest.Rdata"))

####Aurum####
Observation_files <- list.files(path=paste0(DataDir, "/Aurum/Observation"), pattern = ("\\.txt$"))
Observation_files<-paste0(DataDir, "/Aurum/Observation/", Observation_files)

#Select those that are bp

PatObsAurum<-data.frame()

AurumBP<-rename(AurumBP, medcodeid = MedCodeId)

for (i in 1:(length(Observation_files))) {
  FileName<-Observation_files[i]
  PatObs<-ReadAurumMedObs(FileName)
  PatObs<-select(PatObs, patid, medcodeid, obsdate, enterdate, value, numunitid, obstypeid, numrangelow, numrangehigh)
  PatObs<-merge(x=AurumBP, y=PatObs, by ="medcodeid", all.y = FALSE, all.x=FALSE)
  PatObsAurum<-rbind(PatObsAurum, PatObs)
  print(Observation_files[i])
}
rm(PatObs)

####Merge Aurum and Gold files
PatObsAurum$patid<-paste0(PatObsAurum$patid, "-A")

PatObsAurum<-select(PatObsAurum, medcode=medcodeid, readcode=CleansedReadCode, desc=Term, patid, eventdate=obsdate, sysdate=enterdate, value, numunitid, Type, Hypertension)

PatObsAurum$eventdate<-as.Date(PatObsAurum$eventdate, format="%d/%m/%Y")
PatObsAurum$sysdate<-as.Date(PatObsAurum$sysdate, format="%d/%m/%Y")

length(which(is.na(PatObsAurum$eventdate)))
length(which(is.na(PatObsAurum$sysdate)))

length(which(PatObsAurum$eventdate<="1900/01/01"))
length(which(PatObsAurum$sysdate<="1900/01/01"))

PatObsAurum$eventdate[PatObsAurum$eventdate<="1900/01/01"]<-NA
PatObsAurum$sysdate[PatObsAurum$sysdate<="1900/01/01"]<-NA

PatObsAurum$eventdate[is.na(PatObsAurum$eventdate)]<-PatObsAurum$sysdate[is.na(PatObsAurum$eventdate)]

save(PatObsAurum, file=paste0(ProjectDir, "/Extracts/BPAurum.Rdata"))
