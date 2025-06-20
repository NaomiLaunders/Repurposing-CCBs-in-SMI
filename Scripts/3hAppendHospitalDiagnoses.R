####Apppend hospital diagnoses####

#Call start script
source("./Scripts/1aSetUp.R")

load(paste0(ProjectDir, "/Data/HypDate.rdata"))

####Hospital####
HospAurum<-read.table(paste0(DataDir, "/Linkages/Results/Aurum_linked/Final/HES APC/hes_patient_21_000729.txt"), header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character", "gen_hesid" = "character"))

length(unique(HospAurum$patid))
HospAurum<-select(HospAurum, -gen_ethnicity, -match_rank, -pracid)

HospGold<-read.table(paste0(DataDir, "/Linkages/Results/Final Gold/HES APC/hes_patient_21_000729.txt"), header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character", "gen_hesid" = "character"))

length(unique(HospGold$patid))
HospGold<-select(HospGold, -gen_ethnicity, -match_rank, -pracid)

#Convert patid
HospAurum$patid<-paste0(HospAurum$patid, "-A")
HospGold$patid<-paste0(HospGold$patid, "-G")
#Combine as one 
HospRecord<-rbind(HospGold, HospAurum)

HospRecord<-subset(HospRecord, patid %in% HospAll$patid)
length(unique(HospRecord$patid))

####Spells####
#Pull in hospital file which shows spells
SpellA<-read.table(paste0(DataDir, "/Linkages/Results/Aurum_linked/Final/HES APC/hes_hospital_21_000729.txt"), header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))
SpellG<-read.table(paste0(DataDir,"/Linkages/Results/Final Gold/HES APC/hes_hospital_21_000729.txt"), header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))

####Create unique variables in main tables and merge them####

length(unique(SpellA$spno))
length(unique(SpellG$spno))

SpellA$spno<-paste0("A", SpellA$spno)
SpellG$spno<-paste0("G", SpellG$spno)

length(unique(SpellA$patid))
length(unique(SpellG$patid))

SpellA$patid<-paste0(SpellA$patid, "-A")
SpellG$patid<-paste0(SpellG$patid, "-G")

Spell<-rbind(SpellA, SpellG)

Spell$admidate<-as.Date(Spell$admidate, format="%d/%m/%Y")
Spell$discharged<-as.Date(Spell$discharged, format="%d/%m/%Y")
Spell$admimeth<-as.character(Spell$admimeth)

#Limit to our patients
Spell<-subset(Spell, patid %in% HospAll$patid)

#Is spell number unique?
length(unique(Spell$spno))

#Code up spells with detail. These are not excluded here, this was just a first look...
table(Spell$admimeth)

Spell$admiDet[Spell$admimeth=="11"|Spell$admimeth=="12"|Spell$admimeth=="13"]<-"Planned"
Spell$admiDet[Spell$admimeth=="21"]<-"A&E"
Spell$admiDet[Spell$admimeth=="22"]<-"GP emergency"
Spell$admiDet[Spell$admimeth=="23"]<-"Bed bureau emergency"
Spell$admiDet[Spell$admimeth=="24"]<-"Consultant emergency"
Spell$admiDet[Spell$admimeth=="25"]<-"MH emergency"
Spell$admiDet[Spell$admimeth=="28"]<-"Other emergency"
Spell$admiDet[Spell$admimeth=="2A"]<-"Another A&E"
Spell$admiDet[Spell$admimeth=="2B"]<-"Emergency hospital transfer"
Spell$admiDet[Spell$admimeth=="2C"]<-"Maternity"
Spell$admiDet[Spell$admimeth=="2D"]<-"Other emergency"
Spell$admiDet[Spell$admimeth=="31"]<-"Maternity"
Spell$admiDet[Spell$admimeth=="32"]<-"Maternity"
Spell$admiDet[Spell$admimeth=="81"]<-"Planned hospital transfer"
Spell$admiDet[Spell$admimeth=="82"]<-"Maternity"
Spell$admiDet[Spell$admimeth=="83"]<-"Maternity"
Spell$admiDet[Spell$admimeth=="84"]<-"MH emergency"
Spell$admiDet[Spell$admimeth=="89"]<-"Unknown"
Spell$admiDet[Spell$admimeth=="98"]<-"Unknown"
Spell$admiDet[Spell$admimeth=="99"]<-"Unknown"

table(Spell$admiDet, useNA="ifany")

table(Spell$admimeth[is.na(Spell$admiDet)])

#remove spells that are maternity
Spell<-subset(Spell, Spell$admiDet!="Maternity")
length(unique(Spell$spno))

#Remove any with missing discharge dates as these are unfinished spells captured in the next year
Spell<-subset(Spell, !is.na(discharged))

#Create unique idenitifier
Spell$sppat<-paste0(Spell$spno, Spell$patid)

#Bring in episodes
EpisodeA<-read.table(paste0(DataDir,"/Linkages/Results/Aurum_linked/Final/HES APC/hes_episodes_21_000729.txt"), header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))
EpisodeG<-read.table(paste0(DataDir,"/Linkages/Results/Final Gold/HES APC/hes_episodes_21_000729.txt"), header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))

#Code up to match
EpisodeA$spno<-paste0("A", EpisodeA$spno)
EpisodeG$spno<-paste0("G", EpisodeG$spno)

EpisodeA$epikey<-paste0("A", EpisodeA$epikey)
EpisodeG$epikey<-paste0("G", EpisodeG$epikey)

EpisodeA$patid<-paste0(EpisodeA$patid, "-A")
EpisodeG$patid<-paste0(EpisodeG$patid, "-G")

Episode<-rbind(EpisodeA, EpisodeG)

#Limit it to our patients
Episode<-subset(Episode, patid %in% Spell$patid)
Episode<-subset(Episode, spno %in% Spell$spno)

#Is spell number unique to a patient?
Episode<-Episode%>%
  group_by(spno)%>%
  mutate(patid_n=length(unique(patid)))
table(Episode$patid_n)
#Yes

#Is episode number unique to a patient?
Episode<-Episode%>%
  ungroup()%>%
  group_by(epikey)%>%
  mutate(patid_n=length(unique(patid)))
table(Episode$patid_n)

#Yes - but not over whole data set so create linking variable
Episode$EpiPat<-paste0(Episode$epikey, Episode$patid)
Episode$sppat<-paste0(Episode$spno, Episode$patid)

length(unique(Episode$sppat))

#Spells can have multiple episodes, but episodes should only have one spell
Episode<-Episode%>%
  ungroup()%>%
  group_by(EpiPat)%>%
  mutate(spno_n=length(unique(sppat)))%>%
  ungroup()
table(Episode$spno_n)
#Yes.

#Patients can have multiple spells, but spells should only have one patient
Episode<-Episode%>%
  ungroup()%>%
  group_by(sppat)%>%
  mutate(pat_n=length(unique(patid)))%>%
  ungroup()
table(Episode$pat_n)
#Yes.

#Just check
length(which(!(Episode$patid %in% HospAll$patid)))
length(which(!(Episode$sppat %in% Spell$sppat)))
length(which(!(Episode$patid %in% Spell$patid)))

#Are there any class 2 (day case) emergency admissions
table(Episode$classpat, Episode$admimeth)

#Just check regular attenders
CheckDay<-subset(Episode, classpat==3&eorder==1)
length(unique(CheckDay$spno))

CheckNight<-subset(Episode, classpat==4&eorder==1)
length(unique(CheckNight$spno))

#Only keep episodes that are ordinary or day admissions
table(Episode$classpat, useNA="ifany")
Episode<-subset(Episode, classpat==1|classpat==2)

#Only keep episodes where epitype is "general episode" - the rest are maternity or a weird psych one that's rarely used.
table(Episode$epitype, useNA="ifany")
Checkbirth<-subset(Episode, (epitype==2| epitype==3) &eorder==1)
Episode<-subset(Episode, epitype==1)

#Only keep spells that have one episode at least
Spell<-subset(Spell, sppat %in% Episode$sppat)

#Merge back to spells and count episodes per spell
EpisodePerSpell<-Episode%>%
  group_by(sppat)%>%
  summarise(EpiPerSpell=n(), minEOrder=min(eorder))

length(unique(EpisodePerSpell$sppat))

#Only include spells with at least one episode (because I've excluded the episodes)
table(EpisodePerSpell$EpiPerSpell, useNA="ifany")
Spell<-merge(x=Spell, y=EpisodePerSpell, by="sppat", all.x=FALSE, all.y=TRUE)

#Only include those which have a first episode. If they don't it's because I've already excluded the first episode and so it's not a hospitalisation that I'm interested in
length(which(Spell$minEOrder>1))
table(Spell$EpiPerSpell, useNA="ifany")
Spell<-subset(Spell, minEOrder==1)

#Remove those episodes as well
Episode<-subset(Episode, sppat %in% Spell$sppat)
Spell<-subset(Spell, sppat %in% Episode$sppat)

length(unique(Spell$patid))
length(unique(Episode$patid))

#How many are split between years: very few - so after checking I chose to ignore this.(111 potential)
Spell$DischargeDay<-day(Spell$discharged)
Spell$DischargeMonth<-month(Spell$discharged)
Spell$AdmiDay<-day(Spell$admidate)
Spell$AdmiMonth<-month(Spell$admidate)

Spell<-Spell%>%
  group_by(patid)%>%
  mutate(SpellPerPat=n())

length(which(Spell$DischargeDay==31&Spell$DischargeMonth==3))
PotentialSPlitEpisodesDis<-subset(Spell, Spell$DischargeDay==31&Spell$DischargeMonth==3&SpellPerPat>1)
PotentialSPlitEpisodesDis<-select(PotentialSPlitEpisodesDis, patid, admidate, discharged, admimeth, dismeth, duration)
PotentialSPlitEpisodesAdm<-subset(Spell, Spell$AdmiDay==1&Spell$AdmiMonth==4&SpellPerPat>1)
PotentialSPlitEpisodesAdm<-select(PotentialSPlitEpisodesAdm, patid, admidate, discharged, admimeth, dismeth, duration)

Split<-merge(x=PotentialSPlitEpisodesDis, y=PotentialSPlitEpisodesAdm, by="patid", all.x=FALSE, all.y=FALSE)
Split<-subset(Split, year(admidate.x)==year(admidate.y))

#How many spells per patient do I currently have?
table(Spell$SpellPerPat)

#Pull in all diagnoses
AllDiagA<-read.table(paste0(DataDir,"/Linkages/Results/Aurum_linked/Final/HES APC/hes_diagnosis_epi_21_000729.txt"), header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))
AllDiagG<-read.table(paste0(DataDir,"/Linkages/Results/Final Gold/HES APC/hes_diagnosis_epi_21_000729.txt"), header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character"))

AllDiagA$spno<-paste0("A", AllDiagA$spno)
AllDiagG$spno<-paste0("G", AllDiagG$spno)

AllDiagA$epikey<-paste0("A", AllDiagA$epikey)
AllDiagG$epikey<-paste0("G", AllDiagG$epikey)

AllDiagA$patid<-paste0(AllDiagA$patid, "-A")
AllDiagG$patid<-paste0(AllDiagG$patid, "-G")

AllDiag<-rbind(AllDiagA, AllDiagG)

AllDiag$ICD<-as.character(AllDiag$ICD)
AllDiag$ICDx<-as.character(AllDiag$ICDx)

AllDiag$sppat<-paste0(AllDiag$spno, AllDiag$patid)
AllDiag$EpiPat<-paste0(AllDiag$epikey, AllDiag$patid)

#Limit to our patients
AllDiag<-subset(AllDiag, patid %in% HospAll$patid)
length(unique(AllDiag$sppat))

#Only select those diagnoses where the spell number is in the list of spells
AllDiag<-subset(AllDiag, sppat %in% Spell$sppat)
length(unique(AllDiag$sppat))
length(unique(AllDiag$patid))

#Is spell number unique?
AllDiag<-AllDiag%>%
  group_by(sppat)%>%
  mutate(patid_n=length(unique(patid)))
table(AllDiag$patid_n)

length(which(Spell$sppat %in% AllDiag$sppat))
table(Spell$sppat[!(Spell$sppat %in% AllDiag$sppat)])

#Is epikey unique for each patient
AllDiag<-AllDiag%>%
  ungroup()%>%
  group_by(EpiPat)%>%
  mutate(patid_n=length(unique(patid)))
table(AllDiag$patid_n)

#Is epikey unique for each spell
AllDiag<-AllDiag%>%
  ungroup()%>%
  group_by(EpiPat)%>%
  mutate(spno_n_n=length(unique(sppat)))
table(AllDiag$spno_n_n)

#Only select those for episodes that we have included
AllDiag<-subset(AllDiag, EpiPat %in% Episode$EpiPat)

#Pull in HospAll CCB date, admission date and emergency status
Dates<-select(HospAll, patid, FirstCCBARB)
AllDiag<-merge(x=AllDiag, y=Dates, by="patid", all.x=TRUE, all.y=FALSE)

Spell$Emergency<-1
Spell$Emergency[Spell$admiDet=="Planned hospital transfer"|Spell$admiDet=="Planned"]<-0

Admit<-Spell%>%
  ungroup()%>%
  select(sppat, admidate, Emergency)

AllDiag<-merge(x=AllDiag, y=Admit, by="sppat", all.x=TRUE, all.y=FALSE)

#####Add in physical health diagnoses####
Physical<-AllDiag%>%
  mutate(Phys = case_when(startsWith(ICD, "E11")|startsWith(ICD, "E12")|startsWith(ICD, "E13")|
           startsWith(ICD, "E14")|startsWith(ICD, "H36.0")|startsWith(ICD, "M14.2")|
         startsWith(ICD, "G59.0")|startsWith(ICD, "G63.2")|startsWith(ICD, "H28.0")|
           startsWith(ICD, "O24.1")|startsWith(ICD, "N08.3") ~ "HospDiabetes",
         startsWith(ICD, "F00") | startsWith(ICD, "F01")|startsWith(ICD, "F02")|startsWith(ICD, "F03")|
           startsWith(ICD, "F05.1")|startsWith(ICD, "G30")|startsWith(ICD, "G31.1") ~ "HospDementia",
         startsWith(ICD, "I21") | startsWith(ICD, "I22")|startsWith(ICD, "I23")|startsWith(ICD, "I241")|
           startsWith(ICD, "I25.2") ~ "HospMI",
         startsWith(ICD, "G45") | startsWith(ICD, "G46")|startsWith(ICD, "H34.0")|startsWith(ICD, "I60")|
           startsWith(ICD, "I61")|startsWith(ICD, "I62")|startsWith(ICD, "I63")|
           startsWith(ICD, "I64")|startsWith(ICD, "I65")|startsWith(ICD, "I66")|startsWith(ICD, "I67")|
           startsWith(ICD, "I68")|startsWith(ICD, "I69") ~ "HospCereb",
         startsWith(ICD, "I11.0") | startsWith(ICD, "I13.0")|startsWith(ICD, "I13.2")|startsWith(ICD, "I50") ~ "HospHF",
         TRUE ~ "Drop"))%>%
  subset(Phys!="Drop")

Physical<-Physical%>%
  subset(admidate<=FirstCCBARB)%>%
  select(patid, Phys)%>%
  mutate(value=1)%>%
  distinct()%>%
  pivot_wider(names_from = Phys, values_from = value, values_fill = 0)

#Compare to primary care
DiabPrimary<-subset(HospAll, !is.na(FirstDiab) & FirstDiab<=FirstCCBARB)
Diab<-subset(Physical, HospDiabetes==1)
length(which(DiabPrimary$patid %in% Diab$patid))
length(which(Diab$patid %in% DiabPrimary$patid))

length(unique(Physical$patid))

#Merge diagnoses onto main cohort
HospAll<-merge(x=HospAll, y=Physical, by="patid", all.x=TRUE, all.y=FALSE)
HospAll<-HospAll%>%
  mutate_at(c(79:83), ~replace(., is.na(.), 0))

#####Save HospAll and AllDiag for future use####
#Add in epiorder
Epi<-select(Episode, EpiPat, eorder)
AllDiag<-merge(x=AllDiag, y=Epi, by="EpiPat", all.x=TRUE, all.y=FALSE)
save(AllDiag, file = paste0(ProjectDir, "/Data/HospDiagnoses.rdata"))
save(HospAll, file = paste0(ProjectDir, "/Data/HospPhys.rdata"))
