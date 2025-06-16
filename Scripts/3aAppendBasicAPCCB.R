####Append basic antipsychotics and CCBs to the main cohort####

#Call start script
source("./Scripts/1aSetUp.R")

####Load SMI cohort####
load(paste0(DataDir, "/MergedFile/SMI2023Final.Rdata"))
CPRD<-AllSMI2023Final
rm(AllSMI2023Final)

####Sort out CCBs####
load(paste0(ProjectDir,"/Extracts/AllCCBs.Rdata"))

#sort out combos
table(AllCCB$ingredient[AllCCB$Class=="Combo"], useNA="ifany")

AllCCB<-AllCCB%>%
  mutate(
    Class = case_when(Name == "Combo" & grepl("ipine", ingredient, ignore.case=TRUE) & grepl("sartan", ingredient, ignore.case=TRUE) ~ "Combo",
                           Name == "Combo" & grepl("ipine", ingredient, ignore.case=TRUE) & grepl("pril", ingredient, ignore.case=TRUE) ~ "Combo",
                           Name == "Combo" & grepl("ipine", ingredient, ignore.case=TRUE) & grepl("Verapamil", ingredient, ignore.case=TRUE) ~ "Combo",
                           Name == "Combo" & grepl("Verapamil", ingredient, ignore.case=TRUE) & grepl("pril", ingredient, ignore.case=TRUE) ~ "Combo",
                           Name == "Combo" & grepl("Verapamil", ingredient, ignore.case=TRUE) & grepl("sartan", ingredient, ignore.case=TRUE) ~ "Combo",
                           Name == "Combo" & grepl("pril", ingredient, ignore.case=TRUE) & grepl("sartan", ingredient, ignore.case=TRUE) ~ "Combo",
                      
                      Name == "Combo" & grepl("Amlodipine", ingredient, ignore.case=TRUE) & grepl("olol", ingredient, ignore.case=TRUE) ~ "NBBP_CCB+BBlocker",
                      Name == "Combo" & grepl("ipine", ingredient, ignore.case=TRUE) & grepl("olol", ingredient, ignore.case=TRUE) ~ "BBP_CCB+BBlocker",
    Name == "Combo" & grepl("Amlodipine", ingredient, ignore.case=TRUE) & grepl("thiazide", ingredient, ignore.case=TRUE) ~ "NBBP_CCB+Diuretic",
    Name == "Combo" & grepl("ipine", ingredient, ignore.case=TRUE) & grepl("thiazide", ingredient, ignore.case=TRUE) ~ "BBP_CCB+Diuretic",
    Name == "Combo" & (grepl("benazepril", ingredient, ignore.case=TRUE)|grepl("Enal", ingredient, ignore.case=TRUE)|grepl("moexipril", ingredient, ignore.case=TRUE)|grepl("Imidapril", ingredient, ignore.case=TRUE)|grepl("quinapril", ingredient, ignore.case=TRUE)) & grepl("olol", ingredient, ignore.case=TRUE) ~ "NBBP_ACE+BBlocker",
    Name == "Combo" & grepl("pril", ingredient, ignore.case=TRUE) & grepl("olol", ingredient, ignore.case=TRUE) ~ "BBP_ACE+BBlocker",
    Name == "Combo" & (grepl("benazepril", ingredient, ignore.case=TRUE)|grepl("Enal", ingredient, ignore.case=TRUE)|grepl("moexipril", ingredient, ignore.case=TRUE)|grepl("Imidapril", ingredient, ignore.case=TRUE)|grepl("quinapril", ingredient, ignore.case=TRUE)) & grepl("thiazide", ingredient, ignore.case=TRUE) ~ "NBBP_ACE+Diuretic",
    Name == "Combo" & grepl("pril", ingredient, ignore.case=TRUE) & grepl("thiazide", ingredient, ignore.case=TRUE) ~ "BBP_ACE+Diuretic",
    Name == "Combo" & (grepl("Candesartan", ingredient, ignore.case=TRUE)|grepl("Telmisartan", ingredient, ignore.case=TRUE)) & grepl("olol", ingredient, ignore.case=TRUE) ~ "BBP_ARB+BBlocker",
    Name == "Combo" & grepl("sartan", ingredient, ignore.case=TRUE) & grepl("olol", ingredient, ignore.case=TRUE) ~ "NBBP_ARB+BBlocker",
    Name == "Combo" & (grepl("Candesartan", ingredient, ignore.case=TRUE)|grepl("Telmisartan", ingredient, ignore.case=TRUE)) & grepl("thiazide", ingredient, ignore.case=TRUE) ~ "BBP_ARB+Diuretic",
    Name == "Combo" & grepl("sartan", ingredient, ignore.case=TRUE) & grepl("thiazide", ingredient, ignore.case=TRUE) ~ "NBBP_ARB+Diuretic",
    Name == "Combo" & grepl("Verapamil", ingredient, ignore.case=TRUE) & grepl("olol", ingredient, ignore.case=TRUE) ~ "Verapamil+BBlocker",
    Name == "Combo" & grepl("Verapamil", ingredient, ignore.case=TRUE) & grepl("thiazide", ingredient, ignore.case=TRUE) ~ "Verapamil+Diuretic",
                             TRUE ~ Class))

table(AllCCB$ingredient[AllCCB$Name=="Combo"], AllCCB$Class[AllCCB$Name=="Combo"], useNA="ifany")

table(AllCCB$Class)

table(AllCCB$Name[AllCCB$Class=="ACE"])

YOB<-select(CPRD, patid, yob)
length(which(AllCCB$patid %in% CPRD$patid))

CCBProd<-subset(AllCCB, patid %in% CPRD$patid)

CCBProd<-merge(x=CCBProd, y=YOB, by="patid", all.x=TRUE, all.y=FALSE)

CCBProd<-subset(CCBProd, issuedate<="2019-12-31" & year(issuedate)>yob) #Because ever diagnosed

table(CCBProd$Class)

####Define "any oral antihypertensive"####
#Remove those that are not for hypertension
OralHT<-subset(CCBProd, Group!="6 month exclusion")
table(OralHT$Route, useNA="ifany")

NoRoute<-subset(OralHT, Route=="")
Check<-subset(OralHT, Route=="Route of administration not applicable")


OralHT<-OralHT%>%
  mutate(Route = case_when((Route==""|Route=="Route of administration not applicable") & grepl("oral|tab|cap|Titration pack", Term.from.EMIS, ignore.case=TRUE) ~ "Oral",
                           (Route==""|Route=="Route of administration not applicable") & grepl("oral|tab|cap|Titration pack", productname, ignore.case=TRUE) ~ "Oral",
                           TRUE ~ Route))
         
OralHT$Route[OralHT$Route==""]<-"Oral"         
OralHT<-subset(OralHT, Route=="Oral")

Exclude<-subset(CCBProd, !(prodcode %in% OralHT$prodcode))

#If not oral, then put it in the 6 month exclusion group         
AllCCB$Group<-case_when(AllCCB$prodcode %in% Exclude$prodcode ~ "6 month exclusion",
                        TRUE ~ AllCCB$Group)

####Save AllCCB####
save(AllCCB, file = paste0(ProjectDir, "/Extracts/AllCCBs.Rdata"))
rm(AllCCB)
         
####First ever antihypertensive####
AntiHyp<-OralHT%>%
  group_by(patid)%>%
  mutate(FirstAntiHypDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstAntiHypDate)%>%
  distinct()

length(unique(AntiHyp$patid))

CPRD<-merge(x=CPRD, y=AntiHyp, by="patid", all.x=TRUE, all.y=FALSE)
rm(AntiHyp)

####Set trial arms####
OralHT<-OralHT%>%
  mutate(Arm = case_when(Name == "Amlodipine" ~ "NBBP CCB",
                         Class == "CCB" ~ "BBP CCB",
                         Name=="Candesartan" | Name == "Telmisartan" ~ "BBP ARB",
                         Class == "ARB" ~  "NBBP ARB",
                         Name == "Benazepril" | Name== "Enalapril" |Name== "Enalopril" | Name=="Moexipril" | Name=="Quinapril"|Name=="Imidapril" ~ "NBBP ACE",
                         Class == "ACE" ~  "BBP ACE",
                         TRUE ~ Class))

table(OralHT$Arm, OralHT$Name)

####First CCB####
NBBP_CCB<-OralHT%>%
  subset(Arm=="NBBP CCB")%>%
  group_by(patid)%>%
  mutate(FirstNBBP_CCBDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstNBBP_CCBDate)%>%
  distinct()

length(unique(NBBP_CCB$patid))

CPRD<-merge(x=CPRD, y=NBBP_CCB, by="patid", all.x=TRUE, all.y=FALSE)
rm(NBBP_CCB)

BBP_CCB<-OralHT%>%
  subset(Arm=="BBP CCB")%>%
  group_by(patid)%>%
  mutate(FirstBBP_CCBDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstBBP_CCBDate)%>%
  distinct()

length(unique(BBP_CCB$patid))

CPRD<-merge(x=CPRD, y=BBP_CCB, by="patid", all.x=TRUE, all.y=FALSE)
rm(BBP_CCB)

####First Verapamil####
Verapamil<-OralHT%>%
  subset(Arm=="Verapamil")%>%
  group_by(patid)%>%
  mutate(FirstVerapamilDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstVerapamilDate)%>%
  distinct()

length(unique(Verapamil$patid))

CPRD<-merge(x=CPRD, y=Verapamil, by="patid", all.x=TRUE, all.y=FALSE)
rm(Verapamil)

####First ARB####
NBBP_ARB<-OralHT%>%
  subset(Arm=="NBBP ARB")%>%
  group_by(patid)%>%
  mutate(FirstNBBP_ARBDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstNBBP_ARBDate)%>%
  distinct()

length(unique(NBBP_ARB$patid))

CPRD<-merge(x=CPRD, y=NBBP_ARB, by="patid", all.x=TRUE, all.y=FALSE)
rm(NBBP_ARB)

BBP_ARB<-OralHT%>%
  subset(Arm=="BBP ARB")%>%
  group_by(patid)%>%
  mutate(FirstBBP_ARBDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstBBP_ARBDate)%>%
  distinct()

length(unique(BBP_ARB$patid))

CPRD<-merge(x=CPRD, y=BBP_ARB, by="patid", all.x=TRUE, all.y=FALSE)
rm(BBP_ARB)

####First ACE####
NBBP_ACE<-OralHT%>%
  subset(Arm=="NBBP ACE")%>%
  group_by(patid)%>%
  mutate(FirstNBBP_ACEDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstNBBP_ACEDate)%>%
  distinct()

length(unique(NBBP_ACE$patid))

CPRD<-merge(x=CPRD, y=NBBP_ACE, by="patid", all.x=TRUE, all.y=FALSE)
rm(NBBP_ACE)

BBP_ACE<-OralHT%>%
  subset(Arm=="BBP ACE")%>%
  group_by(patid)%>%
  mutate(FirstBBP_ACEDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstBBP_ACEDate)%>%
  distinct()

length(unique(BBP_ACE$patid))

CPRD<-merge(x=CPRD, y=BBP_ACE, by="patid", all.x=TRUE, all.y=FALSE)
rm(BBP_ACE)

####First Combo####
Combo<-OralHT%>%
  subset(Arm=="Combo")%>%
  group_by(patid)%>%
  mutate(FirstComboDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstComboDate)%>%
  distinct()

length(unique(Combo$patid))

CPRD<-merge(x=CPRD, y=Combo, by="patid", all.x=TRUE, all.y=FALSE)
rm(Combo)

Diuretic_BBP_ACE<-OralHT%>%
  subset(Arm=="BBP_ACE+Diuretic")%>%
  group_by(patid)%>%
  mutate(FirstDiuretic_BBP_ACEDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstDiuretic_BBP_ACEDate)%>%
  distinct()

length(unique(Diuretic_BBP_ACE$patid))

CPRD<-merge(x=CPRD, y=Diuretic_BBP_ACE, by="patid", all.x=TRUE, all.y=FALSE)
rm(Diuretic_BBP_ACE)


Diuretic_BBP_ARB<-OralHT%>%
  subset(Arm=="BBP_ARB+Diuretic")%>%
  group_by(patid)%>%
  mutate(FirstDiuretic_BBP_ARBDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstDiuretic_BBP_ARBDate)%>%
  distinct()

length(unique(Diuretic_BBP_ARB$patid))

CPRD<-merge(x=CPRD, y=Diuretic_BBP_ARB, by="patid", all.x=TRUE, all.y=FALSE)
rm(Diuretic_BBP_ARB)

Blocker_BBP_CCB<-OralHT%>%
  subset(Arm=="BBP_CCB+BBlocker")%>%
  group_by(patid)%>%
  mutate(FirstBlocker_BBP_CCBDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstBlocker_BBP_CCBDate)%>%
  distinct()

length(unique(Blocker_BBP_CCB$patid))

CPRD<-merge(x=CPRD, y=Blocker_BBP_CCB, by="patid", all.x=TRUE, all.y=FALSE)
rm(Blocker_BBP_CCB)

Diuretic_NBBP_ACE<-OralHT%>%
  subset(Arm=="NBBP_ACE+Diuretic")%>%
  group_by(patid)%>%
  mutate(FirstDiuretic_NBBP_ACEDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstDiuretic_NBBP_ACEDate)%>%
  distinct()

length(unique(Diuretic_NBBP_ACE$patid))

CPRD<-merge(x=CPRD, y=Diuretic_NBBP_ACE, by="patid", all.x=TRUE, all.y=FALSE)
rm(Diuretic_NBBP_ACE)

Diuretic_NBBP_ARB<-OralHT%>%
  subset(Arm=="NBBP_ARB+Diuretic")%>%
  group_by(patid)%>%
  mutate(FirstDiuretic_NBBP_ARBDate=min(issuedate, na.rm=TRUE))%>%
  select(patid, FirstDiuretic_NBBP_ARBDate)%>%
  distinct()

length(unique(Diuretic_NBBP_ARB$patid))

CPRD<-merge(x=CPRD, y=Diuretic_NBBP_ARB, by="patid", all.x=TRUE, all.y=FALSE)
rm(Diuretic_NBBP_ARB)

####Any antihypertensive in medical record####
load(paste0(ProjectDir, "/Extracts/CCBMedAll.Rdata"))

length(which(PatCCBMed$patid %in% CPRD$patid))
PatCCBMed<-subset(PatCCBMed, patid %in% CPRD$patid)

PatCCBMed<-merge(x=PatCCBMed, y=YOB, by="patid", all.x=TRUE, all.y=FALSE)

CCBMed<-subset(PatCCBMed, eventdate<="2019-12-31" & year(eventdate)>yob) #Because ever diagnosed

table(CCBMed$Term)
table(CCBMed$Type)
table(CCBMed$Term[CCBMed$Type=="not prescribed"])

CCBMed<-subset(CCBMed, Type!="not prescribed")

####First medcode CCB####
CCBMed<-CCBMed%>%
  group_by(patid)%>%
  mutate(FirstEvidenceAntiHypDate=min(eventdate, na.rm=TRUE))%>%
  select(patid, FirstEvidenceAntiHypDate)%>%
  distinct()

length(unique(CCBMed$patid))

CPRD<-merge(x=CPRD, y=CCBMed, by="patid", all.x=TRUE, all.y=FALSE)
rm(CCBMed)

save(CPRD, file = paste0(ProjectDir, "/Data/AntiHypAppend.Rdata"))

####AP Products####
load(paste0(ProjectDir,"/StatinExtracts/APProd.Rdata"))

PatAPAll<-subset(PatAPAll, patid %in% CPRD$patid)

PatAPAll<-merge(x=PatAPAll, y=YOB, by="patid", all.x=TRUE, all.y=FALSE)

APProd<-subset(PatAPAll, eventdate<="2019-12-31" & year(eventdate)>yob) #Because ever diagnosed
rm(PatAPAll)

table(APProd$AP)
table(APProd$Gen)

####First any psych
Psych<-APProd%>%
  group_by(patid)%>%
  mutate(FirstPsychDate=min(eventdate), na.rm=TRUE)%>%
  select(patid, FirstPsychDate)%>%
  distinct()

length(unique(Psych$patid))

CPRD<-merge(x=CPRD, y=Psych, by="patid", all.x=TRUE, all.y=FALSE)
rm(Psych)

####First Prescribed AP####
AP<-subset(APProd, Gen!=3)
AP<-AP%>%
  group_by(patid)%>%
  mutate(FirstAPDate=min(eventdate), na.rm=TRUE)%>%
  select(patid, FirstAPDate)%>%
  distinct()

length(unique(AP$patid))

CPRD<-merge(x=CPRD, y=AP, by="patid", all.x=TRUE, all.y=FALSE)
rm(AP)

####First BP med####
BP<-subset(APProd, Gen==3)

BP<-BP%>%
  group_by(patid)%>%
  mutate(FirstBPDate=min(eventdate), na.rm=TRUE)%>%
  select(patid, FirstBPDate)%>%
  distinct()

CPRD<-merge(x=CPRD, y=BP, by="patid", all.x=TRUE, all.y=FALSE)
rm(BP)

####Any AP indication####
load(paste0(ProjectDir,"/StatinExtracts/APMed.Rdata"))

PatAPMedAll<-subset(PatAPMedAll, patid %in% CPRD$patid)
PatAPMedAll<-merge(x=PatAPMedAll, y=YOB, by="patid", all.x=TRUE, all.y=FALSE)

APMed<-subset(PatAPMedAll, eventdate<="2019-12-31" & year(eventdate)>yob) #Because ever diagnosed
rm(PatAPMedAll)

table(APMed$AP)
table(APMed$term)

APMed<-subset(APMed, term!="fetal valproate syndrome" & term!="Fetal valproate syndrome")

table(APMed$term[APMed$AP=="EvidenceOfInjectables"])

####First any psych
Psych<-APMed%>%
  group_by(patid)%>%
  mutate(FirstEvidencePsychDate=min(eventdate), na.rm=TRUE)%>%
  select(patid, FirstEvidencePsychDate)%>%
  distinct()

length(unique(Psych$patid))

CPRD<-merge(x=CPRD, y=Psych, by="patid", all.x=TRUE, all.y=FALSE)
rm(Psych)

####First medcode AP####
AP<-subset(APMed, AP!="EvidenceOfBP")
AP<-AP%>%
  group_by(patid)%>%
  mutate(FirstEvidenceAPDate=min(eventdate), na.rm=TRUE)%>%
  select(patid, FirstEvidenceAPDate)%>%
  distinct()

length(unique(AP$patid))

CPRD<-merge(x=CPRD, y=AP, by="patid", all.x=TRUE, all.y=FALSE)
rm(AP)

####First medcode BP####
BP<-subset(APMed, AP=="EvidenceOfBP")
BP<-BP%>%
  group_by(patid)%>%
  mutate(FirstEvidenceBPDate=min(eventdate), na.rm=TRUE)%>%
  select(patid, FirstEvidenceBPDate)%>%
  distinct()

length(unique(BP$patid))

CPRD<-merge(x=CPRD, y=BP, by="patid", all.x=TRUE, all.y=FALSE)
rm(BP)

####First medcode injectables####
Inj<-subset(APMed, AP=="EvidenceOfInjectables")
Inj<-Inj%>%
  group_by(patid)%>%
  mutate(FirstEvidenceInjDate=min(eventdate), na.rm=TRUE)%>%
  select(patid, FirstEvidenceInjDate)%>%
  distinct()

length(unique(Inj$patid))

CPRD<-merge(x=CPRD, y=Inj, by="patid", all.x=TRUE, all.y=FALSE)
rm(Inj)



save(CPRD, file = paste0(ProjectDir, "/Data/CPRD_AP.Rdata"))
