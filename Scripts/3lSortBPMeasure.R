####Sort BP####

#Call start script
source("./Scripts/1aSetUp.R")

#Need to check values are numeric!

load(paste0(ProjectDir, "/Extracts/BPAurum.Rdata"))
load(paste0(ProjectDir, "/Extracts/BPGoldClinical.Rdata"))
load(paste0(ProjectDir, "/Extracts/BPGoldTest.Rdata"))

load(paste0(ProjectDir, "/Data/CPRD_Phys.rdata"))

PatTest<-subset(PatObsGoldTest, patid %in% Trial$patid)
PatClin<-subset(PatObsGoldClin, patid %in% Trial$patid)
PatBPAurum<-subset(PatObsAurum, patid %in% Trial$patid)

#####Values: Aurum#####
numunits <- ReadGeneral(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/NumUnit.txt"))
obstype <- ReadGeneral(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/ObsType.txt"))

BPAurumValue<-merge(x=PatBPAurum, y=numunits, by="numunitid", all.x=TRUE, all.y=FALSE)

#Only select those with values
BPAurumValue<-subset(BPAurumValue, !is.na(value)& value!=0)
summary(BPAurumValue$value)
BPAurumValue$patdate<-paste0(BPAurumValue$patid, BPAurumValue$eventdate)

#If negative make positive

BPAurumValue$value[BPAurumValue$value<0]<-BPAurumValue$value[BPAurumValue$value<0]*-1

#Check obs types
Unit<-BPAurumValue%>%
  group_by(Description)%>%
  summarise(Count=n())

#check term if unit is systolic or diastolic
table(BPAurumValue$desc[BPAurumValue$Description=="Systolic"])
table(BPAurumValue$desc[BPAurumValue$Description=="Diastolic"])

Term<-BPAurumValue%>%
  group_by(desc)%>%
  summarise(Count=n())

TermUnit<-BPAurumValue%>%
  group_by(desc, Description)%>%
  summarise(Count=n())

#All units now make sense - most appear to be mmHg

####Label up terms####

BPAurumValue <- BPAurumValue %>%
  mutate(Systolic= case_when(grepl("systol", desc) ~ 1, 
                             TRUE ~ 0),
         Diastolic = case_when(grepl("diastol", desc) ~ 1,
                               TRUE ~ 0),
         Mean =    case_when(Systolic==0 & Diastolic==0 ~ 1, 
                             TRUE ~ 0))

#Is the mean, really mean?
table(BPAurumValue$desc[BPAurumValue$Systolic==1])
table(BPAurumValue$desc[BPAurumValue$Diastolic==1])
table(BPAurumValue$desc[BPAurumValue$Mean==1])

BPAurumValue<-distinct(BPAurumValue)

#Flag if ambulatory, or clinical
BPAurumValue <- BPAurumValue %>%
  mutate(AmbClin = case_when(grepl("amb|average|home|24|continuous", desc) ~ "Ambulatory", 
                            TRUE ~ "Clinical"))

Check<-select(BPAurumValue, desc, AmbClin)
Check<-distinct(Check)


#If multiple just take those that are sys/dia
AurumSys<-subset(BPAurumValue, Systolic==1)
AurumDia<-subset(BPAurumValue, Diastolic==1)
AurumMean<-subset(BPAurumValue, Mean==1)

AurumMean<-subset(AurumMean, !(patdate %in% AurumSys$patdate) & !(patdate %in% AurumDia$patdate))

#Only include mean if in sensible range
AurumMean<-subset(AurumMean, value>20 & value<300)

AurumMean<-AurumMean%>%
  group_by(patdate)%>%
  mutate(SameDay=n(), maxval=max(value), minval=min(value))

#If multiple on the same day can probably assume sys/dia
AurumMeanKeep<-subset(AurumMean, SameDay==1)
AurumMeanKeep<-select(AurumMeanKeep, -minval, -maxval, -SameDay)

AurumMeanFix<-subset(AurumMean, SameDay>1)

AurumMeanFix<-AurumMeanFix%>%
  subset(SameDay==2)%>%
  group_by(patdate)%>%
  mutate(Mean= 0,
         Systolic = case_when(value==maxval ~ 1,
                              TRUE ~ 0),
         Diastolic = case_when(value==minval ~ 1,
                               TRUE ~ 0))%>%
  select(-minval, -maxval, -SameDay)

BPAurumValue<-rbind(AurumSys, AurumDia, AurumMeanKeep, AurumMeanFix)

#Change to long format
BPAurumLong<-BPAurumValue%>%
  pivot_longer(13:15, names_to = "Label", values_to = "Count")%>%
  subset(Count>0)%>%
  select(-Count)

table(BPAurumLong$Label, useNA="ifany")

tapply(BPAurumLong$value, BPAurumLong$Label, summary)

rm(Term, BPAurumValue, PatBPAurum, Unit, obstype, numunits)
rm(AurumSys, AurumDia, AurumMeanKeep, AurumMeanFix)
rm(AurumMean, Check, TermUnit)

####Sort out Gold####
entity <- ReadGeneral(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGold/entity.txt"))
opr <- ReadGeneral(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGold/TXTFILES/OPR.txt"))
sum <- ReadGeneral(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGold/TXTFILES/SUM.txt"))
tqu <- ReadGeneral(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGold/TXTFILES/TQU.txt"))

Ent<-select(entity, enttype, description)

Additional<-read.table(paste0(DataDir, "/GOLD/Additional/SMI_GOLD_Extract_Additional_001.txt"), header=TRUE, fill=TRUE, sep="\t", colClasses = c("patid"="character"))
Additional$patid<-paste0(Additional$patid, "-G")

#How many of those with additional info are for blood pressure
Additional<-subset(Additional, patid %in% Trial$patid)
Additional<-merge(x=Additional, y=Ent, by="enttype", all.x=TRUE, all.y=FALSE)
table(Additional$description)
BPAdd<-subset(Additional, description=="Blood pressure")
length(which(BPAdd$patid %in% PatClin$patid))

BPAdd$patad<-paste0(BPAdd$patid, "-", BPAdd$adid)
PatClin$patad<-paste0(PatClin$patid, "-", PatClin$adid)
length(which(BPAdd$patad %in% PatClin$patad))

BPAdd<-select(BPAdd, -patid, -enttype, -adid)

BPValue<-merge(x=PatClin, y=BPAdd, by="patad", all.x=FALSE, all.y=FALSE)

#Nothing of note in the additional table
#Try the test table
PatTest<-merge(x=PatTest, y=Ent, by="enttype", all.x=TRUE, all.y=FALSE)

#Look at categories
TQUVals<-subset(PatClin, enttype==392)
TQUVals2<-subset(PatClin, enttype==411|enttype==288 )

#Back to values
PatClin<-merge(x=PatClin, y=Ent, by="enttype", all.x=TRUE, all.y=FALSE)
table(PatClin$enttype, PatClin$description)
table(BPValue$enttype, BPValue$description)

#Flag if ambulatory, or clinical
BPValue <- BPValue %>%
  mutate(AmbClin = case_when(grepl("amb|average|home|24|continuous", desc) ~ "Ambulatory", 
                             TRUE ~ "Clinical"))

Check<-select(BPValue, desc, AmbClin)
Check<-distinct(Check)

#Check these descriptions

GoldLong<-BPValue%>%
  rename(Systolic=data1, Diastolic=data2)%>%
  pivot_longer(13:14, names_to = "Label", values_to = "value")%>%
  mutate(value = as.numeric(value))%>%
  subset(value>0)%>%
  select(patid, medcode, readcode, desc, Type, Hypertension, eventdate, value, Label, patdate, AmbClin)

rm(BPGoldValue, opr, sum, tqu, TQUVals, TQUVals2, GoldValue, Ent, EntCheck, Check, entity, PatBPGold1)
rm(Additional, BPAdd, Date, Date2, EntCheckAdd, GoldMed, PatBPCheck, PatBPGold)
rm(TermCheck)

####All values####
BPAurumLong<-select(BPAurumLong, patid, medcode, readcode, desc, Type, Hypertension, eventdate, value, Label, patdate, AmbClin)
BP<-rbind(GoldLong, BPAurumLong)
BP<-distinct(BP)
BP$patdatelabel<-paste0(BP$patid, BP$eventdate, BP$Label, BP$AmbClin)
length(unique(BP$patdatelabel))

#Drop medcode  and QOF as likely the only difference for some
BP<-select(BP, -medcode, -readcode, -desc, -Type, -Hypertension)
BP<-distinct(BP)
length(unique(BP$patdatelabel))

#Find multiple on the same day
Multi<-BP%>%
  group_by(patdatelabel)%>%
  mutate(SameDay=n())%>%
  subset(SameDay>1)
length(unique(Multi$patdatelabel))

Single<-BP%>%
  group_by(patdatelabel)%>%
  mutate(SameDay=n())%>%
  subset(SameDay==1)
length(unique(Single$patdatelabel))

#Drop if out of range and there is one in range
Multi<-Multi%>%
  group_by(patdatelabel)%>%
  mutate(value = as.numeric(value))%>%
  mutate(maxval=max(value), minval=min(value), ave=mean(value), diff=maxval-minval)%>%
  ungroup%>%
  mutate(InRange = case_when(Label=="Systolic" & value<=269.5 & value>=52.2 ~ 1,
                             Label=="Diastolic" & value<=167.2 & value>=27.9 ~ 1,
                             Label=="Mean" & value<=269.5 & value>=27.9 ~ 1,
                             TRUE ~ 0))%>%
  group_by(patdatelabel)%>%
  mutate(EverInRange = sum(InRange))%>%
  subset(InRange==1 | (InRange==0&EverInRange==0))%>%
  mutate(maxval=max(value), minval=min(value), ave=mean(value), diff=maxval-minval)

length(which(Multi$diff==0))
length(which(Multi$diff<=10))

NewSingle<-Multi %>%
  subset(diff<=10)%>%
  group_by(patdatelabel) %>%
  mutate(value=ave) %>%
  filter(row_number()==1)%>%
  select(-maxval, -minval, -ave, -diff)
length(unique(NewSingle$patdatelabel))

Single<-rbind(Single, NewSingle)

LargeDiff<-subset(Multi, diff>10)

NewSingle<-LargeDiff %>%
  group_by(patdatelabel) %>%
  mutate(value=ave) %>%
  filter(row_number()==1)%>%
  select(-maxval, -minval, -ave, -diff)
length(unique(NewSingle$patdatelabel))

Single<-rbind(Single, NewSingle)

length(unique(Single$patdatelabel))

NewBP<-ungroup(Single)
NewBP<-select(NewBP, -InRange, -EverInRange, -SameDay, -patdatelabel)

table(NewBP$Label)

FinalBP<-NewBP%>%
  group_by(patdate)%>%
  pivot_wider(names_from = Label, values_from = c(value))
length(unique(FinalBP$patdate))

#Rename
FinalBP$eventyear<-year(FinalBP$eventdate)

#Limit to valid values
table(FinalBP$Mean)
length(which(is.na(FinalBP$Systolic) & is.na(FinalBP$Diastolic)))

FinalBP$Systolic[FinalBP$Systolic<52.2|FinalBP$Systolic>269.5]<-NA
FinalBP$Diastolic[FinalBP$Diastolic<27.9|FinalBP$Diastolic>167.2]<-NA
FinalBP$Mean[FinalBP$Mean<27.9|FinalBP$Mean>269.5]<-NA

FinalBP$Drop<-0
FinalBP$Drop[is.na(FinalBP$Diastolic) & is.na(FinalBP$Systolic) & is.na(FinalBP$Mean)]<-1
table(FinalBP$Drop)

FinalBP<-FinalBP%>%
  ungroup()

FinalBP<-subset(FinalBP, Drop==0)
FinalBP<-select(FinalBP, -Drop)
save(FinalBP, file=paste0(ProjectDir, "/Extracts/BPValues.Rdata"))

####Find categories####
load(paste0(ProjectDir, "/Extracts/BPGoldClinical.Rdata"))
PatClin<-subset(PatClin, patid %in% Trial$patid)
BPCatGold<-select(PatClin, medcode, readcode, desc, Type, Hypertension, patid, eventdate)
BPCatGold2<-select(PatTest, medcode, readcode, desc, Type, Hypertension, patid, eventdate)

BPCatGold<-rbind(BPCatGold, BPCatGold2)

table(BPCatGold$desc)
table(BPCatGold$Hypertension)
BPCatGold<-subset(BPCatGold, Hypertension!="no")

BPCatGold$patdate<-paste0(BPCatGold$patid, BPCatGold$eventdate)
BPCatGold$patdatelabel<-paste0(BPCatGold$patid,BPCatGold$eventdate, BPCatGold$Hypertension)

#Seperate out history, pregnancy and other for later
GoldHistory<-subset(BPCatGold, Type=="history")
GoldPregnancy<-subset(BPCatGold, Type=="pregnancy")
GoldOther<-subset(BPCatGold, Type!="pregnancy" & Type!="history")

BPCatGold<-select(BPCatGold, patid, patdate, patdatelabel, eventdate, Hypertension)
length(unique(BPCatGold$patdatelabel))

#All are unique!
BPCatGold<-distinct(BPCatGold)

length(unique(BPCatGold$patdate))

#Sort out those with different labels
BPCatGold<-BPCatGold%>%
  select(-patdatelabel)%>%
  group_by(patdate)%>%
  distinct()%>%
  mutate(SameDay = n())

CatGoldSingle<-subset(BPCatGold, SameDay==1)

CatGoldMulti<-subset(BPCatGold, SameDay>1)

CatGoldMulti<-CatGoldMulti%>%
  group_by(patdate)%>%
  pivot_wider(names_from=Hypertension, values_from = SameDay, values_fill = 0)

CatGoldMulti<-CatGoldMulti%>%
  group_by(patdate)%>%
  mutate(Category = case_when(hypertension>0 ~ "hypertension",
                              `high bp`>0 ~ "high bp",
                              `normal bp`>0 ~ "normal bp",
                              `postural drop`>0 ~ "low bp",
                              `abnormal bp`>0 ~ "abnormal bp",
                              `normal bp`>0 ~ "normal bp",
                           TRUE ~ "Check"))

CatGoldMulti<-select(CatGoldMulti, patid, patdate,eventdate, Category)
CatGoldMulti<-distinct(CatGoldMulti)
length(unique(CatGoldMulti$patdate))

CatGoldSingle<-select(CatGoldSingle, patid, patdate,eventdate, Category=Hypertension)
CatGoldSingle$Category[CatGoldSingle$Category=="postural drop"]<-"low bp"
CatGoldSingle$Category[CatGoldSingle$Category=="suspected hypertension"]<-"hypertension"

CatGold<-rbind(CatGoldSingle, CatGoldMulti)

#Sort out those that are only historical - come back to these later...
length(which(CatGold$patid %in% GoldHistory$patid & !CatGold$patid %in% GoldOther$patid))
length(which(CatGold$patid %in% GoldPregnancy$patid & !CatGold$patid %in% GoldOther$patid))

####Aurum categories####
load(paste0(ProjectDir, "/Extracts/BPAurum.Rdata"))
PatBPAurum<-subset(PatObsAurum, patid %in% Trial$patid)
table(PatBPAurum$desc)
table(PatBPAurum$Hypertension)
BPCatAurum<-subset(PatBPAurum, Hypertension!="no")

BPCatAurum$patdate<-paste0(BPCatAurum$patid, BPCatAurum$eventdate)
BPCatAurum$patdatelabel<-paste0(BPCatAurum$patid,BPCatAurum$eventdate, BPCatAurum$Hypertension)

#Seperate out history, pregnancy and other for later
AurumHistory<-subset(BPCatAurum, Type=="history")
AurumPregnancy<-subset(BPCatAurum, Type=="pregnancy")
AurumOther<-subset(BPCatAurum, Type!="pregnancy" & Type!="history")

BPCatAurum<-select(BPCatAurum, patid, patdate, patdatelabel, eventdate, Hypertension)
length(unique(BPCatAurum$patdatelabel))

#All are unique!
BPCatAurum<-distinct(BPCatAurum)

length(unique(BPCatAurum$patdate))

#Sort out those with different labels
BPCatAurum<-BPCatAurum%>%
  select(-patdatelabel)%>%
  group_by(patdate)%>%
  distinct()%>%
  mutate(SameDay = n())

CatAurumSingle<-subset(BPCatAurum, SameDay==1)

CatAurumMulti<-subset(BPCatAurum, SameDay>1)

CatAurumMulti<-CatAurumMulti%>%
  group_by(patdate)%>%
  pivot_wider(names_from=Hypertension, values_from = SameDay, values_fill = 0)

CatAurumMulti<-CatAurumMulti%>%
  group_by(patdate)%>%
  mutate(Category = case_when(hypertension>0 ~ "hypertension",
                              `high bp`>0 ~ "high bp",
                              `suspected hypertension`>0 ~ "hypertension",
                              `normal bp`>0 ~ "normal bp",
                              `low bp`>0 ~ "low bp",
                              `postural drop`>0 ~ "low bp",
                              `abnormal bp`>0 ~ "abnormal bp",
                              `normal bp`>0 ~ "normal bp",
                              TRUE ~ "Check"))

CatAurumMulti<-select(CatAurumMulti, patid, patdate,eventdate, Category)
CatAurumMulti<-distinct(CatAurumMulti)
length(unique(CatAurumMulti$patdate))

CatAurumSingle<-select(CatAurumSingle, patid, patdate,eventdate, Category=Hypertension)
CatAurumSingle$Category[CatAurumSingle$Category=="postural drop"]<-"low bp"
CatAurumSingle$Category[CatAurumSingle$Category=="suspected hypertension"]<-"hypertension"

CatAurum<-rbind(CatAurumSingle, CatAurumMulti)

#Sort out those that are only historical - come back to these later...
length(which(CatAurum$patid %in% AurumHistory$patid & !CatAurum$patid %in% AurumOther$patid))
length(which(CatAurum$patid %in% AurumPregnancy$patid & !CatAurum$patid %in% AurumOther$patid))

#All BP category data
AllBPCat<-rbind(CatAurum, CatGold)

#Merge and save historical and preg
AurumHistory<-select(AurumHistory, medcode, readcode, desc, Type, Hypertension, patid, eventdate, patdate, patdatelabel)
HistBP<-rbind(GoldHistory, AurumHistory)

AurumPregnancy<-select(AurumPregnancy, medcode, readcode, desc, Type, Hypertension, patid, eventdate, patdate, patdatelabel)
PregBP<-rbind(GoldPregnancy, AurumPregnancy)

AurumOther<-select(AurumOther, medcode, readcode, desc, Type, Hypertension, patid, eventdate, patdate, patdatelabel)
OtherBP<-rbind(GoldOther, AurumOther)

save(HistBP, file = paste0(ProjectDir, "/Extracts/HistoricBP.Rdata"))
save(PregBP, file = paste0(ProjectDir, "/Extracts/PregBP.Rdata" ))
save(OtherBP, file = paste0(ProjectDir, "/Extracts/OtherBP.Rdata" ))

#load BP value data
save(AllBPCat, file = paste0(ProjectDir, "/Extracts/BPCat.Rdata"))
