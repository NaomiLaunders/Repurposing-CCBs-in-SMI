####Remove HES duplicates####

#Call start script
source("./Scripts/1aSetUp.R")

load(paste0(ProjectDir, "/Data/CleanCPRD.Rdata"))

#Load in linkage file

table(CPRD$hes_apc_e, useNA="ifany")
prop.table(table(CPRD$hes_apc_e, CPRD$source, useNA="ifany"),2)

####Eligible####
Hosp<-subset(CPRD, hes_apc_e==1)

#Load HES patient files
#Keep ethnicity the same, otherwise will improve only those with hospitalizations
HESAurum<-read.table(paste0(DataDir, "/Linkages/Results/Aurum_linked/Final/HES APC/hes_patient_21_000729.txt"), header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character", "gen_hesid" = "character"))

length(unique(HESAurum$patid))
HESAurum<-select(HESAurum, -gen_ethnicity, -match_rank, -pracid)

HESGold<-read.table(paste0(DataDir, "/Linkages/Results/Final Gold/HES APC/hes_patient_21_000729.txt"), header=TRUE, quote="", fill=TRUE, sep="\t", colClasses = c("patid"="character", "gen_hesid" = "character"))

length(unique(HESGold$patid))
HESGold<-select(HESGold, -gen_ethnicity, -match_rank, -pracid)

#Convert patid
HESAurum$patid<-paste0(HESAurum$patid, "-A")
HESGold$patid<-paste0(HESGold$patid, "-G")
#Combine as one 
HospRecord<-rbind(HESGold, HESAurum)

#Merge to CPRD
HospAll<-merge(x=Hosp, y=HospRecord, by="patid", all.x=TRUE, all.y=FALSE)
rm(Hosp)
#Patid is unique
length(unique(HospAll$patid))
#Most patients have a HES id
length(which(!is.na(HospAll$gen_hesid)))
#Some are duplicates!
length(unique(HospAll$gen_hesid))

#Identify patients which have multiple records in CPRD but only one HES record (i.e. are the same patient)
HospAll<-HospAll%>%
  group_by(gen_hesid)%>%
  mutate(patid_n=n())%>%
  ungroup()
table(HospAll$patid_n, useNA="ifany")

#Check duplicates
Dups<-subset(HospAll, patid_n>1&!(is.na(gen_hesid)))
table(Dups$patid_n, useNA="ifany")
Dups<-select(Dups, patid, gen_hesid, enter, end, source, FirstAntiHypDate)
length(unique(Dups$gen_hesid))

#Look at time between each CCB use
Dups<-Dups%>%
  group_by(gen_hesid)%>%
  arrange(gen_hesid, FirstAntiHypDate)%>%
  mutate(StatDiff=FirstAntiHypDate-lag(FirstAntiHypDate), n=n(), EnterDiff=enter-lag(enter), FU=end-enter)%>%
  mutate(MinStatDate=min(FirstAntiHypDate), MinEnter=min(enter))%>%
  mutate(TotalStatDiff=sum(as.numeric(StatDiff), na.rm=TRUE), MinStatDiff=min(as.numeric(StatDiff), na.rm=TRUE))%>%
  mutate(TotalEnterDiff=sum(as.numeric(EnterDiff), na.rm=TRUE), MinEnterDiff=min(as.numeric(EnterDiff), na.rm=TRUE), maxFU=max(as.numeric(FU)))%>%
  mutate(NumericSource=case_when(source=="Aurum" ~ 1, TRUE ~ 0))%>%
  ungroup()

#If start and first CCB are the same, then take the one with the longest follow up, otherwise take Aurum
Same<-subset(Dups, MinStatDiff==0 & (StatDiff==0 | is.na(StatDiff)) & MinEnterDiff==0 & (EnterDiff==0 | is.na(EnterDiff)))
Keep1<-Same%>%
  subset(maxFU==FU)%>% #Take the one with the longest follow up
  mutate(n=n(),TotalSource=sum(NumericSource))%>%
  subset(n==1 | (n>1 & TotalSource>0 & NumericSource==1) | (n>1 & TotalSource==0))%>% #Take those that are now unique, or those that are aurum, or if both gold keep both
  mutate(n=n())%>%
  subset(n==1 | (n>1 & is.na(StatDiff)))%>% #A few still have 2 records that are totally identical so just take the first
  ungroup()
length(unique(Keep1$gen_hesid))
length(unique(Keep1$patid))

#If first CCB are different then take the earliest
Diff<-subset(Dups, MinStatDiff!=0)

Keep2<-Diff%>%
  subset(MinStatDate==FirstAntiHypDate)
length(unique(Keep2$gen_hesid))
length(unique(Keep2$patid))

#Otherwise take max follow up
Check<-subset(Dups, !(gen_hesid %in% Keep1$gen_hesid | gen_hesid %in% Keep2$gen_hesid))
Keep3<-Check%>%
  subset(maxFU==FU)%>%
  group_by(gen_hesid)%>%
  mutate(n=n())%>%
  subset(n==1)%>%
  ungroup()
length(unique(Keep3$gen_hesid))
length(unique(Keep3$patid))

#Or identical, just take Aurum
Check2<-subset(Dups, !(gen_hesid %in% Keep1$gen_hesid | gen_hesid %in% Keep2$gen_hesid | gen_hesid %in% Keep3$gen_hesid ))
Keep4<-Check2%>%
  group_by(gen_hesid)%>%
  mutate(EverAurum=sum(source=="Aurum"))%>%
  subset((EverAurum>0 & source=="Aurum")|EverAurum==0)%>%
  mutate(n=n())%>%
  subset(n==1)%>%
  ungroup()
length(unique(Keep4$gen_hesid))
length(unique(Keep4$patid))

#Any left
Check3<-subset(Dups, !(gen_hesid %in% Keep1$gen_hesid | gen_hesid %in% Keep2$gen_hesid | gen_hesid %in% Keep3$gen_hesid| gen_hesid %in% Keep4$gen_hesid ))

#Drop them manually
Keep5<-subset(Dups, patid=="1195590920665-A" | patid=="2052773720879-A"|patid=="1845355820450-A"|patid=="1845278820450-A")

#Check Drop and Keep add up to Dups. They do!
Drop<-subset(Dups, !(patid %in% Keep1$patid | patid %in% Keep2$patid|patid %in% Keep3$patid|patid %in% Keep4$patid|patid %in% Keep5$patid))
Keep<-subset(Dups, !patid %in% Drop$patid)

HospAll<-subset(HospAll, !(HospAll$patid %in% Drop$patid))

#Just double check there are no duplicates now
HospAll<-HospAll%>%
  group_by(gen_hesid)%>%
  mutate(n=n())

table(HospAll$n)
length(unique(HospAll$patid))
length(which(!is.na(HospAll$gen_hesid)))
length(unique(HospAll$gen_hesid))
length(which(HospAll$patid %in% Keep1$patid))
length(which(HospAll$patid %in% Keep2$patid))

HospAll<-ungroup(HospAll)
save(HospAll, file=paste0(ProjectDir, "/Data/Hospselect.rdata"))
