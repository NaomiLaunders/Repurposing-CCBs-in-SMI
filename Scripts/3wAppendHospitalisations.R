####Append outcomes####

#Call start script
source("./Scripts/1aSetUp.R")

load(paste0(ProjectDir, "/Data/SH.rdata"))

####Spells####
load(paste0(ProjectDir, "/Data/HospDiagnoses.rdata"))

####Limit to our population####
AllDiag<-subset(AllDiag, patid %in% Trial$patid)

#Split into all, first ep and first ep primary diag
FirstEpDiag<-subset(AllDiag, eorder==1)

PrimaryDiag<-subset(FirstEpDiag, d_order==1)

#Check primary diagnosis:
table(PrimaryDiag$ICD)

####Code up first episode primary diagnoses####

#Primary diagnosis of first episode
FirstEpPrimary<-PrimaryDiag%>%
  mutate(Diag = case_when(startsWith(ICD, "Z50.2")|startsWith(ICD, "Z50.3") ~ "DROP",
                          startsWith(ICD, "F") ~ "MH",
                          startsWith(ICD, "R41")|startsWith(ICD, "R44")|startsWith(ICD, "R45")|startsWith(ICD, "R46")|startsWith(ICD, "Z00.4")|startsWith(ICD, "Z03.2")|startsWith(ICD, "Z13.3")|startsWith(ICD, "Z50.4")|startsWith(ICD,"Z73") ~ "PrimaryPossMH",
                          startsWith(ICD, "Y1")|startsWith(ICD, "Y2")|startsWith(ICD, "Y3")|startsWith(ICD, "X6")|startsWith(ICD, "X7")|startsWith(ICD, "X8")|startsWith(ICD, "Y87.0") ~ "Self harm",
                          startsWith(ICD, "A")|startsWith(ICD, "B")|startsWith(ICD, "C")|startsWith(ICD, "D0")|startsWith(ICD, "D1")|startsWith(ICD, "D2")|startsWith(ICD, "D3")|startsWith(ICD, "D4")|startsWith(ICD, "D5")|startsWith(ICD, "D6")|startsWith(ICD, "D7")|startsWith(ICD, "D8")|startsWith(ICD, "E")|startsWith(ICD, "G")|startsWith(ICD, "H0")|startsWith(ICD, "H1")|startsWith(ICD, "H2")|startsWith(ICD, "H3")|startsWith(ICD, "H4")|startsWith(ICD, "H5")|startsWith(ICD, "H6")|startsWith(ICD, "H7")|startsWith(ICD, "H8")|startsWith(ICD, "H9")|startsWith(ICD, "I")|startsWith(ICD, "J")|startsWith(ICD, "K")|startsWith(ICD, "L")|startsWith(ICD, "M")|startsWith(ICD, "N")|startsWith(ICD, "R0")|startsWith(ICD, "R1")|startsWith(ICD, "R20")|startsWith(ICD, "R21")|startsWith(ICD, "R22")|startsWith(ICD, "R23")|startsWith(ICD, "R25")|startsWith(ICD, "R26")|startsWith(ICD, "R27")|startsWith(ICD, "R28")|startsWith(ICD, "R29")|startsWith(ICD, "R3") |startsWith(ICD, "R50")|startsWith(ICD, "R51")|startsWith(ICD, "R52")|startsWith(ICD, "R53")|startsWith(ICD, "R55")|startsWith(ICD, "R56")|startsWith(ICD, "R57")|startsWith(ICD, "R59")|startsWith(ICD, "R64")|startsWith(ICD, "R65")|startsWith(ICD, "R68.0")|startsWith(ICD, "R70")|startsWith(ICD, "R71")|startsWith(ICD, "R72")|startsWith(ICD, "R73")|startsWith(ICD, "R74")|startsWith(ICD, "R75")|startsWith(ICD, "R76")|startsWith(ICD, "R77")|startsWith(ICD, "R79")|startsWith(ICD, "R80")|startsWith(ICD, "R81")|startsWith(ICD, "R82.0")|startsWith(ICD, "R82.1")|startsWith(ICD, "R82.2")|startsWith(ICD, "R82.3")|startsWith(ICD, "R82.4")|startsWith(ICD, "R82.6")|startsWith(ICD, "R82.7")|startsWith(ICD, "R82.8")|startsWith(ICD, "R82.9")|startsWith(ICD, "R83")|startsWith(ICD, "R84")|startsWith(ICD, "R85")|startsWith(ICD, "R86")|startsWith(ICD, "R87")|startsWith(ICD, "R88")|startsWith(ICD, "R89")|startsWith(ICD, "R90")|startsWith(ICD, "R91")|startsWith(ICD, "R92")|startsWith(ICD, "R93")|startsWith(ICD, "R94")|startsWith(ICD, "U0")|startsWith(ICD, "U1")|startsWith(ICD, "U2")|startsWith(ICD, "U3")|startsWith(ICD, "U4")|startsWith(ICD, "U8")|startsWith(ICD, "Z00.0")|startsWith(ICD, "Z00.1")|startsWith(ICD, "Z00.2")|startsWith(ICD, "Z00.3")|startsWith(ICD, "Z00.6")|startsWith(ICD, "Z00.7")|startsWith(ICD, "Z00.8")|startsWith(ICD, "Z01")|startsWith(ICD, "Z02")|startsWith(ICD, "Z03.0")|startsWith(ICD, "Z03.1")|startsWith(ICD, "Z03.3")|startsWith(ICD, "Z03.4")|startsWith(ICD, "Z03.5")|startsWith(ICD, "Z03.8")|startsWith(ICD, "Z03.9")|startsWith(ICD, "Z08")|startsWith(ICD, "Z09.1")|startsWith(ICD, "Z09.2") |startsWith(ICD, "Z10")|startsWith(ICD, "Z11")|startsWith(ICD, "Z12")|startsWith(ICD, "Z13.0")|startsWith(ICD, "Z13.1")|startsWith(ICD, "Z13.2")|startsWith(ICD, "Z13.5")|startsWith(ICD, "Z13.6")|startsWith(ICD, "Z13.8")|startsWith(ICD, "Z13.9")|startsWith(ICD, "Z40")|startsWith(ICD, "Z45")|startsWith(ICD, "Z49")|startsWith(ICD, "Z50")|startsWith(ICD, "Z51.0")|startsWith(ICD, "Z51.1")|startsWith(ICD, "Z51.2")|startsWith(ICD, "Z51.6")|startsWith(ICD, "Z54.1")|startsWith(ICD, "Z54.2")|startsWith(ICD, "Z71.3")|startsWith(ICD, "Z71.7")|startsWith(ICD, "Z80")| startsWith(ICD, "Z85")|startsWith(ICD, "Z86.0")|startsWith(ICD, "Z86.1")|startsWith(ICD, "Z86.2")|startsWith(ICD, "Z86.3")|startsWith(ICD, "Z86.6")|startsWith(ICD, "Z86.7")|startsWith(ICD, "Z87.0")|startsWith(ICD, "Z87.1")|startsWith(ICD, "Z87.2")|startsWith(ICD, "Z87.3")|startsWith(ICD, "Z87.4")|startsWith(ICD, "Z87.6")|startsWith(ICD, "Z87.7")|startsWith(ICD, "Z87.8")|startsWith(ICD, "Z88")|startsWith(ICD, "Z91.0")|startsWith(ICD, "Z94")|startsWith(ICD, "Z95")|startsWith(ICD, "Z2") ~ "Physical",  
                          startsWith(ICD, "R78")|startsWith(ICD, "S")|startsWith(ICD, "T0")|startsWith(ICD, "T10")|startsWith(ICD, "T11")| startsWith(ICD, "T12")|startsWith(ICD, "T13")|startsWith(ICD, "T14")|startsWith(ICD, "T15")|startsWith(ICD, "T16")|startsWith(ICD, "T17")|startsWith(ICD, "T18")|startsWith(ICD, "T19")|startsWith(ICD, "T2")|startsWith(ICD, "T30")|startsWith(ICD, "T31")|startsWith(ICD, "T32")|startsWith(ICD, "T33")|startsWith(ICD, "T34")|startsWith(ICD, "T35")|startsWith(ICD, "T36")|startsWith(ICD, "T37")|startsWith(ICD, "T38")|startsWith(ICD, "T39")|startsWith(ICD, "T4")|startsWith(ICD, "T5")|startsWith(ICD, "T60")|startsWith(ICD, "T61")|startsWith(ICD, "T62")|startsWith(ICD, "T63")|startsWith(ICD, "T64")|startsWith(ICD, "T65")|startsWith(ICD, "T66")|startsWith(ICD, "T67")|startsWith(ICD, "T68")|startsWith(ICD, "T69")|startsWith(ICD, "T70")|startsWith(ICD, "T71")|startsWith(ICD, "T72")|startsWith(ICD, "T73")|startsWith(ICD, "T74")|startsWith(ICD, "T75")|startsWith(ICD, "T76")|startsWith(ICD, "T77")|startsWith(ICD, "T78")|startsWith(ICD, "T79")|startsWith(ICD, "T9")|startsWith(ICD, "V")|startsWith(ICD, "W")|startsWith(ICD, "X0")|startsWith(ICD, "X1")|startsWith(ICD, "X2")|startsWith(ICD, "X3")|startsWith(ICD, "X4")|startsWith(ICD, "X5")|startsWith(ICD, "X6")|startsWith(ICD, "X7")|startsWith(ICD, "X80")|startsWith(ICD, "X81")|startsWith(ICD, "X82")|startsWith(ICD, "X83")|startsWith(ICD, "X84")|startsWith(ICD, "X85")|startsWith(ICD, "X86")|startsWith(ICD, "X87")|startsWith(ICD, "X88")|startsWith(ICD, "X89")|startsWith(ICD, "X9")|startsWith(ICD, "Y0")|startsWith(ICD, "Y1")|startsWith(ICD, "Y2")|startsWith(ICD, "Y30")|startsWith(ICD, "Y31")|startsWith(ICD, "Y32")|startsWith(ICD, "Y33")|startsWith(ICD, "Y34")|startsWith(ICD, "Y35")|startsWith(ICD, "Y36")|startsWith(ICD, "Y85")|startsWith(ICD, "Y86")|startsWith(ICD, "Y87")|startsWith(ICD, "Y89")|startsWith(ICD, "Y90")|startsWith(ICD, "Y91")|startsWith(ICD, "Z03.6")|startsWith(ICD, "Z04.0")|startsWith(ICD, "Z04.1")|startsWith(ICD, "Z04.2")|startsWith(ICD, "Z04.3")|startsWith(ICD, "Z04.4")|startsWith(ICD, "Z04.5")|startsWith(ICD, "Z04.6")|startsWith(ICD, "Z04.7")|startsWith(ICD, "Z04.8")|startsWith(ICD, "Z04.9")|startsWith(ICD, "Z09.4")|startsWith(ICD, "Z50.2")|startsWith(ICD, "Z50.3")|startsWith(ICD, "Z54.4")|startsWith(ICD, "Z71.4")|startsWith(ICD, "Z71.5")|startsWith(ICD, "Z71.6")|startsWith(ICD, "Z72")|startsWith(ICD, "Z86.4")|startsWith(ICD, "Z91.5")|startsWith(ICD, "Z91.6") ~"Accident",
    TRUE ~ "DROP"))

FirstEpPrimary<-subset(FirstEpPrimary, Diag!="DROP")

####Code up First episode any diagnoses####
#Any MH
AnyMH<-FirstEpDiag%>%
  mutate(AnyMH = case_when(startsWith(ICD, "F") ~ 1,
                           TRUE ~ 0))%>%
  subset(AnyMH == 1)

#Any SH
AnySH<-FirstEpDiag%>%
  mutate(AnySH = case_when(startsWith(ICD, "Y1")|startsWith(ICD, "Y2")|startsWith(ICD, "Y3")|startsWith(ICD, "X6")|startsWith(ICD, "X7")|startsWith(ICD, "X8")|startsWith(ICD, "Y87.0") ~ 1,
                           TRUE ~ 0))%>%
  subset(AnySH == 1)

AnyMH<-subset(FirstEpDiag, AnyMH==1) #113859
AnySelfHarm<-subset(FirstEpDiag, AnySH==1) #7309

#Limit the possible MH to those who have a mental health diagnosis at some point that episode
FinalDiag<-FirstEpPrimary%>%
  mutate(Diag = case_when(EpiPat %in% AnyMH$EpiPat & Diag == "PrimaryPossMH" ~ "MH",
                          EpiPat %in% AnySH$EpiPat & Diag == "Accident" ~ "Self harm",
                          TRUE ~ Diag))

length(unique(FinalDiag$sppat[FinalDiag$Diag=="MH"]))
length(unique(FinalDiag$sppat[FinalDiag$Diag=="Self harm"]))

length(unique(FinalDiag$sppat))

#Look at planned vs. unplanned spells

#Only include physical or accidents if they are emergencies
FinalDiag<-FinalDiag%>%
  mutate(Diag = case_when(Emergency ==0 & Diag=="Physical" ~ "DROP",
                          Emergency == 0 & Diag == "Accident" ~ "DROP",
                          TRUE ~ Diag))%>%
  subset(Diag!="DROP")


Date<-select(Trial, patid, end)

FinalDiag<-merge(x=FinalDiag, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

#Only include if before end
FinalDiag<-subset(FinalDiag, admidate<=end)

####Merge back to the main file####
#Baseline
YearBefore<-FinalDiag%>%
  subset(admidate<=FirstCCBARB & FirstCCBARB-admidate<=365.25)%>%
  group_by(sppat)%>%
  select(sppat, patid, Diag)%>%
  mutate(Count = n())%>%
  pivot_wider(values_from = Count, names_from = Diag, values_fill = 0)%>%
  ungroup()%>%
  group_by(patid)%>%
  summarise_at(c("Physical", "Self harm", "MH", "Accident"), sum)%>%
  rename(PriorPhysical = Physical, PriorSH = `Self harm`, PriorMH= MH, PriorAccident=Accident)
    
Trial<-merge(x=Trial, y=YearBefore, by="patid", all.x=TRUE, all.y=TRUE)

####Outcomes####
# 3mo outcome
Out3<-FinalDiag%>%
  subset(admidate>FirstCCBARB & admidate-FirstCCBARB<=84)%>%
  group_by(sppat)%>%
  select(sppat, patid, Diag)%>%
  mutate(Count = n())%>%
  pivot_wider(values_from = Count, names_from = Diag, values_fill = 0)%>%
  ungroup()%>%
  select(-sppat)%>%
  group_by(patid)%>%
  summarise_at(c("Self harm", "MH"), sum)%>%
  rename(OutcomeSH3 = `Self harm`, OutcomeMH3= MH)

Trial<-merge(x=Trial, y=Out3, by="patid", all.x=TRUE, all.y=TRUE)

# 6mo
Out6<-FinalDiag%>%
  subset(admidate>FirstCCBARB & admidate-FirstCCBARB<=182.625)%>%
  group_by(sppat)%>%
  select(sppat, patid, Diag)%>%
  mutate(Count = n())%>%
  pivot_wider(values_from = Count, names_from = Diag, values_fill = 0)%>%
  ungroup()%>%
  select(-sppat)%>%
  group_by(patid)%>%
  summarise_at(c("Self harm", "MH"), sum)%>%
  rename(OutcomeSH6 = `Self harm`, OutcomeMH6= MH)

Trial<-merge(x=Trial, y=Out6, by="patid", all.x=TRUE, all.y=TRUE)

# 12mo

Out12<-FinalDiag%>%
  subset(admidate>FirstCCBARB & admidate-FirstCCBARB<=365.25)%>%
  group_by(sppat)%>%
  select(sppat, patid, Diag)%>%
  mutate(Count = n())%>%
  pivot_wider(values_from = Count, names_from = Diag, values_fill = 0)%>%
  ungroup()%>%
  select(-sppat)%>%
  group_by(patid)%>%
  summarise_at(c("Self harm", "MH"), sum)%>%
  rename(OutcomeSH12 = `Self harm`, OutcomeMH12= MH)

Trial<-merge(x=Trial, y=Out12, by="patid", all.x=TRUE, all.y=TRUE)

# 24mo

Out24<-FinalDiag%>%
  subset(admidate>FirstCCBARB & admidate-FirstCCBARB<=730.5)%>%
  group_by(sppat)%>%
  select(sppat, patid, Diag)%>%
  mutate(Count = n())%>% #SHould be 1 for everything - just for summing
  pivot_wider(values_from = Count, names_from = Diag, values_fill = 0)%>%
  ungroup()%>%
  select(-sppat)%>%
  group_by(patid)%>%
  summarise_at(c("Self harm", "MH"), sum)%>%
  rename(OutcomeSH24 = `Self harm`, OutcomeMH24= MH)

Trial<-merge(x=Trial, y=Out24, by="patid", all.x=TRUE, all.y=TRUE)

#Time to event
Time<-FinalDiag%>%
  subset(admidate>FirstCCBARB & admidate-FirstCCBARB<=730.5)%>%
  select(patid, Diag, admidate)%>%
  group_by(patid, Diag)%>%
  mutate(Date=min(admidate))%>%
  subset(Date == admidate)%>%
  select(-admidate)%>%
  distinct()%>%
  pivot_wider(values_from = Date, names_from = Diag)%>%
  ungroup()%>%
  rename(OutcomeSH24Date = `Self harm`, OutcomeMH24Date= MH)%>%
  select(-PrimaryPossMH, -Accident, -Physical)


Trial<-merge(x=Trial, y=Time, by="patid", all.x=TRUE, all.y=TRUE)

save(Trial, file=paste0(ProjectDir, "/Data/Trial_Complete.rdata"))

