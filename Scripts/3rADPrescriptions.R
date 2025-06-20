####Check which Antidepressants the six months before start####

#Call start script
source("./Scripts/1aSetUp.R")

####Load data####
load(paste0(ProjectDir, "/Data/Trial_CCB.rdata"))
load(paste0(ProjectDir, "/StatinExtracts/ADMedAurum.Rdata"))
load(paste0(ProjectDir, "/StatinExtracts/GoldADMed.Rdata"))
load(paste0(ProjectDir, "/StatinExtracts/GoldADProd.Rdata"))
load(paste0(ProjectDir, "/StatinExtracts/AurumADProd.Rdata"))

AurumAD$patid<-paste0(AurumAD$patid, "-A")
AurumAD<-select(AurumAD, patid, eventdate, AD)
GoldAD$patid<-paste0(GoldAD$patid, "-G")
GoldAD<-select(GoldAD, patid, eventdate, AD)

AllAD<-rbind(AurumAD, GoldAD)
AllAD<-subset(AllAD, patid %in% Trial$patid)

rm(AurumAD, GoldAD)

length(which(Trial$patid %in% AllAD$patid))

Date<-select(Trial, patid, FirstCCBARB)
AllAD<-merge(x=AllAD, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

####Label up####
AllAD<-AllAD%>%
  mutate(Type=case_when(AD=="citalopram" | AD=="dapoxetine"|AD=="escitalopram"|AD=="fluoxetine"|
                          AD=="fluvoxamine"|AD=="paroxetine"|AD=="sertraline"|AD=="vortioxetine" ~ "SSRI",
         AD=="amitriptyline" | AD=="clomipramine"|AD=="dosulepin"|AD=="imipramine"|
                          AD=="lofepramine"|AD=="nortriptyline"|AD=="doxepin" |AD=="maprotiline"|AD=="mianserin"|
          AD=="mirtazapine" |AD=="trimipramine"~ "TCA",
         TRUE ~ "Other"))

table(AllAD$Type, AllAD$AD)
table(AllAD$Type)

#6 months before CCBs
AD<-AllAD%>%
  subset(eventdate<=FirstCCBARB)%>%
  subset(FirstCCBARB-eventdate<=182.625)

length(unique(AD$patid))/length(which(!is.na(Trial$FirstCCBARB)))

SSRI<-subset(AD, Type=="SSRI")
TCA<-subset(AD, Type=="TCA")
Other<-subset(AD, Type=="Other")

Trial$SSRI<-0
Trial$SSRI[Trial$patid %in% SSRI$patid]<-1

Trial$TCA<-0
Trial$TCA[Trial$patid %in% TCA$patid]<-1

Trial$Other<-0
Trial$Other[Trial$patid %in% Other$patid]<-1

table(Trial$SSRI)
table(Trial$TCA)
table(Trial$Other)

save(Trial, file = paste0(ProjectDir, "/Data/Trial_AD.rdata"))

