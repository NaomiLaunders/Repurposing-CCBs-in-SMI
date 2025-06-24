####Raynauds code list####

#Call start script
source("./Scripts/1aSetUp.R")

#Look ups
AurumMed<-ReadAurumMedCodelist(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt"))
GoldMed<-ReadGoldMedCodelist(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGOLD/medical.txt"))
                    
AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)
GoldMed$desc<-tolower(GoldMed$desc)

####Check for new Ray codes Aurum####

#Checked codes for blood pressure or Ray but none give definite diagnosis
RayAurum<-AurumMed%>%
  mutate(Ray = case_when(grepl("raynaud", Term, ignore.case=TRUE) ~1,
                        TRUE ~ 0))%>%
  subset(Ray==1)

RayAurum<-select(RayAurum, MedCodeId, CleansedReadCode, Term)

#Check for others
RayAurumExtra<-AurumMed%>%
  subset(!(MedCodeId %in% RayAurum$MedCodeId))%>%
  mutate(Ray = case_when(grepl("G730", CleansedReadCode, ignore.case=TRUE) ~1,
                         grepl("G730", OriginalReadCode, ignore.case=TRUE) ~1,
                         TRUE ~ 0))%>%
  subset(Ray==1)

RayAurumExtra<-select(RayAurumExtra, MedCodeId, CleansedReadCode, Term)
RayAurum<-rbind(RayAurum, RayAurumExtra)

####Check for new Ray codes Gold####
RayGold<-GoldMed%>%
  mutate(Ray = case_when(grepl("raynaud", desc, ignore.case=TRUE) ~1,
                         TRUE ~ 0))%>%
  subset(Ray==1)

RayGold<-select(RayGold, medcode, readcode, desc)

#Check for others
RayGoldExtra<-GoldMed%>%
  subset(!(medcode %in% RayGold$medcode))%>%
  mutate(Ray = case_when(grepl("Z6G", readcode, ignore.case=TRUE) ~0,
    grepl("G730", readcode, ignore.case=TRUE) ~1,
                         TRUE ~ 0))%>%
  subset(Ray==1)

RayGoldExtra<-select(RayGoldExtra, medcode, readcode, desc)
RayGold<-rbind(RayGold, RayGoldExtra)

####Compare Gold and Aurum####
GoldRayCheck<-subset(RayGold, !(desc %in% RayAurum$Term)& desc!="")
AddtoAurumT<-subset(AurumMed, Term %in% GoldRayCheck$desc) #none to add

AurumRayCheck<-subset(RayAurum, !(Term %in% RayGold$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumRayCheck$Term) #None to add on term

GoldRayCheck<-subset(RayGold, !(readcode %in% RayAurum$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldRayCheck$readcode) #None to add

AurumRayCheck<-subset(RayAurum, !(CleansedReadCode %in% RayGold$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumRayCheck$CleansedReadCode) #4 to add on readcode but all there

####Save final lists####
WriteCodeList(RayAurum, "Codelists/RayAurum.txt")
WriteCodeList(RayGold, "Codelists/RayGold.txt")
