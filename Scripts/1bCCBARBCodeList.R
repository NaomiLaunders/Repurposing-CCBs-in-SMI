####CCB and ARB code list####

#Call start script
source("./Scripts/1aSetUp.R")

####Load product dictionaries####
AurumProd<-ReadAurumProdCodelist(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/202205_EMISProductDictionary.txt"))
GoldProd<-ReadGoldProdCodelist(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGOLD/product.txt"))

####CCB search#####
AurumCCB<-AurumProd%>%
  mutate(Group = case_when((grepl("Hydrochlorothiazide|Triapin|Indapamide|\\/", DrugSubstanceName, ignore.case=TRUE)|grepl("Hydrochlorothiazide|Triapin|Indapamide", Term.from.EMIS, ignore.case=TRUE))& grepl("ipine|verapamil|sartan|pril", DrugSubstanceName, ignore.case=TRUE) ~"Combo",
                           grepl("diltiazem|Nimodipine|adizem|Calcicard|Dilcardia", DrugSubstanceName, ignore.case=TRUE)|grepl("Dilcardia|diltiazem|Nimodipine|adizem|Calcicard", ProductName, ignore.case=TRUE)|grepl("diltiazem|Nimodipine|adizem|Calcicard", Term.from.EMIS, ignore.case=TRUE)~ "6 month exclusion",
                           grepl("caprilon|Prilocaine|pipril|Sprilon|bandage|Priligy", DrugSubstanceName, ignore.case=TRUE)|grepl("caprilon|Prilocaine|pipril|Sprilon|bandage|Priligy",Term.from.EMIS, ignore.case=TRUE) ~ "Not",
                           grepl("pril", DrugSubstanceName, ignore.case=TRUE)|grepl("pril", ProductName, ignore.case=TRUE)|grepl("pril", Term.from.EMIS, ignore.case=TRUE)~ "ever exclusion",
                           grepl("unipine|calcipine", Term.from.EMIS, ignore.case=TRUE) ~ "Check",
                            grepl("ipine|verapamil|sartan", ProductName, ignore.case=TRUE) |
                              grepl("ipine|verapamil|sartan", DrugSubstanceName, ignore.case=TRUE) | 
                              grepl("ipine|verapamil|sartan", Term.from.EMIS, ignore.case=TRUE) ~ "Mono",
                           BNFChapter=="2050501"|BNFChapter=="2050502"|BNFChapter=="2060200"~ "Check",
                            TRUE ~ "Not"))%>%
  subset(Group!="Not")

table(AurumCCB$DrugSubstanceName[AurumCCB$Group =="Mono"], useNA="ifany")
table(AurumCCB$DrugSubstanceName[AurumCCB$Group =="Combo"], useNA="ifany")
table(AurumCCB$DrugSubstanceName[AurumCCB$Group =="6 month exclusion"], useNA="ifany")
table(AurumCCB$Term.from.EMIS[AurumCCB$Group =="6 month exclusion"&AurumCCB$DrugSubstanceName==""], useNA="ifany")

table(AurumCCB$DrugSubstanceName[AurumCCB$Group =="ever exclusion"], useNA="ifany")
table(AurumCCB$Term.from.EMIS[AurumCCB$Group =="ever exclusion"&AurumCCB$DrugSubstanceName==""], useNA="ifany")

table(AurumCCB$BNFChapter, useNA="ifany")
table(AurumCCB$Term.from.EMIS[AurumCCB$BNFChapter ==2040000], useNA="ifany") #All nifedipine plus atenolol
AurumCCB$Group[AurumCCB$BNFChapter ==2040000]<-"Combo"
table(AurumCCB$Term.from.EMIS[AurumCCB$Group =="Check"])
AurumCCB$Group[grepl("Tritace", AurumCCB$Term.from.EMIS, ignore.case=TRUE)]<-"ever exclusion"
AurumCCB$Group[AurumCCB$Group =="Check"]<-"Mono"

#Find any other words that might be useful
AurumCCBLab1<-unique(as.list((word(AurumCCB$ProductName, 1)), 
                              (word(AurumCCB$Term.from.EMIS, 1)), 
                              (word(AurumCCB$DrugSubstanceName, 1))))

AurumCCBLab1<-subset(AurumCCBLab1, AurumCCBLab1!= "")
AurumCCB1<-subset(AurumProd, !(ProdCodeId %in% AurumCCB$ProdCodeId))
AurumCCB1<-subset(AurumCCB1, (grepl(paste(AurumCCBLab1, collapse='|'), ProductName, ignore.case=TRUE))|
                     (grepl(paste(AurumCCBLab1, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                     (grepl(paste(AurumCCBLab1, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

#Just check those without drug substance
AurumCCB1<-subset(AurumCCB1, DrugSubstanceName=="")
AurumCCB1<-AurumCCB1%>%
  mutate(Group = case_when(grepl("emla|lidocain|absorbable|Colistin|Fabahistin|limb|web|half|nimrodel|Otrivine|unilet|sterile|Vincristine|universal|Perindopril|eye|prilocain|Citanest", Term.from.EMIS, ignore.case=TRUE) ~ "Not",
                           grepl("optil|Dilcardia", Term.from.EMIS, ignore.case=TRUE) ~ "6 month exclusion",
                           grepl("triapin", Term.from.EMIS, ignore.case=TRUE) ~ "Combo",
                          TRUE ~ ""))%>%
  subset(Group!="Not")

AurumCCB<-rbind(AurumCCB, AurumCCB1)

#Label them with drug name and class

AurumCCB<-AurumCCB%>%
  mutate(Name = case_when(Group == "Combo" ~ "Combo",
                          Group == "6 month exclusion" ~ "6 month exclusion",
    grepl("Amlodipine", DrugSubstanceName, ignore.case=TRUE)|grepl("Amlodipine", Term.from.EMIS, ignore.case=TRUE) ~ "Amlodipine",
    grepl("Azilsartan", DrugSubstanceName, ignore.case=TRUE)|grepl("Azilsartan", Term.from.EMIS, ignore.case=TRUE)~ "Azilsartan",
    grepl("Candesartan", DrugSubstanceName, ignore.case=TRUE)|grepl("Candesartan", Term.from.EMIS, ignore.case=TRUE)~ "Candesartan",
    grepl("Eprosartan", DrugSubstanceName, ignore.case=TRUE)|grepl("Eprosartan", Term.from.EMIS, ignore.case=TRUE)~ "Eprosartan",
    grepl("Felodipine|Parmid", DrugSubstanceName, ignore.case=TRUE)|grepl("Felodipine|Parmid", Term.from.EMIS, ignore.case=TRUE)~ "Felodipine",
    grepl("Irbesartan", DrugSubstanceName, ignore.case=TRUE)|grepl("Irbesartan", Term.from.EMIS, ignore.case=TRUE)~ "Irbesartan",
    grepl("Isradipine", DrugSubstanceName, ignore.case=TRUE)|grepl("Isradipine", Term.from.EMIS, ignore.case=TRUE)~ "Isradipine",
    grepl("Lacidipine|calcipine", DrugSubstanceName, ignore.case=TRUE)|grepl("Lacidipine|calcipine", Term.from.EMIS, ignore.case=TRUE)~ "Lacidipine",
    grepl("Lercanidipine", DrugSubstanceName, ignore.case=TRUE)|grepl("Lercanidipine", Term.from.EMIS, ignore.case=TRUE)~ "Lercanidipine",
    grepl("Losartan", DrugSubstanceName, ignore.case=TRUE)|grepl("Losartan", Term.from.EMIS, ignore.case=TRUE)~ "Losartan",
    grepl("Manidipine", DrugSubstanceName, ignore.case=TRUE)|grepl("Manidipine", Term.from.EMIS, ignore.case=TRUE)~ "Manidipine",
    grepl("Nicardipine|cardene", DrugSubstanceName, ignore.case=TRUE)|grepl("Nicardipine|cardene", Term.from.EMIS, ignore.case=TRUE)~ "Nicardipine",
    grepl("Nifedipine|nimodrel|Unipine|Angiopine|Adipine", DrugSubstanceName, ignore.case=TRUE)|grepl("Nifedipine|nimodrel|Unipine|Angiopine|Adipine", Term.from.EMIS, ignore.case=TRUE)~ "Nifedipine",
    grepl("Nimodipine", DrugSubstanceName, ignore.case=TRUE)|grepl("Nimodipine", Term.from.EMIS, ignore.case=TRUE)~ "Nimodipine",
    grepl("Nisoldipine", DrugSubstanceName, ignore.case=TRUE)|grepl("Nisoldipine", Term.from.EMIS, ignore.case=TRUE)~ "Nisoldipine",
    grepl("Olmesartan", DrugSubstanceName, ignore.case=TRUE)|grepl("Olmesartan", Term.from.EMIS, ignore.case=TRUE)~ "Olmesartan",
    grepl("Telmisartan", DrugSubstanceName, ignore.case=TRUE)|grepl("Telmisartan", Term.from.EMIS, ignore.case=TRUE)~ "Telmisartan",
    grepl("Valsartan", DrugSubstanceName, ignore.case=TRUE)|grepl("Valsartan", Term.from.EMIS, ignore.case=TRUE)~ "Valsartan",
    grepl("Verapamil|Cordilox|Securon", DrugSubstanceName, ignore.case=TRUE)|grepl("Verapamil|Cordilox|Securon", Term.from.EMIS, ignore.case=TRUE)~ "Verapamil",
    grepl("Captopril", DrugSubstanceName, ignore.case=TRUE)|grepl("Captopril", Term.from.EMIS, ignore.case=TRUE)~ "Captopril",
    grepl("Cilazapril|Vascace", DrugSubstanceName, ignore.case=TRUE)|grepl("Cilazapril|Vascace", Term.from.EMIS, ignore.case=TRUE)~ "Cilazapril",
    grepl("Enalapril|Innovace", DrugSubstanceName, ignore.case=TRUE)|grepl("Enalapril|Innovace", Term.from.EMIS, ignore.case=TRUE)~ "Enalapril",
    grepl("Fosinopril", DrugSubstanceName, ignore.case=TRUE)|grepl("Fosinopril", Term.from.EMIS, ignore.case=TRUE)~ "Fosinopril",
    grepl("Lisinopril", DrugSubstanceName, ignore.case=TRUE)|grepl("Lisinopril", Term.from.EMIS, ignore.case=TRUE)~ "Lisinopril",
    grepl("Quinapril", DrugSubstanceName, ignore.case=TRUE)|grepl("Quinapril", Term.from.EMIS, ignore.case=TRUE)~ "Quinapril",
    grepl("Imidapril", DrugSubstanceName, ignore.case=TRUE)|grepl("Imidapril", Term.from.EMIS, ignore.case=TRUE)~ "Imidapril",
    grepl("Perindopril", DrugSubstanceName, ignore.case=TRUE)|grepl("Perindopril", Term.from.EMIS, ignore.case=TRUE)~ "Perindopril",
    grepl("Moexipril", DrugSubstanceName, ignore.case=TRUE)|grepl("Moexipril", Term.from.EMIS, ignore.case=TRUE)~ "Moexipril",
    grepl("Ramipril|Tritace", DrugSubstanceName, ignore.case=TRUE)|grepl("Ramipril|Tritace", Term.from.EMIS, ignore.case=TRUE)~ "Ramipril",
    grepl("Trandolapril", DrugSubstanceName, ignore.case=TRUE)|grepl("Trandolapril", Term.from.EMIS, ignore.case=TRUE)~ "Trandolapril",
    TRUE ~ "Check"))
    
table(AurumCCB$Name)         

#Re-run to check there are no more
AurumCCBLab1<-unique(as.list((word(AurumCCB$ProductName, 1)), 
                             (word(AurumCCB$Term.from.EMIS, 1)), 
                             (word(AurumCCB$DrugSubstanceName, 1))))

AurumCCBLab1<-subset(AurumCCBLab1, AurumCCBLab1!= "")
AurumCCB1<-subset(AurumProd, !(ProdCodeId %in% AurumCCB$ProdCodeId))
AurumCCB1<-subset(AurumCCB1, (grepl(paste(AurumCCBLab1, collapse='|'), ProductName, ignore.case=TRUE))|
                     (grepl(paste(AurumCCBLab1, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                     (grepl(paste(AurumCCBLab1, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

AurumCCB1<-subset(AurumCCB1, DrugSubstanceName=="")

AurumCCB1<-AurumCCB1%>%
  mutate(Group = case_when(grepl("emla|lidocain|absorbable|Colistin|Fabahistin|limb|web|half|nimrodel|Otrivine|unilet|sterile|Vincristine|universal|Perindopril|eye|prilocain|Citanest", Term.from.EMIS, ignore.case=TRUE) ~ "Not",
                           grepl("optil|Dilcardia", Term.from.EMIS, ignore.case=TRUE) ~ "6 month exclusion",
                           grepl("triapin", Term.from.EMIS, ignore.case=TRUE) ~ "Combo",
                           TRUE ~ ""))%>%
  subset(Group!="Not")

#None of interest

#Sort exclusions
table(AurumCCB$Name[AurumCCB$Group =="ever exclusion"], useNA="ifany")
table(AurumCCB$DrugSubstanceName[AurumCCB$Group =="6 month exclusion"], useNA="ifany")

table(AurumCCB$Term.from.EMIS[AurumCCB$Group =="6 month exclusion" & AurumCCB$DrugSubstanceName==""], useNA="ifany")

AurumCCB$Name[AurumCCB$DrugSubstanceName=="Diltiazem hydrochloride"|grepl("Diltiazem hydrochloride", AurumCCB$Term.from.EMIS, ignore.case=TRUE)]<-"Diltiazem"
AurumCCB$Name[AurumCCB$DrugSubstanceName=="Nimodipine"]<-"Nimodipine"
AurumCCB$Name[grepl("Adizem|Calcicard|Dilcardia|optil", AurumCCB$Term.from.EMIS, ignore.case=TRUE)]<-"Diltiazem"

table(AurumCCB$Name[AurumCCB$Group =="6 month exclusion"], useNA="ifany")

AurumCCB$Class<-case_when(AurumCCB$Name=="Combo" ~ "Combo",
                          AurumCCB$Name=="Verapamil" ~ "Verapamil",
                          grepl("ipine|iazem", AurumCCB$Name, ignore.case=TRUE) ~ "CCB",
                          grepl("sartan", AurumCCB$Name, ignore.case=TRUE) ~ "ARB",
                          grepl("pril", AurumCCB$Name, ignore.case=TRUE) ~ "ACE",
                          TRUE ~ "Check")

table(AurumCCB$Class, AurumCCB$Group)
AurumCCB$Group[AurumCCB$Group=="" & AurumCCB$Class=="ACE"]<-"ever exclusion"
AurumCCB$Group[AurumCCB$Group=="" & (AurumCCB$Name=="Felodipine" |AurumCCB$Name=="Nifedipine"|AurumCCB$Name=="Verapamil"|AurumCCB$Name=="Nicardipine")]<-"Mono"
AurumCCB$Group[AurumCCB$Name=="Nimodipine"]<-"6 month exclusion"

table(AurumCCB$Name, AurumCCB$Class)
table(AurumCCB$Name, AurumCCB$Group)
table(AurumCCB$Term.from.EMIS[AurumCCB$Group=="Combo"])
table(AurumCCB$DrugSubstanceName[AurumCCB$Group=="Combo"])
table(AurumCCB$DrugSubstanceName[AurumCCB$Class=="ACE"])
table(AurumCCB$DrugSubstanceName[AurumCCB$Class=="CCB"])
table(AurumCCB$DrugSubstanceName[AurumCCB$Class=="ARB"])

AurumCCB<-subset(AurumCCB, !(grepl("Prilocaine", AurumCCB$DrugSubstanceName, ignore.case=TRUE)))

WriteCodeList(AurumCCB, "Codelists/CCBAurum.txt")

####Now try GOLD####
####CCB search#####
GoldCCB<-GoldProd%>%
  mutate(Group = case_when(grepl("prilocain", drugsubstance, ignore.case=TRUE)|grepl("prilocain", productname, ignore.case=TRUE) ~ "Not",
                           grepl("Hydrochlorothiazide|Triapin|Indapamide|atenolol", drugsubstance, ignore.case=TRUE) & grepl("ipine|verapamil|sartan|pril", drugsubstance, ignore.case=TRUE) ~"Combo",
                           grepl("Hydrochlorothiazide|Triapin|Indapamide|atenolol", productname, ignore.case=TRUE) & grepl("ipine|verapamil|sartan|pril", productname, ignore.case=TRUE) ~"Combo",
                           drugsubstance=="Amlodipine Besilate/Valsartan"|drugsubstance=="Olmesartan medoxomil/Amlodipine besilate"|drugsubstance=="Sacubitril/Valsartan"|drugsubstance=="Valsartan/Amlodipine besilate" ~ "Combo",
                           grepl("diltiazem|Nimodipine|adizem|Calcicard|Dilcardia", drugsubstance, ignore.case=TRUE)|grepl("Dilcardia|diltiazem|Nimodipine|adizem|Calcicard", productname, ignore.case=TRUE)~ "6 month exclusion",
                           grepl("caprilon|Prilocaine|pipril|Sprilon|bandage|Priligy", drugsubstance, ignore.case=TRUE)|grepl("caprilon|Prilocaine|pipril|Sprilon|bandage|Priligy",productname, ignore.case=TRUE) ~ "Not",
                           grepl("pril", drugsubstance, ignore.case=TRUE)|grepl("pril", productname, ignore.case=TRUE)~ "ever exclusion",
                           grepl("unipine|calcipine", productname, ignore.case=TRUE) ~ "Check",
                           grepl("unipine|calcipine", drugsubstance, ignore.case=TRUE) ~ "Check",
                           grepl("ipine|verapamil|sartan", productname, ignore.case=TRUE) |
                             grepl("ipine|verapamil|sartan", drugsubstance, ignore.case=TRUE) ~ "Mono",
                           bnfcode=="2050501"|bnfcode=="2050502"|bnfcode=="2060200"~ "Check",
                           TRUE ~ "Not"))%>%
  subset(Group!="Not")

table(GoldCCB$productname[GoldCCB$Group =="Mono"& GoldCCB$drugsubstance==""], useNA="ifany")
table(GoldCCB$drugsubstance[GoldCCB$Group =="Mono"], useNA="ifany")
table(GoldCCB$drugsubstance[GoldCCB$Group =="Combo"], useNA="ifany")
table(GoldCCB$drugsubstance[GoldCCB$Group =="6 month exclusion"], useNA="ifany")
table(GoldCCB$productname[GoldCCB$Group =="6 month exclusion"&GoldCCB$drugsubstance==""], useNA="ifany")

table(GoldCCB$drugsubstance[GoldCCB$Group =="ever exclusion"], useNA="ifany")
table(GoldCCB$productname[GoldCCB$Group =="ever exclusion"&GoldCCB$drugsubstance==""], useNA="ifany")
GoldCCB$Group[GoldCCB$drugsubstance=="Amlodipine besilate/Perindopril erbumine"|
                GoldCCB$drugsubstance=="Felodipine/Ramipril"|
                GoldCCB$drugsubstance=="Perindopril erbumine/Amlodipine besilate"|
                GoldCCB$drugsubstance=="Verapamil hydrochloride/Trandolapril"|
                GoldCCB$drugsubstance=="Verapamil Hydrochloride/Trandolapril"]<-"Combo"

CheckBNF<-as.data.frame(table(GoldCCB$drugsubstance, GoldCCB$bnfcode,useNA="ifany"))
CheckBNF<-subset(CheckBNF, Freq>0)
####GOT TO HERE


table(GoldCCB$productname[GoldCCB$Group =="Check"])
table(GoldCCB$drugname[GoldCCB$Group =="Check"])
GoldCCB$Group[GoldCCB$Group =="Check"]<-"Mono"

#Find any other words that might be useful
GoldCCBLab1<-unique(as.list((word(GoldCCB$productname, 1)), 
                             (word(GoldCCB$drugsubstance, 1))))

GoldCCBLab1<-subset(GoldCCBLab1, GoldCCBLab1!= "")
GoldCCB1<-subset(GoldProd, !(prodcode %in% GoldCCB$prodcode))
GoldCCB1<-subset(GoldCCB1, (grepl(paste(GoldCCBLab1, collapse='|'), productname, ignore.case=TRUE))|
                    (grepl(paste(GoldCCBLab1, collapse='|'), drugsubstance, ignore.case=TRUE)))

#Just check those without drug substance
GoldCCB1<-GoldCCB1%>%
  mutate(Group = case_when(grepl("guard|dress|Casirivimab|FOLGUARD|gel|Pristinamycin|Dalfopristin|disposal|skin|Kelocyanor|hypo|hollister|container|Cream|ANTISTIN|strip|Betahistine|Generic|Hydrochlorothiazide|Atenolol|emla|lidocain|absorbable|Colistin|Fabahistin|limb|web|half|nimrodel|Otrivine|unilet|sterile|Vincristine|universal|Perindopril|eye|prilocain|Citanest", productname, ignore.case=TRUE) ~ "Not",
                           grepl("guard|dress|Casirivimab|FOLGUARD|gel|Pristinamycin|Dalfopristin|disposal|skin|Kelocyanor|hypo|hollister|container|Cream|ANTISTIN|strip|Betahistine|Generic|Hydrochlorothiazide|Atenolol|emla|lidocain|absorbable|Colistin|Fabahistin|limb|web|half|nimrodel|Otrivine|unilet|sterile|Vincristine|universal|Perindopril|eye|prilocain|Citanest", drugsubstance, ignore.case=TRUE) ~ "Not",
                           grepl("optil|Dilcardia", productname, ignore.case=TRUE) ~ "6 month exclusion",
                           grepl("triapin|CARACE|CAPOZIDE", productname, ignore.case=TRUE) ~ "Combo",
                           TRUE ~ ""))%>%
  subset(Group!="Not")

GoldCCB<-rbind(GoldCCB, GoldCCB1)

#Label them with drug name and class

GoldCCB<-GoldCCB%>%
  mutate(Name = case_when(Group == "Combo" ~ "Combo",
                          Group == "6 month exclusion" ~ "6 month exclusion",
                          grepl("Amlodipine", drugsubstance, ignore.case=TRUE)|grepl("Amlodipine", productname, ignore.case=TRUE) ~ "Amlodipine",
                          grepl("Azilsartan", drugsubstance, ignore.case=TRUE)|grepl("Azilsartan", productname, ignore.case=TRUE)~ "Azilsartan",
                          grepl("Candesartan", drugsubstance, ignore.case=TRUE)|grepl("Candesartan", productname, ignore.case=TRUE)~ "Candesartan",
                          grepl("Eprosartan", drugsubstance, ignore.case=TRUE)|grepl("Eprosartan", productname, ignore.case=TRUE)~ "Eprosartan",
                          grepl("Felodipine|Parmid|Delofine", drugsubstance, ignore.case=TRUE)|grepl("Felodipine|Parmid|Delofine", productname, ignore.case=TRUE)~ "Felodipine",
                          grepl("Irbesartan", drugsubstance, ignore.case=TRUE)|grepl("Irbesartan", productname, ignore.case=TRUE)~ "Irbesartan",
                          grepl("Isradipine", drugsubstance, ignore.case=TRUE)|grepl("Isradipine", productname, ignore.case=TRUE)~ "Isradipine",
                          grepl("Lacidipine|calcipine", drugsubstance, ignore.case=TRUE)|grepl("Lacidipine|calcipine", productname, ignore.case=TRUE)~ "Lacidipine",
                          grepl("Lercanidipine", drugsubstance, ignore.case=TRUE)|grepl("Lercanidipine", productname, ignore.case=TRUE)~ "Lercanidipine",
                          grepl("Losartan|cozaar", drugsubstance, ignore.case=TRUE)|grepl("Losartan|cozaar", productname, ignore.case=TRUE)~ "Losartan",
                          grepl("Manidipine", drugsubstance, ignore.case=TRUE)|grepl("Manidipine", productname, ignore.case=TRUE)~ "Manidipine",
                          grepl("Nicardipine|cardene", drugsubstance, ignore.case=TRUE)|grepl("Nicardipine|cardene", productname, ignore.case=TRUE)~ "Nicardipine",
                          grepl("Nifedipine|nimodrel|Unipine|Angiopine|Adipine|adalat|SLOFEDIPINE", drugsubstance, ignore.case=TRUE)|grepl("SLOFEDIPINE|adalat|Nifedipine|nimodrel|Unipine|Angiopine|Adipine", productname, ignore.case=TRUE)~ "Nifedipine",
                          grepl("Nimodipine", drugsubstance, ignore.case=TRUE)|grepl("Nimodipine", productname, ignore.case=TRUE)~ "Nimodipine",
                          grepl("Nisoldipine", drugsubstance, ignore.case=TRUE)|grepl("Nisoldipine", productname, ignore.case=TRUE)~ "Nisoldipine",
                          grepl("Olmesartan", drugsubstance, ignore.case=TRUE)|grepl("Olmesartan", productname, ignore.case=TRUE)~ "Olmesartan",
                          grepl("Telmisartan", drugsubstance, ignore.case=TRUE)|grepl("Telmisartan", productname, ignore.case=TRUE)~ "Telmisartan",
                          grepl("Valsartan", drugsubstance, ignore.case=TRUE)|grepl("Valsartan", productname, ignore.case=TRUE)~ "Valsartan",
                          grepl("Verapamil|Cordilox|Securon", drugsubstance, ignore.case=TRUE)|grepl("Verapamil|Cordilox|Securon", productname, ignore.case=TRUE)~ "Verapamil",
                          grepl("Captopril", drugsubstance, ignore.case=TRUE)|grepl("Captopril", productname, ignore.case=TRUE)~ "Captopril",
                          grepl("Cilazapril|Vascace", drugsubstance, ignore.case=TRUE)|grepl("Cilazapril|Vascace", productname, ignore.case=TRUE)~ "Cilazapril",
                          grepl("Enalapril|Innovace", drugsubstance, ignore.case=TRUE)|grepl("Enalapril|Innovace", productname, ignore.case=TRUE)~ "Enalapril",
                          grepl("Fosinopril", drugsubstance, ignore.case=TRUE)|grepl("Fosinopril", productname, ignore.case=TRUE)~ "Fosinopril",
                          grepl("Lisinopril", drugsubstance, ignore.case=TRUE)|grepl("Lisinopril", productname, ignore.case=TRUE)~ "Lisinopril",
                          grepl("Quinapril", drugsubstance, ignore.case=TRUE)|grepl("Quinapril", productname, ignore.case=TRUE)~ "Quinapril",
                          grepl("Imidapril", drugsubstance, ignore.case=TRUE)|grepl("Imidapril", productname, ignore.case=TRUE)~ "Imidapril",
                          grepl("Perindopril", drugsubstance, ignore.case=TRUE)|grepl("Perindopril", productname, ignore.case=TRUE)~ "Perindopril",
                          grepl("Moexipril", drugsubstance, ignore.case=TRUE)|grepl("Moexipril", productname, ignore.case=TRUE)~ "Moexipril",
                          grepl("Ramipril|Tritace", drugsubstance, ignore.case=TRUE)|grepl("Ramipril|Tritace", productname, ignore.case=TRUE)~ "Ramipril",
                          grepl("Trandolapril", drugsubstance, ignore.case=TRUE)|grepl("Trandolapril", productname, ignore.case=TRUE)~ "Trandolapril",
                          TRUE ~ "Check"))

table(GoldCCB$Name)         

#Re-run to check there are no more
GoldCCBLab1<-unique(as.list((word(GoldCCB$productname, 1)), 
                            (word(GoldCCB$drugsubstance, 1))))

GoldCCBLab1<-subset(GoldCCBLab1, GoldCCBLab1!= "")
GoldCCB1<-subset(GoldProd, !(prodcode %in% GoldCCB$prodcode))
GoldCCB1<-subset(GoldCCB1, (grepl(paste(GoldCCBLab1, collapse='|'), productname, ignore.case=TRUE))|
                   (grepl(paste(GoldCCBLab1, collapse='|'), drugsubstance, ignore.case=TRUE)))

#Just check those without drug substance
GoldCCB1<-GoldCCB1%>%
  mutate(Group = case_when(grepl("guard|dress|Casirivimab|FOLGUARD|gel|Pristinamycin|Dalfopristin|disposal|skin|Kelocyanor|hypo|hollister|container|Cream|ANTISTIN|strip|Betahistine|Generic|Hydrochlorothiazide|Atenolol|emla|lidocain|absorbable|Colistin|Fabahistin|limb|web|half|nimrodel|Otrivine|unilet|sterile|Vincristine|universal|Perindopril|eye|prilocain|Citanest", productname, ignore.case=TRUE) ~ "Not",
                           grepl("guard|dress|Casirivimab|FOLGUARD|gel|Pristinamycin|Dalfopristin|disposal|skin|Kelocyanor|hypo|hollister|container|Cream|ANTISTIN|strip|Betahistine|Generic|Hydrochlorothiazide|Atenolol|emla|lidocain|absorbable|Colistin|Fabahistin|limb|web|half|nimrodel|Otrivine|unilet|sterile|Vincristine|universal|Perindopril|eye|prilocain|Citanest", drugsubstance, ignore.case=TRUE) ~ "Not",
                           grepl("optil|Dilcardia", productname, ignore.case=TRUE) ~ "6 month exclusion",
                           grepl("triapin|CARACE|CAPOZIDE", productname, ignore.case=TRUE) ~ "Combo",
                           TRUE ~ ""))%>%
  subset(Group!="Not")

#None of interest

#Sort exclusions
table(GoldCCB$Name[GoldCCB$Group =="ever exclusion"], useNA="ifany")
table(GoldCCB$drugsubstance[GoldCCB$Group =="6 month exclusion"], useNA="ifany")

table(GoldCCB$productname[GoldCCB$Group =="6 month exclusion" & GoldCCB$drugsubstance==""], useNA="ifany")

GoldCCB$Name[GoldCCB$drugsubstance=="Diltiazem hydrochloride"]<-"Diltiazem"
GoldCCB$Name[GoldCCB$drugsubstance=="Nimodipine"]<-"Nimodipine"
GoldCCB$Name[grepl("Adizem|Calcicard|Dilcardia|diltiazem", GoldCCB$productname, ignore.case=TRUE)|grepl("Adizem|Calcicard|Dilcardia|diltiazem", GoldCCB$drugsubstance, ignore.case=TRUE)]<-"Diltiazem"
GoldCCB$Name[GoldCCB$drugsubstance=="Diltiazem Hydrochloride/Hydrochlorothiazide"]<-"Combo"

table(GoldCCB$Name[GoldCCB$Group =="6 month exclusion"], useNA="ifany")

GoldCCB$Class<-case_when(GoldCCB$Name=="Combo" ~ "Combo",
                          GoldCCB$Name=="Verapamil" ~ "Verapamil",
                          grepl("ipine|iazem", GoldCCB$Name, ignore.case=TRUE) ~ "CCB",
                          grepl("sartan", GoldCCB$Name, ignore.case=TRUE) ~ "ARB",
                          grepl("pril", GoldCCB$Name, ignore.case=TRUE) ~ "ACE",
                          TRUE ~ "Check")

table(GoldCCB$Class, GoldCCB$Group)
GoldCCB$Group[GoldCCB$Group=="" & GoldCCB$Class=="ACE"]<-"ever exclusion"
GoldCCB$Group[GoldCCB$Group=="" & (GoldCCB$Name=="Losartan" |GoldCCB$Name=="Nifedipine"|GoldCCB$Name=="Verapamil"|GoldCCB$Name=="Felodipine")]<-"Mono"


table(GoldCCB$Name, GoldCCB$Class)
table(GoldCCB$Name, GoldCCB$Group)
table(GoldCCB$productname[GoldCCB$Group=="Combo"])
table(GoldCCB$drugsubstance[GoldCCB$Group=="Combo"])
table(GoldCCB$drugsubstance[GoldCCB$Class=="ACE"])
table(GoldCCB$drugsubstance[GoldCCB$Class=="CCB"])
table(GoldCCB$drugsubstance[GoldCCB$Class=="ARB"])

WriteCodeList(GoldCCB, "Codelists/CCBGold.txt")

####Ensure there are none in Gold that aren't in Aurum and vice versa####

GoldCCBLab2<-unique(as.list((word(GoldCCB$productname, 1)), 
                             (word(GoldCCB$bnfchapter, 1)), 
                             (word(GoldCCB$drugsubstance, 1))))

AurumCCBLab2<-unique(as.list((word(AurumCCB$ProductName, 1)), 
                              (word(AurumCCB$Term.from.EMIS, 1)), 
                              (word(AurumCCB$DrugSubstanceName, 1))))

AurumCCBLab2<-lapply(AurumCCBLab2, tolower)
GoldCCBLab2<-lapply(GoldCCBLab2, tolower)
AurumCCBLab2<-unique(AurumCCBLab2)
GoldCCBLab2<-unique(GoldCCBLab2)

GoldCCBLab3<-subset(GoldCCBLab2, !(GoldCCBLab2 %in% AurumCCBLab2))
AurumCCBLab3<-subset(AurumCCBLab2, !(AurumCCBLab2 %in% GoldCCBLab2))

GoldCCBLab3<-subset(GoldCCBLab3, GoldCCBLab3!="")
AurumCCBLab3<-subset(AurumCCBLab3, AurumCCBLab3!="")


#Check for those terms in the AurumProd dictionary
GoldCCB2<-subset(GoldProd, !(prodcode %in% GoldCCB$prodcode))
GoldCCB2<-subset(GoldCCB2, (grepl(paste(AurumCCBLab3, collapse='|'), productname, ignore.case=TRUE))|
                    (grepl(paste(AurumCCBLab3, collapse='|'), bnfchapter, ignore.case=TRUE))|
                    (grepl(paste(AurumCCBLab3, collapse='|'), drugsubstance, ignore.case=TRUE)))

#None found


AurumCCB2<-subset(AurumProd, !(ProdCodeId %in% AurumCCB$ProdCodeId))
AurumCCB2<-subset(AurumCCB2, (grepl(paste(GoldCCBLab3, collapse='|'), ProductName, ignore.case=TRUE))|
                     (grepl(paste(GoldCCBLab3, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                     (grepl(paste(GoldCCBLab3, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

AurumCCB2<-subset(AurumCCB2, grepl("Nifelease|Nifensar|Calanif|Britiazim|Berkatens|Anoheal|Calazem|Capto|Ednyt|Felendil|Nifedotard|Nivaten", AurumCCB2$Term.from.EMIS, ignore.case=TRUE))
AurumCCB2$Name<-case_when(grepl("Nifelease|Nifensar|Calanif|Nifedotard|Nivaten", AurumCCB2$Term.from.EMIS, ignore.case=TRUE) ~ "Nifedipine",
                          grepl("Britiazim|Anoheal|Calazem", AurumCCB2$Term.from.EMIS, ignore.case=TRUE) ~ "Diltiazem",
                          grepl("Berkatens", AurumCCB2$Term.from.EMIS, ignore.case=TRUE) ~ "Verapamil",
                          grepl("Capto", AurumCCB2$Term.from.EMIS, ignore.case=TRUE) ~ "Captopril",
                          grepl("Ednyt", AurumCCB2$Term.from.EMIS, ignore.case=TRUE) ~ "Enalopril",
                          grepl("Felendil", AurumCCB2$Term.from.EMIS, ignore.case=TRUE) ~ "Felodipine",
                          TRUE ~ "Check")

AurumCCB2$Class<-case_when(AurumCCB2$Name=="Combo" ~ "Combo",
                           AurumCCB2$Name=="Verapamil" ~ "Verapamil",
                           grepl("capto", AurumCCB2$Name, ignore.case=TRUE) ~ "Combo",
                          grepl("ipine|iazem", AurumCCB2$Name, ignore.case=TRUE) ~ "CCB",
                          grepl("sartan", AurumCCB2$Name, ignore.case=TRUE) ~ "ARB",
                          grepl("pril", AurumCCB2$Name, ignore.case=TRUE) ~ "ACE",
                          TRUE ~ "Check")

AurumCCB2$Group<-case_when(AurumCCB2$Class=="Combo" ~ "Combo",
                           AurumCCB2$Class=="ACE" ~ "ever exclusion",
                           grepl("iazem", AurumCCB2$Name, ignore.case=TRUE) ~ "6 month exclusion",
                           TRUE ~ "Mono")

AurumCCB2<-AurumCCB2[,c(1:10, 13,11, 12)]

AurumCCB<-rbind(AurumCCB, AurumCCB2)

####Run aurum one more time
AurumCCBLab1<-unique(as.list((word(AurumCCB$ProductName, 1)), 
                             (word(AurumCCB$Term.from.EMIS, 1)), 
                             (word(AurumCCB$DrugSubstanceName, 1))))

AurumCCBLab1<-subset(AurumCCBLab1, AurumCCBLab1!= "")
AurumCCB1<-subset(AurumProd, !(ProdCodeId %in% AurumCCB$ProdCodeId))
AurumCCB1<-subset(AurumCCB1, (grepl(paste(AurumCCBLab1, collapse='|'), ProductName, ignore.case=TRUE))|
                    (grepl(paste(AurumCCBLab1, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                    (grepl(paste(AurumCCBLab1, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

AurumCCB1<-subset(AurumCCB1, DrugSubstanceName=="")

AurumCCB1<-AurumCCB1%>%
  mutate(Group = case_when(grepl("emla|lidocain|absorbable|Colistin|Fabahistin|limb|web|half|nimrodel|Otrivine|unilet|sterile|Vincristine|universal|Perindopril|eye|prilocain|Citanest", Term.from.EMIS, ignore.case=TRUE) ~ "Not",
                           grepl("optil|Dilcardia", Term.from.EMIS, ignore.case=TRUE) ~ "6 month exclusion",
                           grepl("triapin", Term.from.EMIS, ignore.case=TRUE) ~ "Combo",
                           TRUE ~ ""))%>%
  subset(Group!="Not")

WriteCodeList(AurumCCB, "Codelists/CCBAurum.txt")

####Medcodes suggestive of CCBS####
AurumMed<-ReadAurumMedCodelist(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt"))
GoldMed<-ReadGoldMedCodelist(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGOLD/medical.txt"))

AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)
GoldMed$desc<-tolower(GoldMed$desc)

AurumCCB<-AurumMed%>%
  mutate(CCB = case_when(grepl("pulm|second|cause|liver", Term, ignore.case=TRUE) ~ 0,
    grepl("calcium channel", Term, ignore.case=TRUE) ~ 1,
                            grepl("calcium-channel", Term, ignore.case=TRUE) ~ 1,
                            grepl("Angiotensin|ace inhib", Term, ignore.case=TRUE) ~ 1,
                            grepl("anti", Term, ignore.case=TRUE) & grepl("hyperten", Term, ignore.case=TRUE) ~ 1,
                            grepl("medication|therapy|treatm|drug", Term, ignore.case=TRUE) & (grepl("hyperten", Term, ignore.case=TRUE)|grepl("blood pressure", Term, ignore.case=TRUE)) ~ 1,
                            TRUE ~ 0))%>%
  subset(CCB==1)

AurumCCB<-AurumCCB%>%
  mutate(Type = case_when(grepl("not indicated|declined|contra|refuse", Term, ignore.case=TRUE) ~ "not prescribed",
                         grepl("not tolerated|aller|reaction|advers", Term, ignore.case=TRUE) ~ "allergy",
                         grepl("poison|overd", Term, ignore.case=TRUE) ~ "accidental",
                         grepl("history|fetus", Term, ignore.case=TRUE) ~ "old",
                         grepl("indicated|refer", Term, ignore.case=TRUE) ~ "indicated",
                         grepl("target|indicated|therapy", Term, ignore.case=TRUE) ~ "prescribed",
                         grepl("level|serum|reading", Term, ignore.case=TRUE) ~ "drop",
                         Term=="angiotensin ii"|Term=="angiotensin i"|Term=="angiotensin converting enzyme"|Term=="ace - angiotensin-converting enzyme" ~"drop",
                         TRUE ~ "prescribed"))%>%
  subset(Type!="drop")
                         

####Gold medcodes####
GoldCCB<-GoldMed%>%
  mutate(CCB = case_when(grepl("pulm|second|cause|liver", desc, ignore.case=TRUE) ~ 0,
    grepl("calcium channel", desc, ignore.case=TRUE) ~ 1,
                         grepl("calcium-channel", desc, ignore.case=TRUE) ~ 1,
                         grepl("Angiotensin|ace inhib", desc, ignore.case=TRUE) ~ 1,
                         grepl("anti", desc, ignore.case=TRUE) & grepl("hyperten", desc, ignore.case=TRUE) ~ 1,
                         grepl("medication|therapy|treatm|drug", desc, ignore.case=TRUE) & (grepl("hyperten", desc, ignore.case=TRUE)|grepl("blood pressure", desc, ignore.case=TRUE)) ~ 1,
                         TRUE ~ 0))%>%
  subset(CCB==1)

GoldCCB<-GoldCCB%>%
  mutate(Type = case_when(grepl("not indicated|declined|contra|refuse", desc, ignore.case=TRUE) ~ "not prescribed",
                                 grepl("not tolerated|aller|reaction|advers", desc, ignore.case=TRUE) ~ "allergy",
                                 grepl("poison|overd", desc, ignore.case=TRUE) ~ "accidental",
                                 grepl("history|fetus", desc, ignore.case=TRUE) ~ "old",
                          grepl("indicated|refer", desc, ignore.case=TRUE) ~ "indicated",
                                 grepl("target|indicated|therapy", desc, ignore.case=TRUE) ~ "prescribed",
                                 grepl("level|serum|reading", desc, ignore.case=TRUE) ~ "drop",
                          desc=="angiotensin ii"|desc=="angiotensin i"|desc=="angiotensin converting enzyme"|desc=="ace - angiotensin-converting enzyme" ~"drop",
                                 TRUE ~ "prescribed"))%>%
           subset(Type!="drop")

####Compare gold and aurum####
GoldCCBCheck<-subset(GoldCCB, !(desc %in% AurumCCB$Term))
AddtoAurum<-subset(AurumMed, Term %in% GoldCCBCheck$desc) #None to add on term

AurumCCBCheck<-subset(AurumCCB, !(Term %in% GoldCCB$desc))
AddtoGold<-subset(GoldMed, desc %in% AurumCCBCheck$Term) #None to add on term

GoldCCBCheck<-subset(GoldCCB, !(readcode %in% AurumCCB$CleansedReadCode))
AddtoAurum<-subset(AurumMed, CleansedReadCode %in% GoldCCBCheck$readcode) #none to add on readcode

AurumCCBCheck<-subset(AurumCCB, !(CleansedReadCode %in% GoldCCB$readcode) & CleansedReadCode!="")
AddtoGold<-subset(GoldMed, readcode %in% AurumCCBCheck$CleansedReadCode) #4 to add on readcode

AurumCCBRead<-AurumCCB
GoldCCBRead<-GoldCCB


#Save files
WriteCodeList(GoldCCBRead,  "Codelists/CCBGoldmed.txt")
WriteCodeList(AurumCCBRead,  "Codelists/CCBAurummed.txt")

rm(list = ls(all.names = TRUE))
