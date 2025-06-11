####Beta blockers code list####

#Call start script
source("./Scripts/1aSetUp.R")

####Load product dictionaries####
AurumProd<-ReadAurumProdCodelist(paste0(DataDir,"/LookUps/202205_Lookups_CPRDAurum/202205_EMISProductDictionary.txt"))
GoldProd<-ReadGoldProdCodelist(paste0(DataDir,"/LookUps/202303_Lookups_CPRDGOLD/product.txt"))

####B-blocker and diuretic search#####
AurumBlockers<-AurumProd%>%
  mutate(Group = case_when(BNFChapter=="11060000" | grepl("eye", Formulation, ignore.case=TRUE) ~ "Not",
                           grepl("Inj",Formulation, ignore.case=TRUE)|grepl("Inj",Term.from.EMIS, ignore.case=TRUE) ~ "Not",
    grepl("bisoprolol|carvedilol|nebivolol|atenolol|metoprolol|propranolol|timolol|Acebutolol|Celiprolol|Labetolol|Nadolol|Nebivolol|Pindolol|prenozide|oxprenolol|sotalol", DrugSubstanceName, ignore.case=TRUE)|grepl("bisoprolol|carvedilol|nebivolol|atenolol|metoprolol|propranolol|timolol|Acebutolol|Celiprolol|Labetolol|Nadolol|Nebivolol|Pindolol|prenozide|oxprenolol|sotalol", ProductName, ignore.case=TRUE)|grepl("bisoprolol|carvedilol|nebivolol|atenolol|metoprolol|propranolol|timolol|Acebutolol|Celiprolol|Labetolol|Nadolol|Nebivolol|Pindolol|prenozide|oxprenolol|sotalol", Term.from.EMIS, ignore.case=TRUE) ~"B-blocker",
                           grepl("Chlorthalidone|Chlortalidone|Indapamide|Metolazone|Xipamide|thiazide|amiolzide|tenidone|xipamide|furosemide|torasemide", DrugSubstanceName, ignore.case=TRUE)|grepl("Chlorthalidone|Chlortalidone|Indapamide|Metolazone|Xipamide|thiazide|amiolzide|tenidone|xipamide|furosemide|torasemide", ProductName, ignore.case=TRUE)|grepl("Chlorthalidone|Chlortalidone|Indapamide|Metolazone|Xipamide|thiazide|amiolzide|tenidone|xipamide|furosemide|torasemide", Term.from.EMIS, ignore.case=TRUE)~ "Diuretic",
                           grepl("Spironolactone", DrugSubstanceName, ignore.case=TRUE)|grepl("Spironolactone", ProductName, ignore.case=TRUE)|grepl("Spironolactone ", Term.from.EMIS, ignore.case=TRUE)~ "Spironolactone ",
                           grepl("Doxazosin|indoramin|phenoxybenzamine|phentolamine|prazosin|terazosin", DrugSubstanceName, ignore.case=TRUE)|grepl("Doxazosin|indoramin|phenoxybenzamine|phentolamine|prazosin|terazosin",ProductName, ignore.case=TRUE)|grepl("Doxazosin|indoramin|phenoxybenzamine|phentolamine|prazosin|terazosin",Term.from.EMIS, ignore.case=TRUE) ~ "Alpha blocker",
    
                           TRUE ~ "Not"))%>%
  subset(Group!="Not")

table(AurumBlockers$DrugSubstanceName[AurumBlockers$Group =="Diuretic"], useNA="ifany")
table(AurumBlockers$DrugSubstanceName[AurumBlockers$Group =="B-blocker"], useNA="ifany")
table(AurumBlockers$DrugSubstanceName[AurumBlockers$Group =="alpha blocker"], useNA="ifany")

#Find any other words that might be useful
AurumBlockersLab1<-unique(as.list((word(AurumBlockers$ProductName, 1)), 
                              (word(AurumBlockers$Term.from.EMIS, 1)), 
                              (word(AurumBlockers$DrugSubstanceName, 1))))

AurumBlockersLab1<-subset(AurumBlockersLab1, AurumBlockersLab1!= "")
AurumBlockers1<-subset(AurumProd, !(ProdCodeId %in% AurumBlockers$ProdCodeId))
AurumBlockers1<-subset(AurumBlockers1, (grepl(paste(AurumBlockersLab1, collapse='|'), ProductName, ignore.case=TRUE))|
                     (grepl(paste(AurumBlockersLab1, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                     (grepl(paste(AurumBlockersLab1, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

#Just check those without drug substance
AurumBlockers1<-subset(AurumBlockers1, DrugSubstanceName=="")
AurumBlockers1<-AurumBlockers1%>%
  mutate(Group = case_when(grepl("Halfan|eye|suture|Enalapril|protect|boot|choc|paste|Perindopril|skinnies|strip|panty|injection|navidrex", Term.from.EMIS, ignore.case=TRUE) ~ "Not",
                           grepl("Alphavase|Baratol|Cardura|Hytrin|hypovase", Term.from.EMIS, ignore.case=TRUE) ~ "alpha blocker",
                           grepl("Centyl|Hygroton|Kalspare|Moduretic|Lopresoretic|Lasix|Mefruside", Term.from.EMIS, ignore.case=TRUE) ~ "diuretic",
                           grepl("Eucardic|Betadur|Propanix|Prestim|Trasicor", Term.from.EMIS, ignore.case=TRUE) ~ "b-blocker",
                           grepl("spirospare", Term.from.EMIS, ignore.case=TRUE) ~ "spironolactone",
                           grepl("Atenixco", Term.from.EMIS, ignore.case=TRUE) ~ "combo",
                          TRUE ~ ""))%>%
  subset(Group!="Not")

AurumBlockers<-rbind(AurumBlockers, AurumBlockers1)

#Re-run to check there are no more
AurumBlockersLab1<-unique(as.list((word(AurumBlockers$ProductName, 1)), 
                             (word(AurumBlockers$Term.from.EMIS, 1)), 
                             (word(AurumBlockers$DrugSubstanceName, 1))))

AurumBlockersLab1<-subset(AurumBlockersLab1, AurumBlockersLab1!= "")
AurumBlockers1<-subset(AurumProd, !(ProdCodeId %in% AurumBlockers$ProdCodeId))
AurumBlockers1<-subset(AurumBlockers1, (grepl(paste(AurumBlockersLab1, collapse='|'), ProductName, ignore.case=TRUE))|
                     (grepl(paste(AurumBlockersLab1, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                     (grepl(paste(AurumBlockersLab1, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

AurumBlockers1<-subset(AurumBlockers1, DrugSubstanceName=="")

AurumBlockers1<-AurumBlockers1%>%
  mutate(Group = case_when(grepl("Halfan|eye|suture|Enalapril|protect|boot|choc|paste|Perindopril|skinnies|strip|panty|injection|navidrex", Term.from.EMIS, ignore.case=TRUE) ~ "Not",
                           TRUE ~ ""))%>%
  subset(Group!="Not")

#None of interest
#Remove injections

#None to remove

WriteCodeList(AurumBlockers, "Codelists/BlockersDiureticsAurum.txt")

####Now try GOLD####
####search#####
GoldBlockers<-GoldProd%>%
  mutate(Group = case_when(bnfcode=="11060000" | grepl("eye|injection", formulation, ignore.case=TRUE) |grepl("eye|injection", productname, ignore.case=TRUE) ~ "Not",
                           grepl("Inj|infus",formulation, ignore.case=TRUE)|grepl("Inj|infus",productname, ignore.case=TRUE) ~ "Not",
                           grepl("bisoprolol|carvedilol|nebivolol|atenolol|metoprolol|propranolol|timolol|Acebutolol|Celiprolol|Labetolol|Nadolol|Nebivolol|Pindolol|prenozide|oxprenolol|sotalol", drugsubstance, ignore.case=TRUE)|grepl("bisoprolol|carvedilol|nebivolol|atenolol|metoprolol|propranolol|timolol|Acebutolol|Celiprolol|Labetolol|Nadolol|Nebivolol|Pindolol|prenozide|oxprenolol|sotalol", productname, ignore.case=TRUE) ~"B-blocker",
                           grepl("Chlorthalidone|Chlortalidone|Indapamide|Metolazone|Xipamide|thiazide|amiolzide|tenidone|xipamide|furosemide|torasemide", drugsubstance, ignore.case=TRUE)|grepl("Chlorthalidone|Chlortalidone|Indapamide|Metolazone|Xipamide|thiazide|amiolzide|tenidone|xipamide|furosemide|torasemide", productname, ignore.case=TRUE)~ "Diuretic",
                           grepl("Spironolactone", drugsubstance, ignore.case=TRUE)|grepl("Spironolactone", productname, ignore.case=TRUE)~ "Spironolactone",
                           grepl("Doxazosin|indoramin|phenoxybenzamine|phentolamine|prazosin|terazosin", drugsubstance, ignore.case=TRUE)|grepl("Doxazosin|indoramin|phenoxybenzamine|phentolamine|prazosin|terazosin",productname, ignore.case=TRUE) ~ "Alpha blocker",
                           TRUE ~ "Not"))%>%
  subset(Group!="Not")

table(GoldBlockers$route)
table(GoldBlockers$formulation)

table(GoldBlockers$productname[GoldBlockers$Group =="B-blocker"& GoldBlockers$drugsubstance==""], useNA="ifany")
table(GoldBlockers$drugsubstance[GoldBlockers$Group =="B-blocker"], useNA="ifany")
table(GoldBlockers$drugsubstance[GoldBlockers$Group =="Diuretic"], useNA="ifany")
table(GoldBlockers$drugsubstance[GoldBlockers$Group =="Spironolactone"], useNA="ifany")
table(GoldBlockers$drugsubstance[GoldBlockers$Group =="Alpha blocker"], useNA="ifany")

CheckBNF<-as.data.frame(table(GoldBlockers$drugsubstance, GoldBlockers$bnfcode,useNA="ifany"))
CheckBNF<-subset(CheckBNF, Freq>0)

#Find any other words that might be useful
GoldBlockersLab1<-unique(as.list((word(GoldBlockers$productname, 1)), 
                             (word(GoldBlockers$drugsubstance, 1))))

GoldBlockersLab1<-subset(GoldBlockersLab1, GoldBlockersLab1!= "")
GoldBlockers1<-subset(GoldProd, !(prodcode %in% GoldBlockers$prodcode))
GoldBlockers1<-subset(GoldBlockers1, (grepl(paste(GoldBlockersLab1, collapse='|'), productname, ignore.case=TRUE))|
                    (grepl(paste(GoldBlockersLab1, collapse='|'), drugsubstance, ignore.case=TRUE)))

#Just check those without drug substance
GoldBlockers1<-subset(GoldBlockers1, drugsubstance=="")

GoldBlockers1<-GoldBlockers1%>%
  mutate(Group = case_when(grepl("Halfan|eye|suture|Enalapril|protect|boot|choc|paste|Perindopril|skinnies|strip|panty|injection|navidrex|dress|generic", productname, ignore.case=TRUE) ~ "Not",
                           grepl("captopril|NIFEDIPINE|DILTIAZEM|ADIZEM|IRBESARTAN|MEPROBAMATE|SERPASIL|RESERPINE", productname, ignore.case=TRUE) ~ "Not",
                           grepl("Acbs|saline|Angiotensin|feed|nutri|anti|camo|catheter|hormon|food|emol|bowel|supp|homeo|formula|misc|drug|calcium|comp|deo|mouth", bnfchapter, ignore.case=TRUE) ~ "Not", 
                           grepl("oest|nasal|vit|lubri|prep|test|head|herb|gel|iron|opi|pancreatin|soap", bnfchapter, ignore.case=TRUE) ~ "Not",                   
                           grepl("Alphavase|Baratol|Cardura|Hytrin|hypovase|METHYLDOPA|DIBENYLINE", productname, ignore.case=TRUE) ~ "alpha blocker",
                                                      grepl("LASIX|amilofruse|frumil|Centyl|Hygroton|Kalspare|Moduretic|Lopresoretic|AMILORIDE|DYAZIDE|ENDURONYL|ESIDREX|NATRILIX|FRUSEMIDE", productname, ignore.case=TRUE) ~ "diuretic",
                                                      grepl("Eucardic|Betadur|Propanix|Prestim|Trasicor|SOTALOL|BEDRANO|BETALOC|INDERAL|LOPRESOR|SOTACOR|SOTAZIDE", productname, ignore.case=TRUE) ~ "b-blocker",
                                                      grepl("spirospare", productname, ignore.case=TRUE) ~ "spironolactone",
                                                      grepl("Atenixco|ALDACTIDE|CAPOZIDE|CARACE|MODUCREN|TOTARETIC|TENORETIC", productname, ignore.case=TRUE) ~ "combo",
                                                      TRUE ~ ""))%>%
  subset(Group!="Not")

GoldBlockers<-rbind(GoldBlockers, GoldBlockers1)

#Re-run to check there are no more
GoldBlockersLab1<-unique(as.list((word(GoldBlockers$productname, 1)), 
                            (word(GoldBlockers$drugsubstance, 1))))

GoldBlockersLab1<-subset(GoldBlockersLab1, GoldBlockersLab1!= "")
GoldBlockers1<-subset(GoldProd, !(prodcode %in% GoldBlockers$prodcode))
GoldBlockers1<-subset(GoldBlockers1, (grepl(paste(GoldBlockersLab1, collapse='|'), productname, ignore.case=TRUE))|
                   (grepl(paste(GoldBlockersLab1, collapse='|'), drugsubstance, ignore.case=TRUE)))

#Just check those without drug substance
GoldBlockers1<-GoldBlockers1%>%
  mutate(Group = case_when(grepl("Halfan|eye|suture|Enalapril|protect|boot|choc|paste|Perindopril|skinnies|strip|panty|injection|navidrex|dress|generic", productname, ignore.case=TRUE) ~ "Not",
                           grepl("captopril|NIFEDIPINE|DILTIAZEM|ADIZEM|IRBESARTAN|MEPROBAMATE|SERPASIL|RESERPINE", productname, ignore.case=TRUE) ~ "Not",
                           grepl("Acbs|saline|Angiotensin|feed|nutri|anti|camo|catheter|hormon|food|emol|bowel|supp|homeo|formula|misc|drug|calcium|comp|deo|mouth", bnfchapter, ignore.case=TRUE) ~ "Not", 
                           grepl("oest|nasal|vit|lubri|prep|test|head|herb|gel|iron|opi|pancreatin|soap", bnfchapter, ignore.case=TRUE) ~ "Not",
                           TRUE ~ ""))%>%
  subset(Group!="Not")

#None of interest

#Remove injections
GoldBlockers<-subset(GoldBlockers, !grepl("inj", productname, ignore.case=TRUE))

WriteCodeList(GoldBlockers, "Codelists/BlockersDiureticsGold.txt")

####Ensure there are none in Gold that aren't in Aurum and vice versa####

GoldBlockersLab2<-unique(as.list((word(GoldBlockers$productname, 1)), 
                             (word(GoldBlockers$bnfchapter, 1)), 
                             (word(GoldBlockers$drugsubstance, 1))))

AurumBlockersLab2<-unique(as.list((word(AurumBlockers$ProductName, 1)), 
                              (word(AurumBlockers$Term.from.EMIS, 1)), 
                              (word(AurumBlockers$DrugSubstanceName, 1))))

AurumBlockersLab2<-lapply(AurumBlockersLab2, tolower)
GoldBlockersLab2<-lapply(GoldBlockersLab2, tolower)
AurumBlockersLab2<-unique(AurumBlockersLab2)
GoldBlockersLab2<-unique(GoldBlockersLab2)

GoldBlockersLab3<-subset(GoldBlockersLab2, !(GoldBlockersLab2 %in% AurumBlockersLab2))
AurumBlockersLab3<-subset(AurumBlockersLab2, !(AurumBlockersLab2 %in% GoldBlockersLab2))

GoldBlockersLab3<-subset(GoldBlockersLab3, GoldBlockersLab3!="")
AurumBlockersLab3<-subset(AurumBlockersLab3, AurumBlockersLab3!="")


#Check for those terms in the AurumProd dictionary
GoldBlockers2<-subset(GoldProd, !(prodcode %in% GoldBlockers$prodcode))
GoldBlockers2<-subset(GoldBlockers2, (grepl(paste(AurumBlockersLab3, collapse='|'), productname, ignore.case=TRUE))|
                    (grepl(paste(AurumBlockersLab3, collapse='|'), bnfchapter, ignore.case=TRUE))|
                    (grepl(paste(AurumBlockersLab3, collapse='|'), drugsubstance, ignore.case=TRUE)))

#None found


AurumBlockers2<-subset(AurumProd, !(ProdCodeId %in% AurumBlockers$ProdCodeId))
AurumBlockers2<-subset(AurumBlockers2, (grepl(paste(GoldBlockersLab3, collapse='|'), ProductName, ignore.case=TRUE))|
                     (grepl(paste(GoldBlockersLab3, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                     (grepl(paste(GoldBlockersLab3, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

AurumBlockers2<-subset(AurumBlockers2, grepl("Aluzine|Apsolol|Betadur|Berkolol|Berkozide|Blocadren|Cardinol|Delvas|Decaserpyl|Diatensec|Diuresal|Dryptal|Enduron|Esidrex|Frusemide|Hydrenox|Hydromet|Hypertane|Lasipressin|Lopranol|Metoros|Mepranix|Meprobamate|Natramid|Bendromax|Normetic|Opumide|Oxyprenix|Probeta|Propanix|Saluric|Serpasil|slow|Sotazide|Spiroctan|Spiro|Spiretic|Spirolone|Spiroprop|Totamol|Tolerzide|Vasaten|Xuret|Atenamin|Capto-co", AurumBlockers2$Term.from.EMIS, ignore.case=TRUE))
AurumBlockers2<-subset(AurumBlockers2, !grepl("inj", AurumBlockers2$Term.from.EMIS, ignore.case=TRUE))

AurumBlockers2$Group<-case_when(grepl("Aluzine|Berkozide|Delvas|Decaserpyl|Diuresal|Dryptal|Esidrex|Enduron|Frusemide|Hydrenox|Lasipressin|Meprobamate|Natramid|Bendromax|Normetic|Opumide|Serpasil|Saluric|Xuret|Capto-co|hydromet|Hypertane", AurumBlockers2$Term.from.EMIS, ignore.case=TRUE) ~ "Diuretic",
                          grepl("Apsolol|Betadur|Berkolol|Blocadren|Cardinol|Lopranol|Mepranix|Metoros|Oxyprenix|Probeta|Propanix|slow|Sotazide|Spiroprop|Totamol|Tolerzide|Vasaten|Atenamin", AurumBlockers2$Term.from.EMIS, ignore.case=TRUE) ~ "B-blocker",
                          grepl("Diatensec|Spiroctan|Spiro|Spiretic|Spirolone", AurumBlockers2$Term.from.EMIS, ignore.case=TRUE) ~ "Spironolactone",
                          TRUE ~ "Check")

AurumBlockers<-rbind(AurumBlockers, AurumBlockers2)

####Run aurum one more time
AurumBlockersLab1<-unique(as.list((word(AurumBlockers$ProductName, 1)), 
                             (word(AurumBlockers$Term.from.EMIS, 1)), 
                             (word(AurumBlockers$DrugSubstanceName, 1))))

AurumBlockersLab1<-subset(AurumBlockersLab1, AurumBlockersLab1!= "")
AurumBlockers1<-subset(AurumProd, !(ProdCodeId %in% AurumBlockers$ProdCodeId))
AurumBlockers1<-subset(AurumBlockers1, (grepl(paste(AurumBlockersLab1, collapse='|'), ProductName, ignore.case=TRUE))|
                    (grepl(paste(AurumBlockersLab1, collapse='|'), Term.from.EMIS, ignore.case=TRUE))|
                    (grepl(paste(AurumBlockersLab1, collapse='|'), DrugSubstanceName, ignore.case=TRUE)))

AurumBlockers1<-subset(AurumBlockers1, DrugSubstanceName=="")

AurumBlockers1<-AurumBlockers1%>%
  mutate(Group = case_when(grepl("Halfan|eye|suture|Enalapril|protect|boot|choc|paste|Perindopril|skinnies|strip|panty|injection|navidrex", Term.from.EMIS, ignore.case=TRUE) ~ "Not",
                         TRUE ~ ""))%>%
  subset(Group!="Not")

WriteCodeList(AurumBlockers, "Codelists/BlockersDiureticsAurum.txt")

