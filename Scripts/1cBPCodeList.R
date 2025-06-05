#Clear environment
rm(list = ls(all.names = TRUE))

#Call start script
source("./Scripts/1aSetUp.R")

####Load BP lists from previous work####
BPGold<-ReadGoldMedCodelist(paste0(OldCodeListDir, "/Blood pressure/NaomiBPGoldFinalScreenAndValue.txt"))
BPAurum<-ReadAurumMedCodelist(paste0(OldCodeListDir, "/Blood pressure/NaomiBPAurumFinalScreenAndValue.txt"))

#Look ups
AurumMed<-ReadAurumMedCodelist(paste0(DataDir, "/LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt"))
GoldMed<-ReadGoldMedCodelist(paste0(DataDir, "/LookUps/202303_Lookups_CPRDGOLD/medical.txt"))
                    
AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)
GoldMed$desc<-tolower(GoldMed$desc)

####Check for new BP codes Aurum####
NewBPAurum<-subset(AurumMed, !(MedCodeId %in% BPAurum$MedCodeId))

#Checked codes for blood pressure or bp but none give definite diagnosis
NewBPAurum<-NewBPAurum%>%
  mutate(BP = case_when(grepl("ocular|cardi|asys|bruit|pulmonary|subpopulation|abcd|index|adverse|albuminuria in pregnancy|chads2|duration|not indicated|declined|bpi", Term, ignore.case=TRUE) ~0,
                        grepl("murmur|ventri|intracranial|score|bph|bpc|bpm|bpp|bps|bpv|unrecordable|centiles|heart|back|burn|diastole|dysfunction|phase", Term, ignore.case=TRUE) ~0,
                        grepl("elevated blood-pressure", Term, ignore.case=TRUE) ~1,
                        grepl("family|fh|gbp|pulse|level|surgery|procedure|hypothermia|iatrogenic|without hypertension", Term, ignore.case=TRUE) ~0,
                        grepl("clinical|neonatal|no |renovascular hypertension|renal|portal|paris|surgical|bacterial|sepsis|stxbp1|subp|systole|valve|ejection|target|venous", Term, ignore.case=TRUE) ~0,
                        grepl("eye|retin|enceph|poison|overdose|nephropathy", Term, ignore.case=TRUE) ~0,
                        MedCodeId=="5166511000006111"|MedCodeId=="4201871000006115" ~ 0,
                        grepl("BP|blood pressure|hypertens|hypotens|systol|diastol", Term, ignore.case=TRUE) ~1,
                        TRUE ~ 0))%>%
  subset(BP==1)

BPAurum<-select(BPAurum, MedCodeId, CleansedReadCode, Term)
NewBPAurum<-select(NewBPAurum, MedCodeId, CleansedReadCode, Term)
BPAurum<-rbind(BPAurum, NewBPAurum)

#Final check
Other<-AurumMed%>%
  subset(!(MedCodeId %in% BPAurum$MedCodeId) & grepl("hypert|hypot", Term, ignore.case=TRUE))%>%
  mutate(BP = case_when(grepl("ocular|cardi|asys|bruit|pulmonary|subpopulation|abcd|index|adverse|albuminuria in pregnancy|chads2|duration|not indicated|declined|bpi", Term, ignore.case=TRUE) ~0,
                        grepl("murmur|ventri|intracranial|score|bph|bpc|bpm|bpp|bps|bpv|unrecordable|centiles|heart|back|burn|diastole|dysfunction|phase", Term, ignore.case=TRUE) ~0,
                        grepl("elevated blood-pressure", Term, ignore.case=TRUE) ~1,
                        grepl("family|fh|gbp|pulse|level|surgery|procedure|hypothermia|iatrogenic|without hypertension", Term, ignore.case=TRUE) ~0,
                        grepl("clinical|neonatal|no |renovascular hypertension|renal|portal|paris|surgical|bacterial|sepsis|stxbp1|subp|systole|valve|ejection|target|venous", Term, ignore.case=TRUE) ~0,
                        grepl("eye|retin|enceph|poison|overdose|nephropathy|hyperty|hypertel|hypotel|hypertr|hypotr|hypoth|hyperth|hyperto|hypoto", Term, ignore.case=TRUE) ~0,
                        MedCodeId=="5166511000006111"|MedCodeId=="4201871000006115" ~ 0,
                        TRUE ~ 1))%>%
  subset(BP==1)

Other<-select(Other, MedCodeId, CleansedReadCode, Term)
BPAurum<-rbind(BPAurum, Other)

#Remove two that appear to be af screening and post op or postural drop
BPAurum<-subset(BPAurum, !(grepl("fetal|pulm|af screen|atrial fibrillation screen|postoperative|unrecordable|height|renal|heart", Term, ignore.case=TRUE)))

#Code up what sort of code
BPAurum<-BPAurum%>%
  mutate(Type = case_when(MedCodeId == "3636695013"|MedCodeId == "164121000006118"|MedCodeId == "790141000006111"|MedCodeId == "461309011" ~ "BP",
                          MedCodeId == "789981000006111" ~ "admin",
                          MedCodeId == "404471000006113" ~ "therapy",
                          grepl("referral", Term, ignore.case=TRUE) ~"referral", 
                         grepl("maternal|gest|child|preg|obstet|partum|eclampsia", Term, ignore.case=TRUE) ~"pregnancy",
                         grepl("secondary|cause|induce|due", Term, ignore.case=TRUE) & !(grepl("due to hypertension", Term, ignore.case=TRUE))~"secondary",
                          grepl("did not attend|cuff size|invit|screen|loan|return|recorder|requests|offer|letter|verbal|admin|monitoring call|monitoring status|default", Term, ignore.case=TRUE) ~"admin",                                   
                         grepl("therapy|drugs|treatm|antihypertensive", Term, ignore.case=TRUE)& !(grepl("reading", Term, ignore.case=TRUE)) ~"therapy", 
                         grepl("histo|\\h/o|resolved", Term, ignore.case=TRUE) ~"history", 
                         grepl("except", Term, ignore.case=TRUE) ~"exceptions", 
                         grepl("reminder|leaflet|understand|seen|review|advice|clinic|attend|inform|educat|recommend|telehealth", Term, ignore.case=TRUE) & !grepl("severe", Term, ignore.case=TRUE) ~"seen", 
                         TRUE ~ "BP"))
                          
#Code up the information - disease or recording or monitoring?
BPAurum<-BPAurum%>%
  mutate(Hypertension = case_when(grepl("hypertension drugs", Term, ignore.case=TRUE) ~ "hypertension",
                                  MedCodeId == "789981000006111" ~ "no",
                                  MedCodeId == "39871000006113" ~ "hypertension",
                                  grepl("highest|not to have|hypertension risk", Term, ignore.case=TRUE) ~ "no",
                                  Type =="admin" & grepl("screen|remote|cuff size|loan|return|bp|blood pressure|recorder", Term, ignore.case=TRUE) ~ "no",
                                  Type =="admin" ~ "hypertension",
                                  grepl("abnormal|bp abn", Term, ignore.case=TRUE) |(grepl("high", Term, ignore.case=TRUE)& grepl("low", Term, ignore.case=TRUE)) ~ "abnormal bp",
                                  grepl("elevated|white coat|high|raised", Term, ignore.case=TRUE) ~ "high bp",
                                  grepl("normal|no postural", Term, ignore.case=TRUE) ~ "normal bp",
                                  grepl("postural|orthostatic", Term, ignore.case=TRUE) ~ "postural drop",
                                  grepl("low|hypo", Term, ignore.case=TRUE) & !grepl("follow", Term, ignore.case=TRUE)  ~ "low bp",
                                  grepl("borderline", Term, ignore.case=TRUE) ~ "abnormal bp",
                                  grepl("suspect|referral to hypertension clinic|decide", Term, ignore.case=TRUE) ~ "suspected hypertension",
                                  grepl("hyperten|renin", Term, ignore.case=TRUE) & !grepl("screen|not required", Term, ignore.case=TRUE)~ "hypertension",
                                  TRUE ~ "no"))

Check<-subset(BPAurum, MedCodeId %in% Other$MedCodeId)

####Check for new BP codes Gold####
NewBPGold<-subset(GoldMed, !(medcode %in% BPGold$medcode))

#Checked codes for blood pressure or bp but none give definite diagnosis
NewBPGold<-NewBPGold%>%
  mutate(BP = case_when(grepl("eye|retin|enceph|poison|ocular|cardi|asys|bruit|pulmonary|subpopulation|abcd|index|adverse|albuminuria in pregnancy|chads2|duration|not indicated|declined|bpi", desc, ignore.case=TRUE) ~0,
                        grepl("murmur|ventri|intracranial|score|bph|bpc|bpm|bpp|bps|bpv|unrecordable|centiles|heart|back|burn|diastole|dysfunction|phase", desc, ignore.case=TRUE) ~0,
                        grepl("elevated blood-pressure", desc, ignore.case=TRUE) ~1,
                        grepl("bpr|family|fh|gbp|pulse|level|surgery|procedure|hypothermia|iatrogenic|without hypertension", desc, ignore.case=TRUE) ~0,
                        grepl("clinical|neonatal|no |renovascular hypertension|renal|portal|paris|surgical|bacterial|sepsis|stxbp1|subp|systole|valve|ejection|target|venous", desc, ignore.case=TRUE) ~0,
                        grepl("BP|blood pressure|hypertens|hypotens|systol|diastol", desc, ignore.case=TRUE) ~1,
                        TRUE ~ 0))%>%
  subset(BP==1)

Check<-subset(NewBPGold, grepl("hypertens|hypotens", desc, ignore.case=TRUE)& !(grepl("hypertension|hypotension", desc, ignore.case=TRUE)))

BPGold<-select(BPGold, medcode, readcode, desc=readterm)
NewBPGold<-select(NewBPGold, medcode, readcode, desc)
BPGold<-rbind(BPGold, NewBPGold)

#One more time

Other<-GoldMed%>%
  subset(!(medcode %in% BPGold$medcode) & grepl("hypert|hypot", desc, ignore.case=TRUE))%>%
  mutate(BP = case_when(grepl("eye|retin|enceph|poison|ocular|cardi|asys|bruit|pulmonary|subpopulation|abcd|index|adverse|albuminuria in pregnancy|chads2|duration|not indicated|declined|bpi", desc, ignore.case=TRUE) ~0,
                        grepl("murmur|ventri|intracranial|score|bph|bpc|bpm|bpp|bps|bpv|unrecordable|centiles|heart|back|burn|diastole|dysfunction|phase", desc, ignore.case=TRUE) ~0,
                        grepl("elevated blood-pressure", desc, ignore.case=TRUE) ~0,
                        grepl("bpr|family|fh|gbp|pulse|level|surgery|procedure|hypothermia|iatrogenic|without hypertension", desc, ignore.case=TRUE) ~0,
                        grepl("eye|retin|enceph|poison|overdose|nephropathy|hyperty|hypertel|hypotel|hypertr|hypotr|hypoth|hyperth|hyperto|hypoto|clinical|neonatal|no |renovascular hypertension|renal|portal|paris|surgical|bacterial|sepsis|stxbp1|subp|systole|valve|ejection|target|venous", desc, ignore.case=TRUE) ~0,
                        TRUE ~ 1))%>%
  subset(BP==1)

Other<-select(Other, medcode, readcode, desc)
BPGold<-rbind(BPGold, Other)

#Remove two that appear to be af screening and post op
BPGold<-subset(BPGold, !(grepl("pulm|unrecordable|height|renal dis|heart", desc, ignore.case=TRUE)))

#Code up what sort of code
BPGold<-BPGold%>%
  mutate(Type = case_when(medcode=="63164" ~ "therapy",
    grepl("maternal|gest|child|preg|obstet|partum|eclampsia", desc, ignore.case=TRUE) ~"pregnancy",
                          grepl("secondary|cause|induce|due", desc, ignore.case=TRUE) & !(grepl("due to hypertension", desc, ignore.case=TRUE))~"secondary",
                          grepl("did not attend|cuff size|invit|screen|loan|return|recorder|requests|offer|letter|verbal|admin|monitoring call|monitoring status|default", desc, ignore.case=TRUE) ~"admin",                                   
                          grepl("therapy|drugs|treatm", desc, ignore.case=TRUE)& !(grepl("reading", desc, ignore.case=TRUE)) ~"therapy", 
                          grepl("histo|\\h/o|resolved", desc, ignore.case=TRUE) ~"history", 
                          grepl("except", desc, ignore.case=TRUE) ~"exceptions", 
                          grepl("refer", desc, ignore.case=TRUE) ~"referal",
                          grepl("reminder|leaflet|understand|seen|review|advice|clinic|attend|inform|educat|recommend|telehealth", desc, ignore.case=TRUE) & !grepl("severe", desc, ignore.case=TRUE) ~"seen", 
                          TRUE ~ "BP"))

#Code up the information - disease or recording or monitoring?
BPGold<-BPGold%>%
  mutate(Hypertension = case_when(medcode=="97781" ~ "hypertension",
    grepl("hypertension drugs", desc, ignore.case=TRUE) ~ "hypertension",
    grepl("highest|not to have|hypertension risk", desc, ignore.case=TRUE) ~ "no",
                                  Type =="admin" & grepl("screen|remote|cuff size|loan|return|bp|blood pressure|recorder", desc, ignore.case=TRUE) ~ "no",
                                  Type =="admin" ~ "hypertension",
                                  grepl("abnormal|bp abn", desc, ignore.case=TRUE) |(grepl("high", desc, ignore.case=TRUE)& grepl("low", desc, ignore.case=TRUE)) ~ "abnormal bp",
                                  grepl("elevated|white coat|high|raised", desc, ignore.case=TRUE) ~ "high bp",
                                  grepl("normal|no postural", desc, ignore.case=TRUE) ~ "normal bp",
                                  grepl("postural|orthostatic", desc, ignore.case=TRUE) ~ "postural drop",
                                  grepl("low|hypo", desc, ignore.case=TRUE) & !grepl("follow", desc, ignore.case=TRUE)  ~ "low bp",
                                  grepl("borderline", desc, ignore.case=TRUE) ~ "abnormal bp",
                                  grepl("suspect|referral to hypertension clinic|decide", desc, ignore.case=TRUE) ~ "suspected hypertension",
                                  grepl("hyperten|renin", desc, ignore.case=TRUE) & !grepl("screen|not required", desc, ignore.case=TRUE)~ "hypertension",
                                  TRUE ~ "no"))

Check<-subset(BPGold, medcode %in% Check$medcode)
Check<-subset(BPGold, medcode %in% Other$medcode)

####Compare Gold and Aurum####
BPAurum$Term<-tolower(BPAurum$Term)
BPAurum$Term<-gsub('"', '', BPAurum$Term)
BPGold$desc<-tolower(BPGold$desc)

GoldBPCheck<-subset(BPGold, !(desc %in% BPAurum$Term)& desc!="")
AddtoAurumT<-subset(AurumMed, Term %in% GoldBPCheck$desc) #none to add

AurumBPCheck<-subset(BPAurum, !(Term %in% BPGold$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumBPCheck$Term) #None to add on term

GoldBPCheck<-subset(BPGold, !(readcode %in% BPAurum$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldBPCheck$readcode) #None to add

AurumBPCheck<-subset(BPAurum, !(CleansedReadCode %in% BPGold$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumBPCheck$CleansedReadCode) #4 to add on readcode but all there

####Save final lists####
WriteCodeList(BPAurum, "Codelists/BPAurum.txt")
WriteCodeList(BPGold, "Codelists/BPGold.txt")
