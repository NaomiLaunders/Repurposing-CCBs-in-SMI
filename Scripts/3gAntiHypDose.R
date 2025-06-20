####Dose at first oral anithypertensive prescription for trial 1 & 2 (or if blank within 6 months)####

#Call start script
source("./Scripts/1aSetUp.R")

load(paste0(ProjectDir, "/Data/Hospselect.rdata"))

load(paste0(ProjectDir, "/Extracts/AllCCBs.Rdata"))

AllCCB<-subset(AllCCB, patid %in% HospAll$patid)
length(unique(AllCCB$patid))

#Check route
table(AllCCB$Route, useNA="ifany")

#Select the prescription that are for our patients
Dates<-select(HospAll, patid, enter, end)
AllCCB<-merge(x=AllCCB, y=Dates, by="patid", all.x=TRUE, all.y=FALSE)

CCB<-subset(AllCCB, issuedate>=(enter) & issuedate<=end)

##Only need to have dose for the ones I am interested in which is just mono or "ever exclusions".
table(CCB$Group, CCB$Class)
table(CCB$Group, CCB$Route)
CCB<-subset(CCB, Group=="Mono"|Group=="ever exclusion")

#Check which ones have dosage information. If no dosage information look at next one

#No PRN
table(CCB$dosage_text, useNA="ifany")

CCB$PRN<-case_when(grepl("YEARLY BLOOD TESTS REQUIRED", CCB$dosage_text, ignore.case=TRUE) ~ 0,
                   grepl("PRN|REQ|NECESSARY|MDU|UNKNOWN|NEEDED", CCB$dosage_text, ignore.case=TRUE) ~1,
                        TRUE ~0)
table(CCB$PRN)
table(CCB$dosage_text[CCB$PRN==1])
table(CCB$Class[CCB$PRN==1])

Check<-subset(CCB, PRN==1)

#Remove PRN
CCB<-subset(CCB, PRN==0)

#Remove those not issued
CCB$NotIssued<-case_when(grepl("not issue", CCB$dosage_text, ignore.case=TRUE) ~1,
                          TRUE ~0)

Check<-subset(CCB, NotIssued==1)
CCB<-subset(CCB, NotIssued==0)
CCB<-select(CCB, -NotIssued)
#Check daily dose
table(CCB$daily_dose, useNA="ifany")
DD<-subset(CCB, is.na(CCB$daily_dose)|CCB$daily_dose==0) 

#Daily dose from text
DD<-subset(DD, !is.na(dosage_text))
DD<-select(DD, dosage_text)
DD<-distinct(DD)

DD$Quantity<-0
DD$Quantity[DD$dosage_text=="28"]<-28
DD$Quantity[DD$dosage_text=="30"]<-30

DD$dose<-0
DD$dose<-case_when(DD$dosage_text=="1 THREE A DAY" ~ 3,
                   DD$dosage_text=="14" ~ 14,
                   DD$dosage_text=="13" ~ 13,
                   DD$dosage_text=="12" ~ 12,
                   DD$dosage_text=="10 D" ~ 10,
                   DD$dosage_text=="10 MLS"|DD$dosage_text=="10" ~ 10,
                   DD$dosage_text=="5-10MLS"|DD$dosage_text=="5-10MLTDSPR" ~ 7.5,
                   DD$dosage_text=="5ML A DAY"|DD$dosage_text=="50D"|DD$dosage_text=="FIVE" ~ 5,
                   grepl("1.5", DD$dosage_text) | DD$dosage_text=="1 OR 2"|DD$dosage_text=="ONE AND A H"|	DD$dosage_text=="ONE AND A HALF"|
                     DD$dosage_text=="1 AND HALF"|DD$dosage_text=="TAKE ONE AND A HALF"|DD$dosage_text=="1-2"|DD$dosage_text=="1 AND HALF"| DD$dosage_text=="11/2"|
                     DD$dosage_text=="TAKE ONE OR TWO"|DD$dosage_text=="ONE AND HALF"|	
                     DD$dosage_text=="ONE OR TWO" ~ 1.5,
                   DD$dosage_text=="1000" |DD$dosage_text=="1 MONTH" |DD$dosage_text=="0.1"|DD$dosage_text=="FP34"|DD$dosage_text=="120"|DD$dosage_text==":[100]"|
                     DD$dosage_text=="120"|DD$dosage_text=="180" |DD$dosage_text=="AUTH=12"|DD$dosage_text=="100"|DD$dosage_text=="30"|DD$dosage_text=="38"|DD$dosage_text=="28"~ 0,
                   grepl("DAYS|NURSE", DD$dosage_text, ignore.case=TRUE) ~ 0,
                   DD$dosage_text=="400MG" ~ 400,
                   DD$dosage_text=="200MG" ~ 200,
                   DD$dosage_text=="100MG" ~ 100,
                   DD$dosage_text=="10MG" ~ 10,
                   DD$dosage_text=="10 MG" ~ 10,
                   DD$dosage_text=="5MG" ~ 5,
                   DD$dosage_text=="300MG" ~ 300,
                   DD$dosage_text=="15MG" ~ 15,
                   DD$dosage_text=="0.5MG" ~ 0.5,
                   DD$dosage_text=="0.5 MG" ~ 0.5,
                   DD$dosage_text=="5ML" ~ 5,
                   DD$dosage_text=="5 ML" ~ 5,
                   DD$dosage_text=="150 MG" ~ 150,
                   DD$dosage_text=="0.1" ~ 0.1,
                   DD$dosage_text=="1 ML" ~ 1,
                   DD$dosage_text=="20  MG" ~ 20,
                   DD$dosage_text=="2.5ML" ~ 2.5,
                   DD$dosage_text=="2.5 ML" ~ 2.5,
                   DD$dosage_text=="20MG A DAY" ~ 20,
                   DD$dosage_text=="10MGS AT NIGHT" ~ 10,
                   DD$dosage_text=="1 EOD"| DD$dosage_text=="HALF A TABL"|DD$dosage_text==".5"|DD$dosage_text=="HALF A TAB"|DD$dosage_text=="HALF A TABLET"|
                     DD$dosage_text=="1/2" ~ 0.5,
                   DD$dosage_text=="HALF TABLET"|DD$dosage_text=="0.5"|DD$dosage_text=="HALF TAB DA"|DD$dosage_text=="ONE HALF A DAY" ~ 0.5,
                   DD$dosage_text=="2.5"|DD$dosage_text=="2 1/2"|DD$dosage_text=="TWO AND HALF" ~ 2.5,
                   DD$dosage_text=="TAKE ONE 3 A DAY"|DD$dosage_text=="1 TSD"| DD$dosage_text=="TAKE ONE TREE TIMES A DAY"~ 3,
                   DD$dosage_text=="1 TWICE"|DD$dosage_text=="IBD"|DD$dosage_text=="1 BDAY" |DD$dosage_text=="TWICW A DAY"|DD$dosage_text=="TWICE"~ 2,
                   DD$dosage_text=="2.5"| DD$dosage_text=="DB"|DD$dosage_text=="2 MONE" |DD$dosage_text=="1DB"~ 2,
                   grepl("ONE THREE|TD|THREE TIME|TOD", DD$dosage_text, ignore.case=TRUE) ~ 3,
                   grepl("ONE Tw", DD$dosage_text, ignore.case=TRUE) ~ 2,
                   DD$dosage_text=="2 THREE A DAY"|DD$dosage_text=="6 D" |DD$dosage_text=="6"~ 6,
                   DD$dosage_text=="ION"| DD$dosage_text=="AM"|DD$dosage_text=="MORNE"|DD$dosage_text=="HALF TWICE"|DD$dosage_text=="OD"|DD$dosage_text=="I O.D."  |DD$dosage_text=="ODS"~ 1,
                   DD$dosage_text=="NIGHTLY"| DD$dosage_text=="DAY"|DD$dosage_text=="O"|DD$dosage_text=="IOD"|DD$dosage_text=="AD"|DD$dosage_text=="A DAY"|DD$dosage_text=="TAKE A DAY"|DD$dosage_text=="0NE A DAY"~ 1,
                   grepl("1|ONE|ONCE|PM", DD$dosage_text) ~ 1,
                   grepl("2|TWO", DD$dosage_text) ~ 2,
                   grepl("3|THREE", DD$dosage_text) ~ 3,
                   grepl("4|FOUR", DD$dosage_text) ~ 4,
                   DD$dosage_text=="ID"|DD$dosage_text=="INOCTE" ~1,
                   TRUE ~ 0)

Check<-subset(DD, dose==0)
table(Check$dosage_text)

DD$Unit<-""
DD$Unit[grepl("mg", DD$dosage_text, ignore.case=TRUE)]<-"MG" 
DD$Unit[grepl("ml", DD$dosage_text, ignore.case=TRUE)]<-"ML" 

#Replace fields in main table if missing
CCB<-merge(x=CCB, y=DD, by="dosage_text", all.x=TRUE, all.y=FALSE)
CCB$daily_dose[is.na(CCB$daily_dose) | CCB$daily_dose==0] <-CCB$dose[is.na(CCB$daily_dose) | CCB$daily_dose==0]
CCB$dose_unit[is.na(CCB$dose_unit) | CCB$dose_unit==0 ]<-CCB$Unit[is.na(CCB$dose_unit) | CCB$dose_unit==0 ]
CCB$quantity[is.na(CCB$quantity) | CCB$quantity==0 ]<-CCB$Quantity[is.na(CCB$quantity) | CCB$quantity==0 ]

CCB<-select(CCB, -dose, -Unit)
table(CCB$daily_dose, useNA="ifany")

#Can we get missing strength from text?

length(which(CCB$strength==""))

ST<-subset(CCB, strength==""|is.na(strength)|strength==0) 

ST<-subset(ST, !is.na(Term.from.EMIS))
ST<-select(ST, Term.from.EMIS)
ST<-distinct(ST)
ST$MG<-0
ST$ML<-0

ST<-ST%>%
  mutate(MG=case_when(grepl("\\d+([.,]\\d+)+mg", Term.from.EMIS, ignore.case=TRUE) ~ as.numeric(str_extract(Term.from.EMIS, "\\d+([.,]\\d+)(?=mg)")),
                      grepl("\\d+mg", Term.from.EMIS, ignore.case=TRUE) ~ as.numeric(str_extract(Term.from.EMIS, "\\d+(?=mg)")),
                      grepl("\\d+([.,]\\d+)+[[:space:]]+mg", Term.from.EMIS, ignore.case=TRUE) ~ as.numeric(str_extract(Term.from.EMIS, "\\d+([.,]\\d+)")),
                      grepl(" mg", Term.from.EMIS, ignore.case=TRUE) ~ as.numeric(str_extract(Term.from.EMIS, "\\d+")),
                      grepl("microgram", Term.from.EMIS, ignore.case=TRUE) ~ (as.numeric(str_extract(Term.from.EMIS, "\\d+([.,]\\d+)(?=microgram)")))/1000,
                      grepl("microgram", Term.from.EMIS, ignore.case=TRUE) ~ (as.numeric(str_extract(Term.from.EMIS, "\\d+(?=microgram)")))/1000,
                      grepl("g", Term.from.EMIS, ignore.case=TRUE) ~ (as.numeric(str_extract(Term.from.EMIS, "\\d+([.,]\\d+)(?=g)")))*1000,
                      grepl("g", Term.from.EMIS, ignore.case=TRUE) ~ (as.numeric(str_extract(Term.from.EMIS, "\\d+(?=g)")))*1000,
                      TRUE ~ as.numeric(str_extract(Term.from.EMIS, "\\d+"))),
         ML=case_when(grepl("5 ml", Term.from.EMIS, ignore.case=TRUE) ~ 5,
                      grepl(" ml", Term.from.EMIS, ignore.case=TRUE) ~ 1,                                                   
                      TRUE ~ 0))

ST<-ST%>%
  mutate(MG = case_when(grepl("titration", Term.from.EMIS, ignore.case=TRUE)~ 2.5,
                        TRUE ~ MG))

#COnvert everything to MG/ML for ease
ST<-ST%>%
  mutate(ST=case_when(ML==0 ~ MG,
                      TRUE ~ MG/ML))

CCB<-merge(x=CCB, y=ST, by="Term.from.EMIS", all.x=TRUE, all.y=FALSE)

#Check strength and convert to mg
#If we have strength take it from there
table(CCB$strength, useNA="ifany")
CCB$StrengthNew<-as.numeric(str_extract(CCB$strength, "\\d+\\.*\\d*"))

table(CCB$StrengthNew, useNA="ifany")
table(CCB$strength, useNA="ifany")

Check<-select(CCB, strength, StrengthNew)
Check<-subset(Check, !is.na(strength))
Check<-distinct(Check)

CCB$StrengthNew[is.na(CCB$strength)|CCB$strength==0|CCB$strength==""]<-CCB$ST[is.na(CCB$strength)|CCB$strength==0|CCB$strength==""]
CCB<-select(CCB, -ST, -MG, -ML)

table(CCB$strength)

Check<-as.data.frame(table(CCB$StrengthNew, CCB$strength,useNA="ifany"))
Check<-subset(Check, Freq!=0)

CCB$StrengthNew<-case_when(grepl("microgram",CCB$strength, ignore.case=TRUE) ~ CCB$StrengthNew/1000,
                                grepl("gram", CCB$strength, ignore.case=TRUE) ~ CCB$StrengthNew*1000,
                                grepl("5ml",CCB$strength, ignore.case=TRUE) ~ CCB$StrengthNew/5,
                                grepl("\\+",CCB$strength, ignore.case=TRUE) & grepl("20",CCB$strength, ignore.case=TRUE) ~ 30,
                            grepl("\\+",CCB$strength, ignore.case=TRUE) & grepl("40",CCB$strength, ignore.case=TRUE) ~ 50,
                            grepl("\\+",CCB$strength, ignore.case=TRUE) & grepl("80",CCB$strength, ignore.case=TRUE) ~ 90,
                                TRUE ~ CCB$StrengthNew)
#Calculate daily dose - prn will be set to zero
CCB<-CCB%>%
  mutate(DailyDose=case_when(dose_unit=="ML" ~ daily_dose*StrengthNew,
                             dose_unit=="MG" ~ daily_dose,
                             TRUE ~ daily_dose*StrengthNew))

table(CCB$DailyDose, useNA="ifany")

Check<-subset(CCB, DailyDose==0 | is.na(DailyDose))  

#Check duration and quantity
Quant<-subset(CCB, dose_unit=="ML")
table(CCB$quantity[is.na(CCB$DailyDose)])
CCB$quantity<-case_when(CCB$quantity<0 ~ CCB$quantity*-1, TRUE ~ CCB$quantity)
Check<-subset(CCB, quantity>1000)#Most of the highest ones are in ML

table(CCB$duration)
CCB$duration<-case_when(CCB$duration<0 ~ CCB$duration*-1, TRUE ~ CCB$duration)
length(which(CCB$duration>366))

CCB$duration<-case_when(CCB$duration>366 ~ 0, TRUE ~ CCB$duration)
table(CCB$duration)

#Because I am only interested in those at right dosage, calculate duration to next prescription
Dur<-CCB%>%
  group_by(patid)%>%
  arrange(issuedate)%>%
  mutate(TimeToNext=lead(issuedate)-issuedate)

Dur$TimeToNext<-as.numeric(Dur$TimeToNext)

Dur<-Dur%>%
  mutate(TimeToNext=case_when(is.na(TimeToNext) ~ as.numeric(end-issuedate),
                              TRUE ~ TimeToNext))

#If time to next is zero set it to be the next time to next
Zeros<-Dur%>%
  subset(TimeToNext==0)%>%
  group_by(patid)%>%
  summarise(Zero=n())

#If its zero take  the biggest on that day
Dur<-Dur%>%
  mutate(patdate=paste0(patid, issuedate))%>%
  group_by(patdate)%>%
  mutate(TimeToNext=case_when(TimeToNext==0 ~ max(TimeToNext),
                              TRUE ~ TimeToNext))

length(which(Dur$TimeToNext==0))

#Are these zero because they are at the same time as end?
length(which(Dur$TimeToNext==0&Dur$issuedate==Dur$end)) # Yes they are - leave them in as they wont be the ones we need anyway, and coverage will be set to NA

#If daily dose is unknown take quantity by duration

#If quantity is 1 and duration is reasonable, assume it's one per day
table(Dur$duration[Dur$quantity==1])

Dur$Newquantity<-case_when(Dur$quantity==1 & is.na(Dur$DailyDose) & Dur$duration>=7 ~ Dur$duration,
                           TRUE ~ NA)

#If daily dose is unknown take quantity by duration
Dur<-Dur%>%
  ungroup()%>%
  mutate(CalcDose=case_when(quantity>0 & duration>0 ~ (quantity/duration)*StrengthNew,
                            TRUE ~ NA_real_),
         CalcDose2=case_when(Newquantity>0 & duration>0 ~ (Newquantity/duration)*StrengthNew,
                             TRUE ~ NA_real_),
         Coverage=case_when(quantity>0 & TimeToNext>0~ (quantity/TimeToNext)*StrengthNew,
                            TRUE ~ NA_real_),
         Coverage2=case_when(Newquantity>0 & TimeToNext>0~ (Newquantity/TimeToNext)*StrengthNew,
                             TRUE ~ NA_real_))


length(which(Dur$duration>112))
length(which(Dur$TimeToNext>112))
length(which(Dur$quantity>1000))

Dur$DailyDose[Dur$DailyDose==0]<-NA
Dur$Coverage[Dur$TimeToNext>366]<-NA

summary(Dur$DailyDose)
summary(Dur$Coverage)

Dur<-Dur%>%
  group_by(patdate, Name)%>%
  mutate(TotalDose1=sum(DailyDose, na.rm=TRUE), TotalDose2=sum(CalcDose, na.rm=TRUE),TotalDose3=sum(CalcDose2, na.rm=TRUE),
         TotalCoverage1=sum(Coverage), TotalCoverage2=sum(Coverage2))%>%
  ungroup()



####Valid dose####
##ARB##
# Azilsartan: low: <40 (below maintenance)
# Azilsartan: med:40- <80 (maintenance)
# Azilsartan:  hi: 80+ (Max)

# Candesartan: low: <8 (below maintenance)
# Candesartan: med: 8 - <32 (maintenance)
# Candesartan:  hi: 32+ (Max)

# Eprosartan: low: <600 (below maintenance)
# Eprosartan: med:600 (maintenance)
# Eprosartan:  hi: >600 (Max)

# Irbesartan: low: <150 (below maintenance)
# Irbesartan: med:150 - <300 (maintenance)
# Irbesartan:  hi: 300+ (Max)

# Losartan: low: <50 (below maintenance)
# Losartan: med:50- <100 (maintenance)
# Losartan:  hi: 100+ (Max)

# Olmesartan: low: <20 (below maintenance)
# Olmesartan: med:20- <40 (maintenance)
# Olmesartan:  hi: 40+ (Max)

# Telmisartan: low: <40 (below maintenance)
# Telmisartan: med:40- <80 (maintenance)
# Telmisartan:  hi: 80+ (Max)

# Valsartan: low: <80 (below maintenance)
# Valsartan: med:80- 160 (maintenance)
# Valsartan:  hi: >160 - 320 (Max)

##CCB##
# Amlodipine: low: <5 (below maintenance)
# Amlodipine: med:5-10 (maintenance)
# Amlodipine:  hi: 10+ (Max)

# Felodipine: low: <5 (below maintenance)
# Felodipine: med:5-10 (maintenance)
# Felodipine:  hi: >10 - 20 (Max)

# Lacidipine: low: <2 (below maintenance)
# Lacidipine: med:2-6 (maintenance)
# Lacidipine:  hi: 6+ (Max)

# Lercanidipine: low: <10 (below maintenance)
# Lercanidipine: med: 10-20 (maintenance)
# Lercanidipine:  hi: 20+ (Max)

# Nicardipine: low: <60 (below maintenance)
# Nicardipine: med:60-120 (maintenance)
# Nicardipine:  hi: 120+ (Max)

# Nifedipine: low: <30 (below maintenance)
# Nifedipine: med:30-90 (maintenance)
# Nifedipine:  hi: 90+ (Max)

length(unique(Dur$patid))
Dur<-subset(Dur, patid %in% HospAll$patid)

#Is the  CCB prescription a valid daily dose?
table(Dur$Name)

Dur<-Dur%>%
  group_by(patdate, Name)%>%
  mutate(n=n())

#Set it at 20% above or below limit
#Use: https://cks.nice.org.uk/topics/hypertension/prescribing-information/calcium-channel-blockers/
#https://www.drugs.com/pro/isradipine.html#s-34068-7
#https://www.drugs.com/dosage/nisoldipine.html
#https://www.drugs.com/mtm/moexipril.html
#https://www.drugs.com/dosage/benazepril.html


save(Dur, file=paste0(ProjectDir, "/Extracts/CCBDose_interim.Rdata"))

table(Dur$n)
table(Dur$Name, useNA="ifany")

Dur<-Dur%>%
  ungroup()%>%
  mutate(valid=case_when(Name=="Amlodipine" & TotalDose1>=4 & TotalDose1<=12 ~ 1,
                         Name=="Felodipine" & TotalDose1>=2 & TotalDose1<=24~1,
                         Name=="Isradipine" & TotalDose1>=2 & TotalDose1<=24~1,
                         Name=="Lacidipine" & TotalDose1>=1.6 & TotalDose1<=7.2~1,
                         Name=="Lercanidipine" & TotalDose1>=8 & TotalDose1<=24~1,
                         Name=="Nicardipine" & TotalDose1>=48 & TotalDose1<=144~1,
                         Name=="Nifedipine" & TotalDose1>=16 & TotalDose1<=108~1,
                         Name=="Nisoldipine" & TotalDose1>=8 & TotalDose1<=72~1,
                         Name=="Verapamil" & TotalDose1>=96 & TotalDose1<=576~1,
                         Name=="Candesartan" & TotalDose1>=3.2 & TotalDose1<=38.4~1,
                         Name=="Eprosartan" & TotalDose1>=480 & TotalDose1<=720~1,
                         Name=="Irbesartan" & TotalDose1>=60 & TotalDose1<=360~1,
                         Name=="Losartan" & TotalDose1>=20 & TotalDose1<=120~1,
                         Name=="Olmesartan" & TotalDose1>=8 & TotalDose1<=48~1,
                         Name=="Telmisartan" & TotalDose1>=16 & TotalDose1<=96~1,
                         Name=="Valsartan" & TotalDose1>=32 & TotalDose1<=384~1,
                         Name=="Benazepril" & TotalDose1>=8 & TotalDose1<=96~1,
                         Name=="Enalapril" & TotalDose1>=2 & TotalDose1<=48~1,
                         Name=="Enalopril" & TotalDose1>=2 & TotalDose1<=48~1,
                         Name=="Moexipril" & TotalDose1>=6 & TotalDose1<=72~1,
                         Name=="Quinapril " & TotalDose1>=2 & TotalDose1<=96~1,
                         Name=="Imidapril " & TotalDose1>=2 & TotalDose1<=24~1,
                         Name=="Captopril" & TotalDose1>=10 & TotalDose1<=180 ~1,
                         Name=="Fosinopril" & TotalDose1>=8 & TotalDose1<=48~1,
                         Name=="Lisinopril" & TotalDose1>=2 & TotalDose1<=96~1,
                         Name=="Ramipril" & TotalDose1>=1 & TotalDose1<=12~1,
                         Name=="Perindopril" & TotalDose1>=1.6 & TotalDose1<=12~1,
                         Name=="Trandolapril" & TotalDose1>=0.4 & TotalDose1<=4.8~1,
                         Name=="Cilazapril" & TotalDose1>=0.4 & TotalDose1<=12~1,
                         
                         Name=="Amlodipine" & TotalDose2>=4 & TotalDose2<=12 ~ 2,
                         Name=="Felodipine" & TotalDose2>=2 & TotalDose2<=24~2,
                         Name=="Isradipine" & TotalDose2>=2 & TotalDose2<=24~2,
                         Name=="Lacidipine" & TotalDose2>=1.6 & TotalDose2<=7.2~2,
                         Name=="Lercanidipine" & TotalDose2>=8 & TotalDose2<=24~2,
                         Name=="Nicardipine" & TotalDose2>=48 & TotalDose2<=144~2,
                         Name=="Nifedipine" & TotalDose2>=16 & TotalDose2<=108~2,
                         Name=="Nisoldipine" & TotalDose2>=8 & TotalDose2<=72~2,
                         Name=="Verapamil" & TotalDose2>=96 & TotalDose2<=576~2,
                         Name=="Candesartan" & TotalDose2>=3.2 & TotalDose2<=38.4~2,
                         Name=="Eprosartan" & TotalDose2>=480 & TotalDose2<=720~2,
                         Name=="Irbesartan" & TotalDose2>=60 & TotalDose2<=360~2,
                         Name=="Losartan" & TotalDose2>=20 & TotalDose2<=120~2,
                         Name=="Olmesartan" & TotalDose2>=8 & TotalDose2<=48~2,
                         Name=="Telmisartan" & TotalDose2>=16 & TotalDose2<=96~2,
                         Name=="Valsartan" & TotalDose2>=32 & TotalDose2<=384~2,
                         Name=="Benazepril" & TotalDose2>=8 & TotalDose2<=96~2,
                         Name=="Enalapril" & TotalDose2>=2 & TotalDose2<=48~2,
                         Name=="Enalopril" & TotalDose2>=2 & TotalDose2<=48~2,
                         Name=="Moexipril" & TotalDose2>=6 & TotalDose2<=72~2,
                         Name=="Quinapril " & TotalDose2>=2 & TotalDose2<=96~2,
                         Name=="Imidapril " & TotalDose2>=2 & TotalDose2<=24~2,
                         Name=="Captopril" & TotalDose2>=10 & TotalDose2<=180 ~2,
                         Name=="Fosinopril" & TotalDose2>=8 & TotalDose2<=48~2,
                         Name=="Lisinopril" & TotalDose2>=2 & TotalDose2<=96~2,
                         Name=="Ramipril" & TotalDose2>=1 & TotalDose2<=12~2,
                         Name=="Perindopril" & TotalDose2>=1.6 & TotalDose2<=12~2,
                         Name=="Trandolapril" & TotalDose2>=0.4 & TotalDose2<=4.8~2,
                         Name=="Cilazapril" & TotalDose2>=0.4 & TotalDose2<=12~2,
                         
                         Name=="Amlodipine" & TotalDose3>=4 & TotalDose3<=12 ~ 3,
                         Name=="Felodipine" & TotalDose3>=2 & TotalDose3<=24~3,
                         Name=="Isradipine" & TotalDose3>=2 & TotalDose3<=24~3,
                         Name=="Lacidipine" & TotalDose3>=1.6 & TotalDose3<=7.2~3,
                         Name=="Lercanidipine" & TotalDose3>=8 & TotalDose3<=24~3,
                         Name=="Nicardipine" & TotalDose3>=48 & TotalDose3<=144~3,
                         Name=="Nifedipine" & TotalDose3>=16 & TotalDose3<=108~3,
                         Name=="Nisoldipine" & TotalDose3>=8 & TotalDose3<=72~3,
                         Name=="Verapamil" & TotalDose3>=96 & TotalDose3<=576~3,
                         Name=="Candesartan" & TotalDose3>=3.2 & TotalDose3<=38.4~3,
                         Name=="Eprosartan" & TotalDose3>=480 & TotalDose3<=720~3,
                         Name=="Irbesartan" & TotalDose3>=60 & TotalDose3<=360~3,
                         Name=="Losartan" & TotalDose3>=20 & TotalDose3<=120~3,
                         Name=="Olmesartan" & TotalDose3>=8 & TotalDose3<=48~3,
                         Name=="Telmisartan" & TotalDose3>=16 & TotalDose3<=96~3,
                         Name=="Valsartan" & TotalDose3>=32 & TotalDose3<=384~3,
                         Name=="Benazepril" & TotalDose3>=8 & TotalDose3<=96~3,
                         Name=="Enalapril" & TotalDose3>=2 & TotalDose3<=48~3,
                         Name=="Enalopril" & TotalDose3>=2 & TotalDose3<=48~3,
                         Name=="Moexipril" & TotalDose3>=6 & TotalDose3<=72~3,
                         Name=="Quinapril " & TotalDose3>=2 & TotalDose3<=96~3,
                         Name=="Imidapril " & TotalDose3>=2 & TotalDose3<=24~3,
                         Name=="Captopril" & TotalDose3>=10 & TotalDose3<=180 ~3,
                         Name=="Fosinopril" & TotalDose3>=8 & TotalDose3<=48~3,
                         Name=="Lisinopril" & TotalDose3>=2 & TotalDose3<=96~3,
                         Name=="Ramipril" & TotalDose3>=1 & TotalDose3<=12~3,
                         Name=="Perindopril" & TotalDose3>=1.6 & TotalDose3<=12~3,
                         Name=="Trandolapril" & TotalDose3>=0.4 & TotalDose3<=4.8~3,
                         Name=="Cilazapril" & TotalDose3>=0.4 & TotalDose3<=12~3,
                         
                         Name=="Amlodipine" & TotalCoverage1>=4 & TotalCoverage1<=12 ~ 4,
                         Name=="Felodipine" & TotalCoverage1>=2 & TotalCoverage1<=24~4,
                         Name=="Isradipine" & TotalCoverage1>=2 & TotalCoverage1<=24~4,
                         Name=="Lacidipine" & TotalCoverage1>=1.6 & TotalCoverage1<=7.2~4,
                         Name=="Lercanidipine" & TotalCoverage1>=8 & TotalCoverage1<=24~4,
                         Name=="Nicardipine" & TotalCoverage1>=48 & TotalCoverage1<=144~4,
                         Name=="Nifedipine" & TotalCoverage1>=16 & TotalCoverage1<=108~4,
                         Name=="Nisoldipine" & TotalCoverage1>=8 & TotalCoverage1<=72~4,
                         Name=="Verapamil" & TotalCoverage1>=96 & TotalCoverage1<=576~4,
                         Name=="Candesartan" & TotalCoverage1>=3.2 & TotalCoverage1<=38.4~4,
                         Name=="Eprosartan" & TotalCoverage1>=480 & TotalCoverage1<=720~4,
                         Name=="Irbesartan" & TotalCoverage1>=60 & TotalCoverage1<=360~4,
                         Name=="Losartan" & TotalCoverage1>=20 & TotalCoverage1<=120~4,
                         Name=="Olmesartan" & TotalCoverage1>=8 & TotalCoverage1<=48~4,
                         Name=="Telmisartan" & TotalCoverage1>=16 & TotalCoverage1<=96~4,
                         Name=="Valsartan" & TotalCoverage1>=32 & TotalCoverage1<=384~4,
                         Name=="Benazepril" & TotalCoverage1>=8 & TotalCoverage1<=96~4,
                         Name=="Enalapril" & TotalCoverage1>=2 & TotalCoverage1<=48~4,
                         Name=="Enalopril" & TotalCoverage1>=2 & TotalCoverage1<=48~4,
                         Name=="Moexipril" & TotalCoverage1>=6 & TotalCoverage1<=72~4,
                         Name=="Quinapril " & TotalCoverage1>=2 & TotalCoverage1<=96~4,
                         Name=="Imidapril " & TotalCoverage1>=2 & TotalCoverage1<=24~4,
                         Name=="Captopril" & TotalCoverage1>=10 & TotalCoverage1<=180 ~4,
                         Name=="Fosinopril" & TotalCoverage1>=8 & TotalCoverage1<=48~4,
                         Name=="Lisinopril" & TotalCoverage1>=2 & TotalCoverage1<=96~4,
                         Name=="Ramipril" & TotalCoverage1>=1 & TotalCoverage1<=12~4,
                         Name=="Perindopril" & TotalCoverage1>=1.6 & TotalCoverage1<=12~4,
                         Name=="Trandolapril" & TotalCoverage1>=0.4 & TotalCoverage1<=4.8~4,
                         Name=="Cilazapril" & TotalCoverage1>=0.4 & TotalCoverage1<=12~4,
                         
                         Name=="Amlodipine" & TotalCoverage2>=4 & TotalCoverage2<=12 ~ 5,
                         Name=="Felodipine" & TotalCoverage2>=2 & TotalCoverage2<=24~5,
                         Name=="Isradipine" & TotalCoverage2>=2 & TotalCoverage2<=24~5,
                         Name=="Lacidipine" & TotalCoverage2>=1.6 & TotalCoverage2<=7.2~5,
                         Name=="Lercanidipine" & TotalCoverage2>=8 & TotalCoverage2<=24~5,
                         Name=="Nicardipine" & TotalCoverage2>=48 & TotalCoverage2<=144~5,
                         Name=="Nifedipine" & TotalCoverage2>=16 & TotalCoverage2<=108~5,
                         Name=="Nisoldipine" & TotalCoverage2>=8 & TotalCoverage2<=72~5,
                         Name=="Verapamil" & TotalCoverage2>=96 & TotalCoverage2<=576~5,
                         Name=="Candesartan" & TotalCoverage2>=3.2 & TotalCoverage2<=38.4~5,
                         Name=="Eprosartan" & TotalCoverage2>=480 & TotalCoverage2<=720~5,
                         Name=="Irbesartan" & TotalCoverage2>=60 & TotalCoverage2<=360~5,
                         Name=="Losartan" & TotalCoverage2>=20 & TotalCoverage2<=120~5,
                         Name=="Olmesartan" & TotalCoverage2>=8 & TotalCoverage2<=48~5,
                         Name=="Telmisartan" & TotalCoverage2>=16 & TotalCoverage2<=96~5,
                         Name=="Valsartan" & TotalCoverage2>=32 & TotalCoverage2<=384~5,
                         Name=="Benazepril" & TotalCoverage2>=8 & TotalCoverage2<=96~5,
                         Name=="Enalapril" & TotalCoverage2>=2 & TotalCoverage2<=48~5,
                         Name=="Enalopril" & TotalCoverage2>=2 & TotalCoverage2<=48~5,
                         Name=="Moexipril" & TotalCoverage2>=6 & TotalCoverage2<=72~5,
                         Name=="Quinapril " & TotalCoverage2>=2 & TotalCoverage2<=96~5,
                         Name=="Imidapril " & TotalCoverage2>=2 & TotalCoverage2<=24~5,
                         Name=="Captopril" & TotalCoverage2>=10 & TotalCoverage2<=180 ~5,
                         Name=="Fosinopril" & TotalCoverage2>=8 & TotalCoverage2<=48~5,
                         Name=="Lisinopril" & TotalCoverage2>=2 & TotalCoverage2<=96~5,
                         Name=="Ramipril" & TotalCoverage2>=1 & TotalCoverage2<=12~5,
                         Name=="Perindopril" & TotalCoverage2>=1.6 & TotalCoverage2<=12~5,
                         Name=="Trandolapril" & TotalCoverage2>=0.4 & TotalCoverage2<=4.8~5,
                         Name=="Cilazapril" & TotalCoverage2>=0.4 & TotalCoverage2<=12~5,
                         TRUE ~ 0))

Check<-subset(Dur, valid==0)

Dur<-Dur%>%
  mutate(TotalDose=case_when(valid==1 ~ TotalDose1,
                             valid==2 ~ TotalDose2,
                             valid==3 ~ TotalDose3,
                             valid==4 ~ TotalCoverage1,
                             valid==5 ~ TotalCoverage2,
                             TRUE ~ NA_real_))

Valid<-Dur%>%
  subset(valid>0)%>%
  select(patid, TotalDose, issuedate, patdate, Name)%>%
  distinct()
length(unique(Valid$patdate))

ValidMulti<-Dur%>%
  subset(valid==0)%>%
  group_by(patdate, DailyDose, Coverage, Name)%>%
  mutate(Duplicate=n())%>%
  ungroup()%>%
  group_by(patdate)%>%
  mutate(n=n())%>%
  subset(Duplicate==n & n>1)%>% #Those that have multiple prescriptions that are all the same
  mutate(NewDose1=TotalDose1/n, NewDose2=TotalDose2/n,NewDose3=TotalDose3/n,NewCoverage1=TotalCoverage1/n, NewCoverage2=TotalCoverage2/n)%>%
  mutate(valid=case_when(Name=="Amlodipine" & NewDose1>=4 & NewDose1<=12 ~ 1,
                         Name=="Felodipine" & NewDose1>=2 & NewDose1<=24~1,
                         Name=="Isradipine" & NewDose1>=2 & NewDose1<=24~1,
                         Name=="Lacidipine" & NewDose1>=1.6 & NewDose1<=7.2~1,
                         Name=="Lercanidipine" & NewDose1>=8 & NewDose1<=24~1,
                         Name=="Nicardipine" & NewDose1>=48 & NewDose1<=144~1,
                         Name=="Nifedipine" & NewDose1>=16 & NewDose1<=108~1,
                         Name=="Nisoldipine" & NewDose1>=8 & NewDose1<=72~1,
                         Name=="Verapamil" & NewDose1>=96 & NewDose1<=576~1,
                         Name=="Candesartan" & NewDose1>=3.2 & NewDose1<=38.4~1,
                         Name=="Eprosartan" & NewDose1>=480 & NewDose1<=720~1,
                         Name=="Irbesartan" & NewDose1>=60 & NewDose1<=360~1,
                         Name=="Losartan" & NewDose1>=20 & NewDose1<=120~1,
                         Name=="Olmesartan" & NewDose1>=8 & NewDose1<=48~1,
                         Name=="Telmisartan" & NewDose1>=16 & NewDose1<=96~1,
                         Name=="Valsartan" & NewDose1>=32 & NewDose1<=384~1,
                         Name=="Benazepril" & NewDose1>=8 & NewDose1<=96~1,
                         Name=="Enalapril" & NewDose1>=2 & NewDose1<=48~1,
                         Name=="Enalopril" & NewDose1>=2 & NewDose1<=48~1,
                         Name=="Moexipril" & NewDose1>=6 & NewDose1<=72~1,
                         Name=="Quinapril " & NewDose1>=2 & NewDose1<=96~1,
                         Name=="Imidapril " & NewDose1>=2 & NewDose1<=24~1,
                         Name=="Captopril" & NewDose1>=10 & NewDose1<=180 ~1,
                         Name=="Fosinopril" & NewDose1>=8 & NewDose1<=48~1,
                         Name=="Lisinopril" & NewDose1>=2 & NewDose1<=96~1,
                         Name=="Ramipril" & NewDose1>=1 & NewDose1<=12~1,
                         Name=="Perindopril" & NewDose1>=1.6 & NewDose1<=12~1,
                         Name=="Trandolapril" & NewDose1>=0.4 & NewDose1<=4.8~1,
                         Name=="Cilazapril" & NewDose1>=0.4 & NewDose1<=12~1,
                         
                         Name=="Amlodipine" & NewDose2>=4 & NewDose2<=12 ~ 2,
                         Name=="Felodipine" & NewDose2>=2 & NewDose2<=24~2,
                         Name=="Isradipine" & NewDose2>=2 & NewDose2<=24~2,
                         Name=="Lacidipine" & NewDose2>=1.6 & NewDose2<=7.2~2,
                         Name=="Lercanidipine" & NewDose2>=8 & NewDose2<=24~2,
                         Name=="Nicardipine" & NewDose2>=48 & NewDose2<=144~2,
                         Name=="Nifedipine" & NewDose2>=16 & NewDose2<=108~2,
                         Name=="Nisoldipine" & NewDose2>=8 & NewDose2<=72~2,
                         Name=="Verapamil" & NewDose2>=96 & NewDose2<=576~2,
                         Name=="Candesartan" & NewDose2>=3.2 & NewDose2<=38.4~2,
                         Name=="Eprosartan" & NewDose2>=480 & NewDose2<=720~2,
                         Name=="Irbesartan" & NewDose2>=60 & NewDose2<=360~2,
                         Name=="Losartan" & NewDose2>=20 & NewDose2<=120~2,
                         Name=="Olmesartan" & NewDose2>=8 & NewDose2<=48~2,
                         Name=="Telmisartan" & NewDose2>=16 & NewDose2<=96~2,
                         Name=="Valsartan" & NewDose2>=32 & NewDose2<=384~2,
                         Name=="Benazepril" & NewDose2>=8 & NewDose2<=96~2,
                         Name=="Enalapril" & NewDose2>=2 & NewDose2<=48~2,
                         Name=="Enalopril" & NewDose2>=2 & NewDose2<=48~2,
                         Name=="Moexipril" & NewDose2>=6 & NewDose2<=72~2,
                         Name=="Quinapril " & NewDose2>=2 & NewDose2<=96~2,
                         Name=="Imidapril " & NewDose2>=2 & NewDose2<=24~2,
                         Name=="Captopril" & NewDose2>=10 & NewDose2<=180 ~2,
                         Name=="Fosinopril" & NewDose2>=8 & NewDose2<=48~2,
                         Name=="Lisinopril" & NewDose2>=2 & NewDose2<=96~2,
                         Name=="Ramipril" & NewDose2>=1 & NewDose2<=12~2,
                         Name=="Perindopril" & NewDose2>=1.6 & NewDose2<=12~2,
                         Name=="Trandolapril" & NewDose2>=0.4 & NewDose2<=4.8~2,
                         Name=="Cilazapril" & NewDose2>=0.4 & NewDose2<=12~2,
                         
                         Name=="Amlodipine" & NewDose3>=4 & NewDose3<=12 ~ 3,
                         Name=="Felodipine" & NewDose3>=2 & NewDose3<=24~3,
                         Name=="Isradipine" & NewDose3>=2 & NewDose3<=24~3,
                         Name=="Lacidipine" & NewDose3>=1.6 & NewDose3<=7.2~3,
                         Name=="Lercanidipine" & NewDose3>=8 & NewDose3<=24~3,
                         Name=="Nicardipine" & NewDose3>=48 & NewDose3<=144~3,
                         Name=="Nifedipine" & NewDose3>=16 & NewDose3<=108~3,
                         Name=="Nisoldipine" & NewDose3>=8 & NewDose3<=72~3,
                         Name=="Verapamil" & NewDose3>=96 & NewDose3<=576~3,
                         Name=="Candesartan" & NewDose3>=3.2 & NewDose3<=38.4~3,
                         Name=="Eprosartan" & NewDose3>=480 & NewDose3<=720~3,
                         Name=="Irbesartan" & NewDose3>=60 & NewDose3<=360~3,
                         Name=="Losartan" & NewDose3>=20 & NewDose3<=120~3,
                         Name=="Olmesartan" & NewDose3>=8 & NewDose3<=48~3,
                         Name=="Telmisartan" & NewDose3>=16 & NewDose3<=96~3,
                         Name=="Valsartan" & NewDose3>=32 & NewDose3<=384~3,
                         Name=="Benazepril" & NewDose3>=8 & NewDose3<=96~3,
                         Name=="Enalapril" & NewDose3>=2 & NewDose3<=48~3,
                         Name=="Enalopril" & NewDose3>=2 & NewDose3<=48~3,
                         Name=="Moexipril" & NewDose3>=6 & NewDose3<=72~3,
                         Name=="Quinapril " & NewDose3>=2 & NewDose3<=96~3,
                         Name=="Imidapril " & NewDose3>=2 & NewDose3<=24~3,
                         Name=="Captopril" & NewDose3>=10 & NewDose3<=180 ~3,
                         Name=="Fosinopril" & NewDose3>=8 & NewDose3<=48~3,
                         Name=="Lisinopril" & NewDose3>=2 & NewDose3<=96~3,
                         Name=="Ramipril" & NewDose3>=1 & NewDose3<=12~3,
                         Name=="Perindopril" & NewDose3>=1.6 & NewDose3<=12~3,
                         Name=="Trandolapril" & NewDose3>=0.4 & NewDose3<=4.8~3,
                         Name=="Cilazapril" & NewDose3>=0.4 & NewDose3<=12~3,
                         
                         Name=="Amlodipine" & NewCoverage1>=4 & NewCoverage1<=12 ~ 4,
                         Name=="Felodipine" & NewCoverage1>=2 & NewCoverage1<=24~4,
                         Name=="Isradipine" & NewCoverage1>=2 & NewCoverage1<=24~4,
                         Name=="Lacidipine" & NewCoverage1>=1.6 & NewCoverage1<=7.2~4,
                         Name=="Lercanidipine" & NewCoverage1>=8 & NewCoverage1<=24~4,
                         Name=="Nicardipine" & NewCoverage1>=48 & NewCoverage1<=144~4,
                         Name=="Nifedipine" & NewCoverage1>=16 & NewCoverage1<=108~4,
                         Name=="Nisoldipine" & NewCoverage1>=8 & NewCoverage1<=72~4,
                         Name=="Verapamil" & NewCoverage1>=96 & NewCoverage1<=576~4,
                         Name=="Candesartan" & NewCoverage1>=3.2 & NewCoverage1<=38.4~4,
                         Name=="Eprosartan" & NewCoverage1>=480 & NewCoverage1<=720~4,
                         Name=="Irbesartan" & NewCoverage1>=60 & NewCoverage1<=360~4,
                         Name=="Losartan" & NewCoverage1>=20 & NewCoverage1<=120~4,
                         Name=="Olmesartan" & NewCoverage1>=8 & NewCoverage1<=48~4,
                         Name=="Telmisartan" & NewCoverage1>=16 & NewCoverage1<=96~4,
                         Name=="Valsartan" & NewCoverage1>=32 & NewCoverage1<=384~4,
                         Name=="Benazepril" & NewCoverage1>=8 & NewCoverage1<=96~4,
                         Name=="Enalapril" & NewCoverage1>=2 & NewCoverage1<=48~4,
                         Name=="Enalopril" & NewCoverage1>=2 & NewCoverage1<=48~4,
                         Name=="Moexipril" & NewCoverage1>=6 & NewCoverage1<=72~4,
                         Name=="Quinapril " & NewCoverage1>=2 & NewCoverage1<=96~4,
                         Name=="Imidapril " & NewCoverage1>=2 & NewCoverage1<=24~4,
                         Name=="Captopril" & NewCoverage1>=10 & NewCoverage1<=180 ~4,
                         Name=="Fosinopril" & NewCoverage1>=8 & NewCoverage1<=48~4,
                         Name=="Lisinopril" & NewCoverage1>=2 & NewCoverage1<=96~4,
                         Name=="Ramipril" & NewCoverage1>=1 & NewCoverage1<=12~4,
                         Name=="Perindopril" & NewCoverage1>=1.6 & NewCoverage1<=12~4,
                         Name=="Trandolapril" & NewCoverage1>=0.4 & NewCoverage1<=4.8~4,
                         Name=="Cilazapril" & NewCoverage1>=0.4 & NewCoverage1<=12~4,
                         
                         Name=="Amlodipine" & NewCoverage2>=4 & NewCoverage2<=12 ~ 5,
                         Name=="Felodipine" & NewCoverage2>=2 & NewCoverage2<=24~5,
                         Name=="Isradipine" & NewCoverage2>=2 & NewCoverage2<=24~5,
                         Name=="Lacidipine" & NewCoverage2>=1.6 & NewCoverage2<=7.2~5,
                         Name=="Lercanidipine" & NewCoverage2>=8 & NewCoverage2<=24~5,
                         Name=="Nicardipine" & NewCoverage2>=48 & NewCoverage2<=144~5,
                         Name=="Nifedipine" & NewCoverage2>=16 & NewCoverage2<=108~5,
                         Name=="Nisoldipine" & NewCoverage2>=8 & NewCoverage2<=72~5,
                         Name=="Verapamil" & NewCoverage2>=96 & NewCoverage2<=576~5,
                         Name=="Candesartan" & NewCoverage2>=3.2 & NewCoverage2<=38.4~5,
                         Name=="Eprosartan" & NewCoverage2>=480 & NewCoverage2<=720~5,
                         Name=="Irbesartan" & NewCoverage2>=60 & NewCoverage2<=360~5,
                         Name=="Losartan" & NewCoverage2>=20 & NewCoverage2<=120~5,
                         Name=="Olmesartan" & NewCoverage2>=8 & NewCoverage2<=48~5,
                         Name=="Telmisartan" & NewCoverage2>=16 & NewCoverage2<=96~5,
                         Name=="Valsartan" & NewCoverage2>=32 & NewCoverage2<=384~5,
                         Name=="Benazepril" & NewCoverage2>=8 & NewCoverage2<=96~5,
                         Name=="Enalapril" & NewCoverage2>=2 & NewCoverage2<=48~5,
                         Name=="Enalopril" & NewCoverage2>=2 & NewCoverage2<=48~5,
                         Name=="Moexipril" & NewCoverage2>=6 & NewCoverage2<=72~5,
                         Name=="Quinapril " & NewCoverage2>=2 & NewCoverage2<=96~5,
                         Name=="Imidapril " & NewCoverage2>=2 & NewCoverage2<=24~5,
                         Name=="Captopril" & NewCoverage2>=10 & NewCoverage2<=180 ~5,
                         Name=="Fosinopril" & NewCoverage2>=8 & NewCoverage2<=48~5,
                         Name=="Lisinopril" & NewCoverage2>=2 & NewCoverage2<=96~5,
                         Name=="Ramipril" & NewCoverage2>=1 & NewCoverage2<=12~5,
                         Name=="Perindopril" & NewCoverage2>=1.6 & NewCoverage2<=12~5,
                         Name=="Trandolapril" & NewCoverage2>=0.4 & NewCoverage2<=4.8~5,
                         Name=="Cilazapril" & NewCoverage2>=0.4 & NewCoverage2<=12~5,
                         TRUE ~ 0))

ValidMulti<-ValidMulti%>%
  mutate(NewDose=case_when(valid==1 ~ NewDose1,
                             valid==2 ~ NewDose2,
                             valid==3 ~ NewDose3,
                             valid==4 ~ NewCoverage1,
                             valid==5 ~ NewCoverage2,
                             TRUE ~ NA_real_))
ValidMulti<-ValidMulti%>%
  ungroup()%>%
  subset(valid>0)%>%
  select(patid, TotalDose=NewDose, issuedate, patdate, Name)%>%
  distinct()

length(unique(ValidMulti$patdate))

Valid<-rbind(Valid, ValidMulti)

Valid<-Valid%>%
  group_by(patdate)%>%
  mutate(n=n())

Check<-subset(Valid, n>1)

length(unique(Valid$patdate))

#Check the invalid ones
NotValid<-subset(Dur, !(patdate %in% Valid$patdate)) #147894/5504674


####For all obs####
#Low= starting dose or lower

Valid<-Valid%>%
  mutate(AntiHypDose = case_when(Name=="Amlodipine" & TotalDose<=5 ~ "Low",
                               Name=="Felodipine" & TotalDose<=2.5 ~"Low",
                               Name=="Isradipine" & TotalDose<=2.5 ~"Low",
                               Name=="Lacidipine" & TotalDose<=2 ~"Low",
                               Name=="Lercanidipine" & TotalDose<=10 ~"Low",
                               Name=="Nicardipine" & TotalDose<=60 ~"Low",
                               Name=="Nifedipine" & TotalDose<=30~"Low",
                               Name=="Nisoldipine" & TotalDose<=20~"Low",
                               Name=="Verapamil" & TotalDose<=240 ~"Low",
                               Name=="Candesartan" & TotalDose<=8~"Low",
                               Name=="Eprosartan" & TotalDose<=540 ~"Low",
                               Name=="Irbesartan" & TotalDose<=150 ~"Low",
                               Name=="Losartan" & TotalDose<=50 ~"Low",
                               Name=="Olmesartan" & TotalDose<=10 ~"Low",
                               Name=="Telmisartan" & TotalDose<=40~"Low",
                               Name=="Valsartan" & TotalDose<=80 ~"Low",
                               Name=="Benazepril" & TotalDose<=10 ~"Low",
                               Name=="Enalapril" & TotalDose<=5 ~"Low",
                               Name=="Enalopril" & TotalDose<=5 ~"Low",
                               Name=="Moexipril" & TotalDose<=7.5 ~"Low",
                               Name=="Quinapril " & TotalDose<=10 ~"Low",
                               Name=="Imidapril " & TotalDose<=5 ~"Low",
                               Name=="Captopril" & TotalDose<=25 ~"Low",
                               Name=="Fosinopril" & TotalDose<=10 ~"Low",
                               Name=="Lisinopril" & TotalDose<=10 ~"Low",
                               Name=="Ramipril" & TotalDose<=1.25 ~"Low",
                               Name=="Perindopril" & TotalDose<=4 ~"Low",
                               Name=="Trandolapril" & TotalDose<=0.5 ~"Low",
                               Name=="Cilazapril" & TotalDose<=1.25 ~"Low",
                               
# Hi = mid-point of maintenance dose                               
                               Name=="Amlodipine" & TotalDose>=7.5 ~ "Hi",
                               Name=="Felodipine" & TotalDose>=7.5 ~"Hi",
                               Name=="Isradipine" & TotalDose>=10~"Hi",
                               Name=="Lacidipine" & TotalDose>=4 ~"Hi",
                               Name=="Lercanidipine" & TotalDose>=15 ~"Hi",
                               Name=="Nicardipine" & TotalDose>=90 ~"Hi",
                               Name=="Nifedipine" & TotalDose>=60 ~"Hi",
                               Name=="Nisoldipine" & TotalDose>=40 ~"Hi",
                               Name=="Verapamil" & TotalDose>=400 ~"Hi",
                               Name=="Candesartan" & TotalDose>=20 ~"Hi",
                               Name=="Eprosartan" & TotalDose>=660 ~"Hi",
                               Name=="Irbesartan" & TotalDose>=225 ~"Hi",
                               Name=="Losartan" & TotalDose>=75 ~"Hi",
                               Name=="Olmesartan" & TotalDose>=30 ~"Hi",
                               Name=="Telmisartan" & TotalDose>=60 ~"Hi",
                               Name=="Valsartan" & TotalDose>=120 ~"Hi",
Name=="Benazepril" & TotalDose>=30 ~"Hi",
Name=="Enalapril" & TotalDose>=30 ~"Hi",
Name=="Enalopril" & TotalDose>=30 ~"Hi",
Name=="Moexipril" & TotalDose>=18.75 ~"Hi",
Name=="Quinapril " & TotalDose>=15 ~"Hi",
Name=="Imidapril " & TotalDose>=15 ~"Hi",
Name=="Captopril" & TotalDose>=62.5 ~"Hi",
Name=="Fosinopril" & TotalDose>=25 ~"Hi",
Name=="Lisinopril" & TotalDose>=50 ~"Hi",
Name=="Ramipril" & TotalDose>=6.25 ~"Hi",
Name=="Perindopril" & TotalDose>=7.5 ~"Hi",
Name=="Trandolapril" & TotalDose>=1.5 ~"Hi",
Name=="Cilazapril" & TotalDose>=3.75 ~"Hi",
                               TRUE ~ "Med"))

table(Valid$AntiHypDose, useNA="ifany")

####Save####
save(Valid, file=paste0(ProjectDir, "/Data/ValidCCB.Rdata"))

####Set first valid date####

####Set date of first ARB/CCB####
#HospAll$FirstCCBARB <-pmin(HospAll$FirstDiuretic_BBP_ACEDate, HospAll$FirstDiuretic_BBP_ARBDate, HospAll$FirstBlocker_BBP_CCBDate, HospAll$FirstDiuretic_NBBP_ACEDate, HospAll$FirstDiuretic_NBBP_ARBDate, HospAll$FirstNBBP_ACEDate,HospAll$FirstBBP_ACEDate,HospAll$FirstNBBP_CCBDate, HospAll$FirstBBP_CCBDate, HospAll$FirstVerapamilDate, HospAll$FirstNBBP_ARBDate, HospAll$FirstBBP_ARBDate, na.rm=TRUE)

HospAll$FirstCCBARB <-pmin(HospAll$FirstNBBP_CCBDate, HospAll$FirstBBP_CCBDate, HospAll$FirstVerapamilDate, HospAll$FirstNBBP_ARBDate, HospAll$FirstBBP_ARBDate, HospAll$FirstNBBP_ACEDate, HospAll$FirstBBP_ACEDate, na.rm=TRUE)

save(HospAll, file = paste0(ProjectDir, "/Data/HypDate.rdata"))
