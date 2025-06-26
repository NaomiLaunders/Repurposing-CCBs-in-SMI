####Dose at first CCB prescription####

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
CCB<-subset(CCB, Class=="CCB")

rm(AllCCB)
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
                         Name=="Nicardipine" & TotalDose1>=16 & TotalDose1<=48~1,
                         Name=="Nifedipine" & TotalDose1>=16 & TotalDose1<=108~1,
                         Name=="Nisoldipine" & TotalDose1>=8 & TotalDose1<=72~1,
                         Name=="Verapamil" & TotalDose1>=96 & TotalDose1<=576~1,
                         
                         Name=="Amlodipine" & TotalDose2>=4 & TotalDose2<=12 ~ 2,
                         Name=="Felodipine" & TotalDose2>=2 & TotalDose2<=24~2,
                         Name=="Isradipine" & TotalDose2>=2 & TotalDose2<=24~2,
                         Name=="Lacidipine" & TotalDose2>=1.6 & TotalDose2<=7.2~2,
                         Name=="Lercanidipine" & TotalDose2>=8 & TotalDose2<=24~2,
                         Name=="Nicardipine" & TotalDose2>=16 & TotalDose2<=48~2,
                         Name=="Nifedipine" & TotalDose2>=16 & TotalDose2<=108~2,
                         Name=="Nisoldipine" & TotalDose2>=8 & TotalDose2<=72~2,
                         Name=="Verapamil" & TotalDose2>=96 & TotalDose2<=576~2,
                         
                         Name=="Amlodipine" & TotalDose3>=4 & TotalDose3<=12 ~ 3,
                         Name=="Felodipine" & TotalDose3>=2 & TotalDose3<=24~3,
                         Name=="Isradipine" & TotalDose3>=2 & TotalDose3<=24~3,
                         Name=="Lacidipine" & TotalDose3>=1.6 & TotalDose3<=7.2~3,
                         Name=="Lercanidipine" & TotalDose3>=8 & TotalDose3<=24~3,
                         Name=="Nicardipine" & TotalDose3>=16 & TotalDose3<=48~3,
                         Name=="Nifedipine" & TotalDose3>=16 & TotalDose3<=108~3,
                         Name=="Nisoldipine" & TotalDose3>=8 & TotalDose3<=72~3,
                         Name=="Verapamil" & TotalDose3>=96 & TotalDose3<=576~3,
                         
                         Name=="Amlodipine" & TotalCoverage1>=4 & TotalCoverage1<=12 ~ 4,
                         Name=="Felodipine" & TotalCoverage1>=2 & TotalCoverage1<=24~4,
                         Name=="Isradipine" & TotalCoverage1>=2 & TotalCoverage1<=24~4,
                         Name=="Lacidipine" & TotalCoverage1>=1.6 & TotalCoverage1<=7.2~4,
                         Name=="Lercanidipine" & TotalCoverage1>=8 & TotalCoverage1<=24~4,
                         Name=="Nicardipine" & TotalCoverage1>=16 & TotalCoverage1<=48~4,
                         Name=="Nifedipine" & TotalCoverage1>=16 & TotalCoverage1<=108~4,
                         Name=="Nisoldipine" & TotalCoverage1>=8 & TotalCoverage1<=72~4,
                         Name=="Verapamil" & TotalCoverage1>=96 & TotalCoverage1<=576~4,
                        
                         Name=="Amlodipine" & TotalCoverage2>=4 & TotalCoverage2<=12 ~ 5,
                         Name=="Felodipine" & TotalCoverage2>=2 & TotalCoverage2<=24~5,
                         Name=="Isradipine" & TotalCoverage2>=2 & TotalCoverage2<=24~5,
                         Name=="Lacidipine" & TotalCoverage2>=1.6 & TotalCoverage2<=7.2~5,
                         Name=="Lercanidipine" & TotalCoverage2>=8 & TotalCoverage2<=24~5,
                         Name=="Nicardipine" & TotalCoverage2>=16 & TotalCoverage2<=48~5,
                         Name=="Nifedipine" & TotalCoverage2>=16 & TotalCoverage2<=108~5,
                         Name=="Nisoldipine" & TotalCoverage2>=8 & TotalCoverage2<=72~5,
                         Name=="Verapamil" & TotalCoverage2>=96 & TotalCoverage2<=576~5,
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
                         Name=="Nicardipine" & NewDose1>=16 & NewDose1<=48~1,
                         Name=="Nifedipine" & NewDose1>=16 & NewDose1<=108~1,
                         Name=="Nisoldipine" & NewDose1>=8 & NewDose1<=72~1,
                         Name=="Verapamil" & NewDose1>=96 & NewDose1<=576~1,
                        
                         Name=="Amlodipine" & NewDose2>=4 & NewDose2<=12 ~ 2,
                         Name=="Felodipine" & NewDose2>=2 & NewDose2<=24~2,
                         Name=="Isradipine" & NewDose2>=2 & NewDose2<=24~2,
                         Name=="Lacidipine" & NewDose2>=1.6 & NewDose2<=7.2~2,
                         Name=="Lercanidipine" & NewDose2>=8 & NewDose2<=24~2,
                         Name=="Nicardipine" & NewDose2>=48 & NewDose2<=144~2,
                         Name=="Nifedipine" & NewDose2>=16 & NewDose2<=108~2,
                         Name=="Nisoldipine" & NewDose2>=8 & NewDose2<=72~2,
                         Name=="Verapamil" & NewDose2>=96 & NewDose2<=576~2,
                        
                         Name=="Amlodipine" & NewDose3>=4 & NewDose3<=12 ~ 3,
                         Name=="Felodipine" & NewDose3>=2 & NewDose3<=24~3,
                         Name=="Isradipine" & NewDose3>=2 & NewDose3<=24~3,
                         Name=="Lacidipine" & NewDose3>=1.6 & NewDose3<=7.2~3,
                         Name=="Lercanidipine" & NewDose3>=8 & NewDose3<=24~3,
                         Name=="Nicardipine" & NewDose3>=48 & NewDose3<=144~3,
                         Name=="Nifedipine" & NewDose3>=16 & NewDose3<=108~3,
                         Name=="Nisoldipine" & NewDose3>=8 & NewDose3<=72~3,
                         Name=="Verapamil" & NewDose3>=96 & NewDose3<=576~3,
                         
                         Name=="Amlodipine" & NewCoverage1>=4 & NewCoverage1<=12 ~ 4,
                         Name=="Felodipine" & NewCoverage1>=2 & NewCoverage1<=24~4,
                         Name=="Isradipine" & NewCoverage1>=2 & NewCoverage1<=24~4,
                         Name=="Lacidipine" & NewCoverage1>=1.6 & NewCoverage1<=7.2~4,
                         Name=="Lercanidipine" & NewCoverage1>=8 & NewCoverage1<=24~4,
                         Name=="Nicardipine" & NewCoverage1>=48 & NewCoverage1<=144~4,
                         Name=="Nifedipine" & NewCoverage1>=16 & NewCoverage1<=108~4,
                         Name=="Nisoldipine" & NewCoverage1>=8 & NewCoverage1<=72~4,
                         Name=="Verapamil" & NewCoverage1>=96 & NewCoverage1<=576~4,
                         
                         Name=="Amlodipine" & NewCoverage2>=4 & NewCoverage2<=12 ~ 5,
                         Name=="Felodipine" & NewCoverage2>=2 & NewCoverage2<=24~5,
                         Name=="Isradipine" & NewCoverage2>=2 & NewCoverage2<=24~5,
                         Name=="Lacidipine" & NewCoverage2>=1.6 & NewCoverage2<=7.2~5,
                         Name=="Lercanidipine" & NewCoverage2>=8 & NewCoverage2<=24~5,
                         Name=="Nicardipine" & NewCoverage2>=48 & NewCoverage2<=144~5,
                         Name=="Nifedipine" & NewCoverage2>=16 & NewCoverage2<=108~5,
                         Name=="Nisoldipine" & NewCoverage2>=8 & NewCoverage2<=72~5,
                         Name=="Verapamil" & NewCoverage2>=96 & NewCoverage2<=576~5,
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
#Code up dosage categories just for CCBs

Valid<-Valid%>%
  mutate(AntiHypDose = case_when(Name=="Amlodipine" & TotalDose<=5 ~ "Starting",
                               Name=="Felodipine" & TotalDose<=5 ~"Starting",
                               Name=="Isradipine" & TotalDose<=2.5 ~"Starting",
                               Name=="Lacidipine" & TotalDose<=2 ~"Starting",
                               Name=="Lercanidipine" & TotalDose<=10 ~"Starting",
                               Name=="Nicardipine" & TotalDose<=20 ~"Starting",
                               Name=="Nifedipine" & TotalDose<=30 ~"Starting",
                               Name=="Nisoldipine" & TotalDose<=17~"Starting",
                               
# Hi = mid-point of maintenance dose                               
                               Name=="Amlodipine" & TotalDose>=10 ~ "Max",
                               Name=="Felodipine" & TotalDose>=20 ~"Max",
                               Name=="Isradipine" & TotalDose>=20~"Max",
                               Name=="Lacidipine" & TotalDose>=6 ~"Max",
                               Name=="Lercanidipine" & TotalDose>=20 ~"Max",
                               Name=="Nicardipine" & TotalDose>=40 ~"Max",
                               Name=="Nifedipine" & TotalDose>=90 ~"Max",
                               Name=="Nisoldipine" & TotalDose>=60 ~"Max",

                               TRUE ~ "Maintenance"))

table(Valid$AntiHypDose, useNA="ifany")

####Save####
save(Valid, file=paste0(ProjectDir, "/Data/ValidCCB.Rdata"))

####Set first valid date####

####Set date of first ARB/CCB####
HospAll$FirstCCBARB <-pmin(HospAll$FirstDiuretic_BBP_ACEDate, HospAll$FirstDiuretic_BBP_ARBDate, HospAll$FirstBlocker_BBP_CCBDate, HospAll$FirstDiuretic_NBBP_ACEDate, HospAll$FirstDiuretic_NBBP_ARBDate, HospAll$FirstNBBP_ACEDate,HospAll$FirstBBP_ACEDate,HospAll$FirstNBBP_CCBDate, HospAll$FirstBBP_CCBDate, HospAll$FirstVerapamilDate, HospAll$FirstNBBP_ARBDate, HospAll$FirstBBP_ARBDate, HospAll$FirstComboDate, na.rm=TRUE)

summary(HospAll$FirstCCBARB)

save(HospAll, file = paste0(ProjectDir, "/Data/HypDate.rdata"))
