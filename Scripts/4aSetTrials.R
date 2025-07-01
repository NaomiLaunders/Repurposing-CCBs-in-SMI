####Set trial####

#Call start script
source("./Scripts/1aSetUp.R")

####Load final file####
load(paste0(ProjectDir, "/Data/Trial_Complete.rdata"))

names(Trial)
names(Trial)[sapply(Trial, is.numeric)]
names(Trial)[sapply(Trial, is.Date)]
names(Trial)[sapply(Trial, is.character)]

#Drop some I don't need
Trial<-select(Trial, -regioncode, -accept, -everQOF, -newpat, -hes_apc_e, -hes_ae_e, -hes_op_e, -ons_death_e, -lsoa_e, -n_patid_hes, -patid_n, -n, -regenddate, -lcd, -Date18, -Start2000, -End2022, -FirstQOF, -LastQOF, -FirstEvidenceInjDate, -FirstDiab, -FirstDementia, -dob, -Date100, -source, -gen_hesid, -Active0022, -FirstDiag, -LastSMIDate, -LastDiag, -EverBipolar, -EverOther, -EverSchizophrenia, -FirstComboDate, -FirstBlockerDate, -FirstDiuretic_BBP_ACEDate, -FirstDiuretic_BBP_ARBDate, -FirstBlocker_BBP_CCBDate, -FirstDiuretic_NBBP_ACEDate, -FirstDiuretic_NBBP_ARBDate, -FirstMI, -FirstCerebrovascular, -FirstCHF, -YearCCB, -HospDiabetes, -HospDementia, -FirstRaynauds)

####Change NAs to zeros for numeric data####
Numeric<-names(Trial)[sapply(Trial, is.numeric)]

sapply(Trial[,Numeric], summary)

Zeros<-(Numeric[40:53])

Trial <- Trial %>% 
  mutate_at(c(Zeros), ~replace_na(.,0))

####Set hypertension and BP time to years
Trial$TimeSinceHyp<-Trial$TimeSinceHyp/365.25
Trial$TimeSinceHighBP<-as.numeric(Trial$FirstCCBARB - Trial$FirstHighBPCatValue)/365.25
Trial$TimeSinceHighBP[Trial$TimeSinceHighBP<0]<-NA


####Add in max 2 year follow up####
Trial$TrialFU<-Trial$StudyFU
Trial$TrialFU[Trial$TrialFU>2]<-2

####Died in FU####
Trial$Died2yr<-0
Trial$Died2yr[Trial$deathdate-Trial$FirstCCBARB<730.5]<-1

###YOB integer###
Trial$yob<-as.integer(Trial$yob)

####Change some dates to years####

Trial$FirstPsychTime<-as.numeric(Trial$FirstPsychTime)/365.25
Trial$SMITime<-as.numeric(Trial$SMITime)/365.25

####BMI missing####
Trial$BMIValMiss<-0
Trial$BMIValMiss[is.na(Trial$BMIVal)]<-1

####Missing antihypertensive dose####
Trial$AntiHypDoseMiss<-0
Trial$AntiHypDoseMiss[is.na(Trial$AntiHypDose)]<-1

####Missing BP measure
Trial$DiaMiss<-0
Trial$DiaMiss[is.na(Trial$Diastolic)]<-1
Trial$SysMiss<-0
Trial$SysMiss[is.na(Trial$Systolic)]<-1

####Current registration####

Trial$RegTime<-as.numeric(Trial$FirstCCBARB - Trial$regstartdate)/365.25

####Binary prior use variables####

Trial$PriorMHBin<-0
Trial$PriorMHBin[Trial$PriorMH>0]<-1

Trial$PriorSHBin<-0
Trial$PriorSHBin[Trial$PriorSH>0]<-1

Trial$PriorPhysicalBin<-0
Trial$PriorPhysicalBin[Trial$PriorPhysical>0]<-1

Trial$PriorAccidentBin<-0
Trial$PriorAccidentBin[Trial$PriorAccident>0]<-1

Trial$PriorGPBin<-0
Trial$PriorGPBin[Trial$PriorGP>0]<-1

Trial$PriorGPSHBin<-0
Trial$PriorGPSHBin[Trial$PriorSHGP>0]<-1

####Sort out time to event####
Trial$TimeToMH24<- as.numeric(Trial$OutcomeMH24Date - Trial$FirstCCBARB)

#Combined SH outcome
Trial$OutcomeCombSH24Date<-pmin(Trial$OutcomeSH24Date, Trial$FirstSHGP24, na.rm=TRUE)
Trial$TimeToSH24<- as.numeric(Trial$OutcomeCombSH24Date - Trial$FirstCCBARB)

Trial$TimeToAll24<- pmin(Trial$TimeToMH24, Trial$TimeToSH24, na.rm = TRUE)

Trial$OutcomeCombSH24<-Trial$OutcomeSH24+Trial$OutcomeSHGP24
Trial$OutcomeCombSH12<-Trial$OutcomeSH12+Trial$OutcomeSHGP12
Trial$OutcomeCombSH6<-Trial$OutcomeSH6+Trial$OutcomeSHGP6
Trial$OutcomeCombSH3<-Trial$OutcomeSH3+Trial$OutcomeSHGP3

#If they dont have an event then it is time to end of follow up.

Trial$TimeToMH24[is.na(Trial$TimeToMH24)]<-Trial$TrialFU[is.na(Trial$TimeToMH24)]*365.25
Trial$TimeToSH24[is.na(Trial$TimeToSH24)]<-Trial$TrialFU[is.na(Trial$TimeToSH24)]*365.25

summary(Trial$TimeToMH24)
summary(Trial$TrialFU)

#Define binary outcome#
Trial$MHBin24<-0
Trial$MHBin24[Trial$OutcomeMH24>0]<-1
Trial$SHBin24<-0
Trial$SHBin24[Trial$OutcomeSH24>0|Trial$OutcomeSHGP24>0]<-1

Trial$MHBin12<-0
Trial$MHBin12[Trial$OutcomeMH12>0]<-1
Trial$SHBin12<-0
Trial$SHBin12[Trial$OutcomeSH12>0|Trial$OutcomeSHGP12>0]<-1

Trial$MHBin6<-0
Trial$MHBin6[Trial$OutcomeMH6>0]<-1
Trial$SHBin6<-0
Trial$SHBin6[Trial$OutcomeSH6>0|Trial$OutcomeSHGP6>0]<-1

Trial$MHBin3<-0
Trial$MHBin3[Trial$OutcomeMH3>0]<-1
Trial$SHBin3<-0
Trial$SHBin3[Trial$OutcomeSH3>0|Trial$OutcomeSHGP3>0]<-1

#Set time as integer
Trial$TimeToMH24<-as.integer(round(Trial$TimeToMH24))
Trial$TimeToSH24<-as.integer(round(Trial$TimeToSH24))

Trial$TimeToMH12<-Trial$TimeToMH24
Trial$TimeToMH12[Trial$TimeToMH12>365]<-as.integer(365)
Trial$TimeToSH12<-Trial$TimeToSH24
Trial$TimeToSH12[Trial$TimeToSH12>365]<-as.integer(365)

Trial$TimeToMH6<-Trial$TimeToMH24
Trial$TimeToMH6[Trial$TimeToMH6>182]<-as.integer(182)
Trial$TimeToSH6<-Trial$TimeToSH24
Trial$TimeToSH6[Trial$TimeToSH6>182]<-as.integer(182)

Trial$TimeToMH3<-Trial$TimeToMH24
Trial$TimeToMH3[Trial$TimeToMH3>90]<-as.integer(90)
Trial$TimeToSH3<-Trial$TimeToSH24
Trial$TimeToSH3[Trial$TimeToSH3>90]<-as.integer(90)

####Primary outcome####
Trial$AllBin24<-0
Trial$AllBin24[Trial$MHBin24==1|Trial$SHBin24==1]<-1
Trial$OutcomeAll24<-Trial$OutcomeSH24+Trial$OutcomeSHGP24+Trial$OutcomeMH24

Trial$AllBin12<-0
Trial$AllBin12[Trial$MHBin12==1|Trial$SHBin12==1]<-1
Trial$OutcomeAll12<-Trial$OutcomeSH12+Trial$OutcomeSHGP12+Trial$OutcomeMH12

Trial$AllBin6<-0
Trial$AllBin6[Trial$MHBin6==1|Trial$SHBin6==1]<-1
Trial$OutcomeAll6<-Trial$OutcomeSH6+Trial$OutcomeSHGP12+Trial$OutcomeMH6

Trial$AllBin3<-0
Trial$AllBin3[Trial$MHBin3==1|Trial$SHBin3==1]<-1
Trial$OutcomeAll3<-Trial$OutcomeSH3+Trial$OutcomeSHGP3+Trial$OutcomeMH3

Trial$TimeToAll24<-pmin(Trial$TimeToMH24, Trial$TimeToSH24)
Trial$TimeToAll12<-pmin(Trial$TimeToMH12, Trial$TimeToSH12)
Trial$TimeToAll6<-pmin(Trial$TimeToMH6, Trial$TimeToSH6)
Trial$TimeToAll3<-pmin(Trial$TimeToMH3, Trial$TimeToSH3)

####Sort most recent AP at dose####
load(paste0(ProjectDir, "/Extracts/BP_dose_CCB.Rdata"))
load(paste0(ProjectDir, "/Extracts/AP_dose_CCB.Rdata"))

CCBDate<-select(Trial, patid, FirstCCBARB)

BPDose<-select(BPDoseFinal, patid, eventdate, AP)
APDose<-select(apDoseFinal, patid, eventdate, AP)
AllDose<-rbind(BPDose, APDose)
AllDose<-distinct(AllDose)
AllDose<-merge(x=AllDose, y=CCBDate, by="patid", all.x=FALSE, all.y=FALSE)

AllDose6mo<-subset(AllDose, FirstCCBARB-eventdate<=182.625 & FirstCCBARB-eventdate>=0) #Have a prescription in 6 months before in

APDose<-AllDose6mo%>%
  group_by(patid)%>%
  mutate(max=max(eventdate))%>%
  subset(max==eventdate)%>%
  select(patid, AP)%>%
  distinct()%>%
  mutate(n=n())%>%
  mutate(APs=case_when(n>1 ~ "Multiple",
                       TRUE ~ AP))%>%
  distinct()

APDose<-APDose%>%
  group_by(patid)%>%
  pivot_wider(names_from = "AP", values_from="AP")

APDose<-APDose%>%
  unite("Multiple", 4:30, sep = "_", remove = FALSE, na.rm = TRUE)


APDose$AP<-case_when(grepl("lithium", APDose$Multiple) & grepl("valproate", APDose$Multiple) & grepl("lamotrigine", APDose$Multiple) & APDose$n==3 ~ "BP",
                     grepl("lithium", APDose$Multiple) & grepl("valproate", APDose$Multiple) & APDose$n==2 ~ "BP",
                     grepl("lithium", APDose$Multiple) & grepl("lamotrigine", APDose$Multiple) & APDose$n==2 ~ "BP",
                     grepl("valproate", APDose$Multiple) & grepl("lamotrigine", APDose$Multiple) & APDose$n==2 ~ "BP",
                     !(grepl("lithium|valproate|lamotrigine", APDose$Multiple)) & APDose$n>1 ~ "AP",
                     APDose$n>1 ~ "Mixed",
                     TRUE ~ APDose$APs)

length(unique(APDose$patid))

table(APDose$AP, useNA="ifany")

APDose<-select(APDose, patid, AP)
Trial<-select(Trial, -AP)
Trial<-merge(x=Trial, y=APDose, by="patid", all.x=TRUE, all.y=FALSE)

####Enter year####
Trial$CCByear<-year(Trial$FirstCCBARB)

####CombinedBP####
Trial$AnyHyp<-0
Trial$AnyHyp[Trial$PriorHyp==1 | Trial$PriorHighBP==1]<-1

Trial$TimeSinceAnyHyp <- pmin(Trial$TimeSinceHighBP, Trial$TimeSinceHyp, na.rm = TRUE)

####Convert to factors####
Factors<-c("gender", "HospHF", "HospCereb", "HospMI", "GPCerebrovascular", "GPMI", "GPCHF", "AllMI", "AllCerebrovascular", "AllCHF", "Died2yr", "region", "ethnicity", "Arm", "Drug", "PriorHyp", "PriorHighBP", "BMICat", "SMIDiag", "AntiHypDose", "SSRI", "TCA", "Other", "PatIMD", "PracIMD", "PriorMHBin", "PriorSHBin", "PriorPhysicalBin", "PriorAccidentBin", "PriorGPBin", "PriorGPSHBin", "AP", "SysMiss", "DiaMiss", "BMIValMiss", "AntiHypDoseMiss", "AnyHyp")

Trial[,Factors]<-lapply(Trial[,Factors], factor)

  
#Export data dictionary
DataDictionary<-names(Trial)
write.csv(DataDictionary, "Outputs/DataDictionary.csv")

####Re-order so it makes sense####


Trial<-Trial[,c(1:4, 8, 45, 72, 73, 97, 27:34, 96, 35, 102, 143, 40:42, 46:51, 55, 56, 144, 57, 95, 145, 58, 101, 59, 100, 60, 98, 62, 64, 142, 70, 71, 67:69, 74, 107, 75, 108, 83, 103, 82, 104, 81, 105, 84, 106, 39, 43, 44, 65, 66, 99, 76:79, 85:92, 116, 115, 114, 113, 138, 136, 134, 132, 117, 118, 131, 119, 120, 133, 121, 122, 135, 123, 124, 137,109, 111, 112, 125, 126, 139, 127, 128, 140, 129, 130, 141,  5:7, 9:26,52:54, 61,63,  80, 93, 94, 110, 36:38)]

####Set NBBP_CCB as the comparator####
Trial$Arm=relevel(Trial$Arm, ref="NBBP_CCB")

save(Trial, file=paste0(ProjectDir, "/Data/Trial_Analysis.rdata"))

     