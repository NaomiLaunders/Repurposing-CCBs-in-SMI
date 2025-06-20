####Append BMI in the last 5 years####

#Call start script
source("./Scripts/1aSetUp.R")

load(paste0(ProjectDir, "/Data/CPRD_Hyp.rdata"))

load(paste0(ProjectDir, "/Extracts/BMIAurumCCBPop.Rdata"))
load(paste0(ProjectDir, "/Extracts/BMIGoldCCBPop.Rdata"))

BMI<-rbind(BMIFinalA, BMIFinalG)
rm(BMIFinalA, BMIFinalG)

length(unique(BMI$patid))

#Find BMI value in the 3 years before antihypertensives - because trial 2 is subset, can just use trial 1
Date<-select(Trial, patid, FirstCCBARB)
BMI<-merge(x=BMI, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

#NUmber with BMI recorded
length(unique(BMI$patid)) # 98% complete

BMICCB<-subset(BMI, eventdate<=FirstCCBARB)
length(unique(BMICCB$patid)) # 94% complete

#How many have it in 3 years
#3 years = 83% complete
BMI3<-BMICCB%>%
  mutate(Time=FirstCCBARB-eventdate)%>%
  subset(Time<=1095.25)
length(unique(BMI3$patid))

#5 years = 87% complete
BMI5<-BMICCB%>%
  mutate(Time=FirstCCBARB-eventdate)%>%
  subset(Time<=1826.25)

length(unique(BMI5$patid))

####Calculate most recent value####
BMI5Value<-BMI5%>%
  subset(!is.na(value))%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)
  
length(unique(BMI5Value$patid)) #87% complete

#Take unique ones
BMI5Value<-BMI5Value%>%
  group_by(patid)%>%
  mutate(n=n(), EverValue=sum(Type=="Value"), EverCalc=sum(Type=="Calc"))

Unique<-subset(BMI5Value, n==1)

#If Multiple, take value over calculated and if multiple values take the average
Multi<-subset(BMI5Value, n>1)
  
Values<-Multi%>%
  group_by(patid)%>%
  subset(EverValue>0 & Type=="Value")%>%
  mutate(MeanBMI=mean(value))%>%
  select(patid, value=MeanBMI, BMICat, eventdate, Type, FirstCCBARB, Time)%>%
  distinct()

length(unique(Values$patid))

Unique<-rbind(Unique, Values)

#If just calculations
Calc<-subset(BMI5Value, ! patid %in% Unique$patid)
Calc<-Calc%>%
  group_by(patid)%>%
  mutate(MeanBMI=mean(value))%>%
  select(patid, value=MeanBMI, BMICat, eventdate, Type, FirstCCBARB, Time)%>%
  distinct()

BMIValue5<-rbind(Unique, Calc)
BMIValue5<-select(BMIValue5, -EverValue, -EverCalc, -maxDate, -n)
length(unique(BMIValue5$patid))

#####Calculate most recent category####

#Take most recent category
BMI5Cat<-subset(BMI5, Type=="Cat")

BMI5Cat<-BMI5Cat%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)%>%
  distinct()%>%
  mutate(n=n())

Unique<-subset(BMI5Cat, n==1)

Multi<-subset(BMI5Cat, n>1)

#If multiple, take the worst
Multi<-Multi%>%
  group_by(patid)%>%
  mutate(EverObese=sum(BMICat=="Obese"), EverOver=sum(BMICat=="Overweight"), EverNormal=sum(BMICat=="Normal"))%>%
  mutate(BMICat=case_when(EverObese>0 ~ "Obese",
                          EverOver>0 ~ "Overweight",
                          EverNormal>0 ~ "Normal",
                          TRUE ~ BMICat))%>%
  distinct()%>%
  select(-EverObese, -EverOver, -EverNormal)
length(unique(Multi$patid))

BMICat5<-rbind(Unique, Multi) 
BMICat5<-select(BMICat5, -maxDate, n)
length(unique(BMICat5$patid))

#Combine unique values and categories and calculate category from value
UniqueBMI<-rbind(BMIValue5, BMICat5)
length(unique(UniqueBMI$patid))

UniqueBMI<-UniqueBMI%>%
  mutate(BMICat=case_when(is.na(BMICat) & value<18.5 ~ "Underweight",
                          is.na(BMICat) & value>=18.5 & value<25 ~ "Normal",
                          is.na(BMICat) & value>=25 & value<30 ~ "Overweight",
                          is.na(BMICat) & value>=30 ~ "Obese",
                          TRUE ~ BMICat))
table(UniqueBMI$BMICat, useNA="ifany")

#Take the most recent
UniqueBMI<-UniqueBMI%>%
  select(-value, -Type, -n)%>%
  group_by(patid)%>%
  mutate(maxDate=max(eventdate, na.rm=FALSE))%>%
  subset(maxDate==eventdate)%>%
  distinct()%>%
  mutate(n=n())

length(unique(UniqueBMI$patid))

Unique<-subset(UniqueBMI, n==1)

Multi<-subset(UniqueBMI, n>1)

#If multiple, take the worst
Multi<-Multi%>%
  group_by(patid)%>%
  mutate(EverObese=sum(BMICat=="Obese"), EverOver=sum(BMICat=="Overweight"), EverNormal=sum(BMICat=="Normal"))%>%
  mutate(BMICat=case_when(EverObese>0 ~ "Obese",
                          EverOver>0 ~ "Overweight",
                          EverNormal>0 ~ "Normal",
                          TRUE ~ BMICat))%>%
  distinct()%>%
  select(-EverObese, -EverOver, -EverNormal)

length(unique(Multi$patid))

BMICat<-rbind(Unique, Multi) 

BMIValue<-select(BMIValue5, patid, BMIVal=value, BMIValDate=eventdate)
BMICat<-select(BMICat, patid, BMICat=BMICat, BMICatDate=eventdate)

Trial<-merge(x=Trial, y=BMIValue, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=BMICat, by="patid", all.x=TRUE, all.y=FALSE)

table(Trial$BMICat, useNA="ifany")

length(which(!is.na(Trial$BMICat)))/length(which(!is.na(Trial$patid)))*100
length(which(!is.na(Trial$BMIVal)))/length(which(!is.na(Trial$patid)))*100

save(Trial, file = paste0(ProjectDir, "/Data/CPRD_BMI.rdata"))
