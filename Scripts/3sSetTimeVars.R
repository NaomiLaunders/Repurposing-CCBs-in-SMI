####Set time vars#####

#Call start script
source("./Scripts/1aSetUp.R")

load(paste0(ProjectDir, "/Data/Trial_AD.rdata"))

#Year of start
Trial$YearCCB<-year(Trial$FirstCCBARB)

#Time since first SMI diagnosis
Trial$SMITime<-as.numeric(Trial$FirstCCBARB - Trial$diagnosis_date)
summary(Trial$SMITime)

length(which(is.na(Trial$SMITime)))

#Time since first antipsychotic/mood stabiliser prescription
Trial$FirstPsychTime<-as.numeric(Trial$FirstCCBARB - Trial$FirstPsychDate)
summary(Trial$FirstPsychTime)

length(which(is.na(Trial$FirstPsychTime)))

#Most recent antipsychotic
load(paste0(ProjectDir, "/StatinExtracts/APProd.Rdata"))
PatAPAll<-subset(PatAPAll, patid %in% Trial$patid)
Date<-select(Trial, patid, FirstCCBARB)
PatAPAll<-merge(x=PatAPAll, y=Date, by="patid", all.x=TRUE, all.y=FALSE)

AP<-PatAPAll%>%
  group_by(patid)%>%
  subset(eventdate<=FirstCCBARB)%>%
  mutate(max=max(eventdate))%>%
  subset(max==eventdate)%>%
  select(patid, AP)%>%
  distinct()%>%
  mutate(n=n())%>%
  mutate(APs=case_when(n>1 ~ "Multiple",
                       TRUE ~ AP))

AP<-AP%>%
  group_by(patid)%>%
  pivot_wider(names_from = "AP", values_from="AP")

AP<-AP%>%
  unite("Multiple", 4:30, sep = "_", remove = FALSE, na.rm = TRUE)


AP$AP<-case_when(grepl("Lithium", AP$Multiple) & grepl("Valproate", AP$Multiple) & grepl("Lamotrigine", AP$Multiple) & AP$n==3 ~ "BP",
                     grepl("Lithium", AP$Multiple) & grepl("Valproate", AP$Multiple) & AP$n==2 ~ "BP",
                     grepl("Lithium", AP$Multiple) & grepl("Lamotrigine", AP$Multiple) & AP$n==2 ~ "BP",
                     grepl("Valproate", AP$Multiple) & grepl("Lamotrigine", AP$Multiple) & AP$n==2 ~ "BP",
                     !(grepl("Lithium|Valproate|Lamotrigine", AP$Multiple)) & AP$n>1 ~ "AP",
                       AP$n>1 ~ "Mixed",
                     TRUE ~ AP$APs)

length(unique(AP$patid))

table(AP$AP)

AP<-select(AP, patid, AP=AP)

Trial<-merge(x=Trial, y=AP, by="patid", all.x=TRUE, all.y=FALSE)

save(Trial, file=paste0(ProjectDir, "/Data/Trial_Time.rdata"))
