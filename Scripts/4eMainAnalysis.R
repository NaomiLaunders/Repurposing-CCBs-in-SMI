####Main analysis - confounder adjusted Cox after MICE####

#Call start script
source("./Scripts/4dAnalysisPackages.R")

####Load trials####
load(paste0(ProjectDir, "/Data/Trial_final.rdata"))

####Imputation####
#Find the nelson aalen for 2 year outcomes
HazardMH <- basehaz(coxph(Surv(TimeToMH24, MHBin24)~1,data=Trial))
HazardMH<-select(HazardMH, HazardMH=hazard, TimeToMH24=time)
HazardSH<- basehaz(coxph(Surv(TimeToSH24, SHBin24)~1,data=Trial))
HazardSH<-select(HazardSH, HazardSH=hazard, TimeToSH24=time)
HazardAll<- basehaz(coxph(Surv(TimeToAll24, AllBin24)~1,data=Trial))
HazardAll<-select(HazardAll, HazardAll=hazard, TimeToAll24=time)

Trial<-merge(x=Trial, y=HazardMH, by="TimeToMH24", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=HazardSH, by="TimeToSH24", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=HazardAll, by="TimeToAll24", all.x=TRUE, all.y=FALSE)

save(Trial, file=paste0(ProjectDir, "/Data/Trial_toimpute.rdata"))

####Basic comparison of trial arms####
names(Trial)

#Table 1 - Cohort basics
MyVars<-c("AnyHyp", "AntiHypDoseMiss", "SysMiss", "DiaMiss", "BMIValMiss", "Drug", "PatIMD", "TrialFU", "Died2yr")
MyVars<-c(Confounders, MyVars)

Table1<-CreateTableOne(vars=MyVars,  data=Trial,  strata="Arm", includeNA = TRUE)
print(Table1, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = TRUE)

Table1Exp <- print(Table1,  printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = TRUE, showAllLevels = TRUE)

write.csv(Table1Exp, file = "Outputs/PubTable1.csv")

####Imputation####
#Define imputation data set
ToImpute<-select(Trial, patid, pracid, TimeToMH24, TimeToSH24, TimeToAll24, TimeToMH12, TimeToSH12, TimeToAll12,
                  TimeToMH6, TimeToSH6, TimeToAll6, TimeToMH3, TimeToSH3, TimeToAll3,
                  MHBin12, SHBin12, AllBin12, MHBin6, SHBin6, AllBin6, MHBin3, SHBin3, AllBin3,#Not included
                  Arm, #Exposure
                  TrialFU, Died2yr, HazardMH, HazardSH, HazardAll, MHBin24, SHBin24, AllBin24) #outcomes

ToImputeCov<-select(Trial, all_of(Confounders))
ToImpute<-cbind(ToImpute, ToImputeCov)

sapply(ToImpute, class)

#Set outcomes to factors just for the imputation
ToImpute[,Outcome]<-lapply(ToImpute[,Outcome], factor)
sapply(ToImpute, class)

Basic<-mice(data=ToImpute, m=1, seed=500)
Pred <- Basic$predictorMatrix
Pred

#Set prediction to 0 for the variables we want to be passive
Pred[, c(1:23)] <- 0

Pred

ImputedData<-mice(data=ToImpute, predictorMatrix=Pred, m=10, seed=500)

summary(ImputedData$imp$ethnicity)
summary(ImputedData$imp$BMIVal)
summary(ImputedData$imp$Systolic)
summary(ImputedData$imp$Diastolic)

PredFinal <- ImputedData$predictorMatrix
PredFinal

ImputedData$method

#Set outcomes back to numeric
datlist<-mids2datlist(ImputedData)

datlist<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    mutate_at(Outcome, as.character)%>%
    mutate_at(Outcome, as.numeric)
})

#Need to group some because otherwise not enough power to converge
#Group APs that are less than 30
datlist<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    group_by(AP)%>%
    mutate(n=n())%>%
    mutate(AP = case_when(n<30 ~"Other",
                          AP == "sulpiride" ~"Other",
                          TRUE ~as.character(AP)))%>%
    mutate(AP=as.factor(AP))%>%
    ungroup()%>%
    mutate(ethnicity = case_when(ethnicity!="White" ~"Other",
                                 TRUE ~as.character(ethnicity)))%>%
    mutate(ethnicity=as.factor(ethnicity))%>%
   mutate(MI=as.numeric(as.character(AllMI)), CHF = as.numeric(as.character(AllCHF)), Cereb = as.numeric(as.character(AllCerebrovascular)))%>%
    mutate(Cardio = MI+CHF+Cereb)%>%
    select(-MI, -CHF, -Cereb)%>%
            ungroup()
})

datlist<-lapply(datlist, FUN=function(Data){
  Data<-Data%>%
    mutate(AP=relevel(AP, ref="olanzapine"))
})

save(datlist, file=paste0(ProjectDir, "/Data/ImputedData.rdata"))

####Mental health####
#Kaplan meier
km_fit <- survfit(Surv(TimeToMH12, MHBin12) ~ Arm, data=Trial)

ggsurvplot(km_fit,
          pval = TRUE, conf.int = TRUE,
                     linetype = "strata", # Change line type by groups
                     ggtheme = theme_bw(), # Change ggplot2 theme
           risk.table = TRUE, # Add risk table
                     palette = c("#E7B800", "#2E9FDF"),
                     xlab = "Time in days",
          legend.labs=c("BBB-P" ,"Amlodipine"),
           title = "Psychiatric admissions",
                     ylim=c(0.75,1), xlim=c(0, 370))

CoxMH12 <- coxph(Surv(TimeToMH12, MHBin12) ~ Arm, data = Trial)
summary(CoxMH12) 
Proportional<-cox.zph(CoxMH12)
Proportional
plot(Proportional, main="Psychiatric admissions")

ggsave("Outputs/SchoenfeldPsych.png")

####Self harm####
km_fit <- survfit(Surv(TimeToSH12, SHBin12) ~ Arm, data=Trial)

ggsurvplot(km_fit,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 
           risk.table = TRUE, # Add risk table
           palette = c("#E7B800", "#2E9FDF"),
           title = "Self harm events",
           xlab = "Time in days",
           legend.labs=c("BBB-P" ,"Amlodipine"),
           ylim=c(0.75,1), xlim=c(0, 370))

CoxSH12 <- coxph(Surv(TimeToSH12, SHBin12) ~ Arm, data = Trial)
summary(CoxSH12) 
Proportional<-cox.zph(CoxSH12)
Proportional
plot(Proportional, main="Self harm events")

ggsave("Outputs/SchoenfeldSH.png")

####Combined####
km_fit <- survfit(Surv(TimeToAll12, AllBin12) ~ Arm, data=Trial)

ggsurvplot(km_fit,
           pval = TRUE, conf.int = TRUE,
           linetype = "strata", # Change line type by groups
           ggtheme = theme_bw(), # Change ggplot2 
           risk.table = TRUE, # Add risk table
           palette = c("#E7B800", "#2E9FDF"),
           title = "Combined psychiatric admissions and self-harm events",
           xlab = "Time in days",
           legend.labs=c("BBB-P" ,"Amlodipine"),
           ylim=c(0.75,1), xlim=c(0, 370))

CoxPhysical12 <- coxph(Surv(TimeToAll12, AllBin12) ~ Arm, data = Trial)
summary(CoxPhysical12) 
Proportional<-cox.zph(CoxPhysical12)
Proportional
plot(Proportional, main="Combined psychiatric admissions and self-harm events")

ggsave("Outputs/ShoenfeldComb.png")

####Patients experiencing an event####
EventsTrial<-CreateTableOne(vars=Outcome, strata ="Arm", factorVars = Outcome, data=Trial, includeNA = FALSE)
print(EventsTrial, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, showAllLevels = FALSE)

EventsTrialExp <- print(EventsTrial,  printToggle = FALSE, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, catDigits=2, includeNA = FALSE, showAllLevels = FALSE)

write.csv(EventsTrialExp, file = "Outputs/EventsTrial.csv")

####Incidence####
#Active
Inc<-Trial

#Change 0 to 1 and divide by 365 so by year
Inc[Time]<-lapply(Inc[Time], function(x) {case_when(x==0 ~ 1,
                                                    TRUE ~ x)})
Inc[Time]<-lapply(Inc[Time], function(x) {x/365})

Active<-subset(Inc, Arm=="BBP_CCB")
Comp<-subset(Inc, Arm=="NBBP_CCB")

UnadjResult<-data.frame()

for (i in (1:length(Outcome))) {
  Unadj <- glm(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], "))")), family = poisson, data=Active)
  Unadj<-tidy(Unadj, conf.int=FALSE, exponentiate=TRUE)
  Unadj$Outcome<-Outcome[i]
  UnadjResult<-rbind(UnadjResult, Unadj)
}

UnadjResult$Group<-"Active"

#Comparator

UnadjResultComp<-data.frame()

for (i in (1:length(Outcome))) {
  Unadj <- glm(as.formula(paste0(Outcome[i], "~ offset(log(", Time[i], "))")), family = poisson, data=Comp)
  Unadj<-tidy(Unadj, conf.int=FALSE, exponentiate=TRUE)
  Unadj$Outcome<-Outcome[i]
  UnadjResultComp<-rbind(UnadjResultComp, Unadj)
}
UnadjResultComp$Group<-"Comp"

FinalResults<-rbind(UnadjResult, UnadjResultComp)
FinalResults$estimate<-FinalResults$estimate*100
FinalResults<-dplyr::select(FinalResults, estimate, Outcome, Group)

#Export
write.csv(FinalResults, "Outputs/IncTrial.csv")

####Final models####
#Do it as a loop

UnadjResult<-data.frame()

for (i in (1:length(Outcome))) {
  Unadj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Arm+strata(pracid)")), data=data)
  })
  Unadj<-summary(mice::pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
  Unadj$Outcome<-Outcome[i]
  UnadjResult<-rbind(Unadj, UnadjResult)
  }

TimePoint<-c(replicate(3,"3"),replicate(3,"6"),replicate(3,"24"),replicate(3,"12"))
Group<-c("All","MH", "SH")
Group<-c(replicate(4, Group))
UnadjResult<-cbind(UnadjResult, TimePoint, Group) 
UnadjResult$Result<-paste0(format(round(UnadjResult$estimate, 2), nsmall=2, scientific = FALSE), " (", format(round(UnadjResult$`2.5 %`, 2), nsmall=2, scientific = FALSE), "-", format(round(UnadjResult$`97.5 %`, 2), nsmall=2, scientific = FALSE), ")")

#Adjusted

#Do the first one to create data
AdjResult<-data.frame()

#Then bind them on

for (i in (1:length(Outcome))) {
  Adj <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ (Arm+SMIDiag+", (paste0(Adjust, "+strata(pracid))")))), data=data)
  })
  Adj<-summary(mice::pool(Adj), conf.int=TRUE, exponentiate=TRUE)
  Adj$Outcome<-Outcome[i]
  AdjResult<-rbind(AdjResult, Adj)
}

TimePoint<-c(replicate(129,"12"),replicate(129,"24"),replicate(129,"6"),replicate(129,"3"))
Group<-c(replicate(43, "MH"), replicate(43,"SH"), replicate(43,"All"))
Group<-c(replicate(4, Group))
AdjResult<-cbind(AdjResult, TimePoint, Group) 
AdjResult$Result<-paste0(format(round(AdjResult$estimate, 2), nsmall=2, scientific = FALSE), " (", format(round(AdjResult$`2.5 %`, 2), nsmall=2, scientific = FALSE), "-", format(round(AdjResult$`97.5 %`, 2), nsmall=2, scientific = FALSE), ")")
AdjResult$Result<-str_squish(AdjResult$Result)

write.csv(UnadjResult, "Outputs/TrialUnadj.csv")
write.csv(AdjResult, "Outputs/TrialAdj.csv")

save(UnadjResult, file=paste0(ProjectDir, "/Data/MainUnadj.rdata"))
save(AdjResult, file=paste0(ProjectDir, "/Data/MainAdj.rdata"))    

####Interaction terms####

#Do the first one to create data
IntResult<-data.frame()

#Then bind them on

for (i in (1:length(Outcome))) {
  Int <- lapply(datlist, FUN=function(data){
    coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ (Arm*SMIDiag+", (paste0(Adjust, "+strata(pracid))")))), data=data)
  })
  Int<-summary(mice::pool(Int), conf.int=TRUE, exponentiate=TRUE)
  Int$Outcome<-Outcome[i]
  IntResult<-rbind(IntResult, Int)
}

TimePoint<-c(replicate(135,"12"),replicate(135,"24"),replicate(135,"6"),replicate(135,"3"))
Group<-c(replicate(45, "MH"), replicate(45,"SH"), replicate(45,"All"))
Group<-c(replicate(4, Group))
IntResult<-cbind(IntResult, TimePoint, Group) 
IntResult$Result<-paste0(format(round(IntResult$estimate, 2), nsmall=2, scientific = FALSE), " (", format(round(IntResult$`2.5 %`, 2), nsmall=2, scientific = FALSE), "-", format(round(IntResult$`97.5 %`, 2), nsmall=2, scientific = FALSE), ")")
IntResult$Result<-str_squish(IntResult$Result)

write.csv(IntResult, "Outputs/TrialInt.csv")


####Test 12 month interaction terms####
ImputedData<-datlist2mids(datlist, progress=FALSE)

#Mental
MainMH<-with(data=ImputedData, coxph(as.formula(paste0("Surv(TimeToMH12, MHBin12) ~ Arm+SMIDiag+", (paste0(Adjust, "+strata(pracid)")))))) 

IntMH<-with(data=ImputedData, coxph(as.formula(paste0("Surv(TimeToMH12, MHBin12) ~ Arm*SMIDiag+", (paste0(Adjust, "+strata(pracid)"))))))

#Wald test
D1(IntMH, MainMH)

#SH
MainSH<-with(data=ImputedData, coxph(as.formula(paste0("Surv(TimeToSH12, SHBin12) ~ Arm+SMIDiag+", (paste0(Adjust, "+strata(pracid)"))))))

IntSH<-with(data=ImputedData, coxph(as.formula(paste0("Surv(TimeToSH12, SHBin12) ~ Arm*SMIDiag+", (paste0(Adjust, "+strata(pracid)"))))))

#Wald test
D1(IntSH, MainSH)

#All
MainAll<-with(data=ImputedData, coxph(as.formula(paste0("Surv(TimeToAll12, AllBin12) ~ Arm+SMIDiag+", (paste0(Adjust, "+strata(pracid)")))))) 

IntAll<-with(data=ImputedData, coxph(as.formula(paste0("Surv(TimeToAll12, AllBin12) ~ Arm*SMIDiag+", (paste0(Adjust, "+strata(pracid)"))))))

#Wald test
D1(IntAll, MainAll)
