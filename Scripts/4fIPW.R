####Overlap weighted model####

#Call start script
source("./Scripts/4dAnalysisPackages.R")

####Load trials####
load(paste0(ProjectDir, "/Data/ImputedData.rdata"))

Imp<-datlist2mids(datlist)

IPWAdj<-Adjust<-paste(Confounders, collapse = " + ")

Weights <- MatchThem::weightthem(as.formula(paste0("Arm ~ ", IPWAdj, "+Died2yr+TrialFU")), #confounders
                               datasets = Imp, # the mids object created by mice
                                              approach = "within",  # use a within methods rather than across (within methods have been shown to be more consistent)
                                              method = "glm", # default propensity score method which uses logistic regression
                                              include.obj = TRUE, # 
                                              estimand = "ATO") # specify estimand of interest - average weight in the overlap
# Observe success of covariate balance
summary(Weights)

Labs<-c(AgeAtAntiHyp="Age", CCBYear="Year", SMITime="Time since SMI", TimeSinceAnyHyp="Time since hypertension", AntiHypDose="CCB dose", gender="Sex", 
        ethnicity="Ethnicity", Region = "Region", SMIDiag="SMI diagnosis", PriorMHBin="Psychiatric admissions", 
        FirstPsychTime="Time on antipsychotics", PriorSHBin="Self harm admissions", PriorGPSHBin="Self harm events", PriorGP="GP consultations",
        AllMI = "MI", AllCHF = "Heart failure", AllCerebrovascular = "Cerebrovascular disease",
        BMIVal="BMI", SSRI="SSRI", TCAt="Tricyclic", Other="Other antidepressant", AP="Antipsychotic", PriorPhysicalBin="Physical health admission", 
        PriorAccidentBin="Accident/injury admission", FullIMD="IMD", Died2yr="Deaths", TrialFU="Follow up time")

love.plot(Weights, binary="std", abs=TRUE,  var.names=Labs, limits=c(-0.7, 0.7), threshold=c(m = 0.1), sample.names = c("Unweighted", "Overlap weighted"))

Table<-bal.tab(Weights,
               stats = c("m", "ks"),
               imp.fun="max")


Unadj <- with(data=Weights, coxph(Surv(TimeToMH12, MHBin12) ~ Arm+strata(pracid)))
Unadj <- summary(MatchThem::pool(Unadj), conf.int=TRUE, exponentiate=TRUE)

####Weighted model####
FinalWeights<-list(complete(Weights, action=1), complete(Weights, action=2), complete(Weights, action=3), complete(Weights, action=4), 
                complete(Weights, action=5), complete(Weights, action=6), complete(Weights, action=7), complete(Weights, action=8),
                complete(Weights, action=9), complete(Weights, action=10))

summary(FinalWeights[[1]]$weights)
summary(FinalWeights[[2]]$weights)

#Check weights
ggplot(FinalWeights[[1]], aes(x = weights, fill = Arm)) +
  geom_density(alpha = 0.5, colour = "grey50") +
  ggtitle("Distribution of inverse probability weights")

#Run cox and pool. Note, have checked that mice pool creates same results as matchthem pool - matchthem pool does not work on lists
#Do the first one to create data
UnadjResult<-data.frame()

#Then bind them on

for (i in (1:length(Outcome))) {
   Unadj <- lapply(FinalWeights, FUN=function(data){
     coxph(as.formula(paste0("Surv(", Time[i], ",", Outcome[i], ") ~ Arm+strata(pracid)")), data=data, weights=weights)
   })
   Unadj<-summary(mice::pool(Unadj), conf.int=TRUE, exponentiate=TRUE)
   Unadj$Outcome<-Outcome[i]
   UnadjResult<-rbind(UnadjResult,Unadj)
}

TimePoint<-c(replicate(3,"12"),replicate(3,"24"),replicate(3,"6"),replicate(3,"3"))
Group<-c("MH", "SH", "All")
Group<-c(replicate(4, Group))
UnadjResult<-cbind(UnadjResult, TimePoint, Group) 
UnadjResult$Result<-paste0(format(round(UnadjResult$estimate, 2), nsmall=2), " (", format(round(UnadjResult$`2.5 %`, 2), nsmall=2), "-", format(round(UnadjResult$`97.5 %`, 2), nsmall=2), ")")

write.csv(UnadjResult, "Outputs/TrialIPW.csv")
save(UnadjResult, file=paste0(ProjectDir, "/Data/IPW.rdata"))
