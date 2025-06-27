####Set up project root directories, packages and functions####

#Clear the workspace
rm(list = ls(all.names = TRUE))

#Pull from Git so we are synched

system("git pull")

####Libraries needed for this project####

#General admin libraries
library(tableone)
library(rprojroot)
library(tidyverse)
library(lubridate)
library(mice)
library(miceadds)
library(survival)
library(survminer)

####Set the directories so that I can use relative links####
#Note, these should be deleted before GitHub goes public

root.dir = find_rstudio_root_file()
DataDir = 
ProjectDir = 
OldCodeListDir = 

####Set confounders, outcomes and time variables####
#note doesn't include SMI diagnosis as this is added either as a confounder or as an interaction term
#Doesn't include region as is stratified by practice ID

####Set confounders, outcomes and time variables####
#note adjustment doesn't include SMI diagnosis as this is added either as a confounder or as an interaction term
#Doesn't include region as is stratified by practice ID
#Doesn't include MI, Cereb, CHF as these are grouped as cardio

AdjustSet<-c("AgeAtAntiHyp","CCByear", "SMITime", "TimeSinceAnyHyp", "AntiHypDose", "gender",
             "ethnicity", "PriorMHBin", "PriorSHBin", "PriorGPSHBin","FirstPsychTime", 
             "Systolic", "Diastolic", "BMIVal","SSRI", "TCA", "Other",
             "AP", "PriorPhysicalBin", "PriorAccidentBin", "PriorGP", "FullIMD")

Adjust<-c(AdjustSet, "Cardio")
Adjust<-paste(Adjust, collapse = " + ")

#Confounders do include region, SMI diag and mi/chf/cereb.

Confounders<-c(AdjustSet, "AllMI", "AllCHF", "AllCerebrovascular", "region", "SMIDiag")

Outcome<-c("MHBin12", "SHBin12", "AllBin12", "MHBin24", "SHBin24", "AllBin24", 
           "MHBin6", "SHBin6", "AllBin6", "MHBin3", "SHBin3", "AllBin3")

Time<-c("TimeToMH12", "TimeToSH12", "TimeToAll12", "TimeToMH24", "TimeToSH24", "TimeToAll24",
        "TimeToMH6", "TimeToSH6", "TimeToAll6", "TimeToMH3", "TimeToSH3", "TimeToAll3")