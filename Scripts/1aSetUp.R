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

#Analysis specific libraries

####Set the directories so that I can use relative links####
#Note, these should be deleted before GitHub goes public

root.dir = find_rstudio_root_file()
DataDir = "R:/2023 CPRD"
ProjectDir = "R:/Datamind_Repurposing/Repurposing-CCBs-in-SMI"
OldCodeListDir = "R:/Datamind_Repurposing/Codelists"

####Project specific functions####

#Read and write from .txt files keeping the key fields as character vectors

ReadGoldProdObs<-function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", fill=TRUE, colClasses=c(prodcode="character", patid="character"))
}

ReadGoldMedObs<-function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", fill=TRUE, colClasses=c(medcode="character", patid="character"))
}

ReadAurumMedObs<-function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", fill=TRUE, colClasses=c(medcodeid="character", patid="character"))
}

ReadAurumProdObs<-function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", fill=TRUE, colClasses=c(prodcodeid="character", patid="character"))
}

ReadGoldProdCodelist<-function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", fill=TRUE, colClasses=c(prodcode="character"))
}

ReadGoldMedCodelist<-function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", fill=TRUE, colClasses=c(medcode="character"))
}

ReadAurumMedCodelist<-function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", fill=TRUE, colClasses=c(MedCodeId="character"))
}

ReadAurumProdCodelist<-function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", fill=TRUE, colClasses=c(ProdCodeId="character"))
}

ReadGeneral<-function (x) { 
  read.table(x, header = TRUE, sep = "\t", dec = ".", quote="", fill=TRUE)
}

WriteCodeList<-function (x, y){
  write.table(x, file = y, col.names=TRUE, row.names=FALSE, sep="\t", dec = ".", quote=FALSE)}
