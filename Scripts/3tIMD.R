####Append IMD####
#Call start script
source("./Scripts/1aSetUp.R")

load(paste0(ProjectDir, "/Data/Trial_Time.rdata"))

####How many are eligible####
table(Trial$lsoa_e)
PatIMDA<-read.table(paste0(DataDir,"/Linkages/Results/Aurum_linked/Final/IMD/patient_2019_imd_21_000729.txt"), header=TRUE, fill=TRUE, sep="\t", dec = ".", colClasses = c(patid="character", pracid="character"))
PracIMDA<-read.table(paste0(DataDir,"/Linkages/Results/Aurum_linked/Final/IMD/practice_imd_21_000729.txt"), header=TRUE, fill=TRUE, sep="\t", dec = ".", colClasses = c(pracid="character"))

PatIMDG<-read.table(paste0(DataDir, "/Linkages/Results/Final Gold/IMD/patient_2019_imd_21_000729.txt"), header=TRUE, fill=TRUE, sep="\t", dec = ".", colClasses = c(patid="character", pracid="character"))
PracIMDG<-read.table(paste0(DataDir,"/Linkages/Results/Final Gold/IMD/practice_imd_21_000729.txt"), header=TRUE, fill=TRUE, sep="\t", dec = ".", colClasses = c(pracid="character"))

PatIMDA$patid<-paste0(PatIMDA$patid, "-A")
PatIMDG$patid<-paste0(PatIMDG$patid, "-G")

PracIMDA$pracid<-paste0(PracIMDA$pracid, "-A")
PracIMDG$pracid<-paste0(PracIMDG$pracid, "-G")

PatIMD<-rbind(PatIMDA, PatIMDG)
PatIMD<-select(PatIMD, patid, PatIMD=e2019_imd_5)
PracIMD<-rbind(PracIMDA, PracIMDG)
PracIMD<-select(PracIMD, pracid, PracIMD=e2019_imd_5)

Trial<-merge(x=Trial, y=PatIMD, by="patid", all.x=TRUE, all.y=FALSE)
Trial<-merge(x=Trial, y=PracIMD, by="pracid", all.x=TRUE, all.y=FALSE)

save(Trial, file=paste0(ProjectDir, "/Data/Trial_IMD.rdata"))
