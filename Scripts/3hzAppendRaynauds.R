####Append Raynauds####

#Call start script
source("./Scripts/1aSetUp.R")

####Load 2023 data####
load(paste0(ProjectDir, "/Data/HospPhys.rdata"))


####Raynauds obs####

load(paste0(ProjectDir, "/Extracts/RayAll.Rdata"))

#create first diagnosis variables
Raynauds<-PatRayAll%>%
  group_by(patid)%>%
  summarise(FirstRaynauds=min(eventdate, na.rm=TRUE))%>%
  subset(FirstRaynauds!=Inf)

#Append first diagnosis dates to trial table
HospAll<-merge(x=HospAll, y=Raynauds, by="patid", all.x=TRUE, all.y=FALSE)

length(unique(HospAll$patid))

save(HospAll, file = paste0(ProjectDir, "/Data/HospRay.rdata"))
