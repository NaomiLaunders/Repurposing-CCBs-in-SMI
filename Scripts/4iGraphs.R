####Main result graphs####

#Call start script
source("./Scripts/4dAnalysisPackages.R")

#Load the results
load(paste0(ProjectDir, "/Data/MainUnadj.rdata"))
MainUnadj<-UnadjResult
MainUnadj$Analysis<-"MainUnadj"

load(paste0(ProjectDir, "/Data/MainAdj.rdata"))
MainAdj<-AdjResult
MainAdj$Analysis<-"MainAdj"

load(paste0(ProjectDir, "/Data/PPAUnadj.rdata"))
PPAUnadj<-UnadjResult
PPAUnadj$Analysis<-"PPAUnadj"

load(paste0(ProjectDir, "/Data/PPAAdj.rdata"))
PPAAdj<-AdjResult
PPAAdj$Analysis<-"PPAAdj"

load(paste0(ProjectDir, "/Data/IPW.rdata"))
IPW<-UnadjResult
IPW$Analysis<-"IPW"

load(paste0(ProjectDir, "/Data/IPWPPA.rdata"))
IPWPPA<-UnadjResult
IPWPPA$Analysis<-"IPWPPA"

rm(AdjResult, UnadjResult)

MainAll<-rbind(MainUnadj, MainAdj, IPW)
MainAll<-subset(MainAll, term=="ArmBBP_CCB")
PPAAll<-rbind(PPAUnadj, PPAAdj, IPWPPA)
PPAAll<-subset(PPAAll, term=="ArmBBP_CCB")

#Forest plot for Trial 1 - 12 month
Main12<-subset(MainAll, TimePoint=="12")
Main12$Group<-factor(Main12$Group, level=c("All", "MH", "SH"))

Main12<-Main12%>%
  arrange(Group, TimePoint)

Plot1<-Main12%>%
  mutate(Group=lapply(Group, \(x) case_when(x=="All"  ~ "Psychiatric/self harm",
                                        x=="MH"  ~ "Psychiatric",
                                        x=="SH" ~ "Self-harm",
                                        TRUE ~ "")),
         Analysis=lapply(Analysis, \(x) case_when(x=="MainUnadj" ~ "Unadjusted",
                                               x=="MainAdj" ~ "Adjusted",
                                               x=="IPW" ~ "Overlap weighted",
                                        TRUE ~ "None")))%>%
  arrange(Group, Analysis)

Label<-select(Plot1, Group, Analysis)

Label$Group[Label$Analysis=="Adjusted"|Label$Analysis=="Overlap weighted"]<-""
                                        
forestplot(labeltext = c(Label), boxsize=0.1, line.margin=0.1,xticks.digits = 1,ci.vertices=TRUE,
           hrzl_lines = list("1" = gpar(lty=2), "4" = gpar(lty=2), "7" = gpar(lty=2)), 
           mean = Main12$estimate,
           lower = Main12$`2.5 %`,
           upper = Main12$`97.5 %`,
           zero=1, xlog=TRUE, lwd.ci=gpar(lwd=2), xlab="Hazard ratio", 
           clip = c(0.4,4),
           xticks = c(0.4, 0.8, 1, 1.5, 2, 4),
           title = "Risk of psychiatric symptoms at 12 months in those prescribed BBB-P CCBs\n compared to amlodipine (intention to treat)",
           txt_gp = fpTxtGp(ticks=gpar(fontFamily="", fontsize=8, cex=1.5), label = gpar(fontFamily = "Arial", fontsize=8,cex=1.5), legend=gpar(fontFamily = "Arial", fontsize=8,cex=1.5), xlab=gpar(fontFamily = "Arial", fontsize=8,cex=1.5)))%>%
  fp_set_zebra_style("#f9f9f9")

####12 month PPA####
PPA12<-subset(PPAAll, TimePoint=="12")
PPA12$Group<-factor(PPA12$Group, level=c("All", "MH", "SH"))

PPA12<-PPA12%>%
  arrange(Group, TimePoint)

Plot2<-PPA12%>%
  mutate(Group=lapply(Group, \(x) case_when(x=="All"  ~ "Psychiatric/self harm",
                                            x=="MH"  ~ "Psychiatric",
                                            x=="SH" ~ "Self-harm",
                                            TRUE ~ "")),
         Analysis=lapply(Analysis, \(x) case_when(x=="PPAUnadj" ~ "Unadjusted",
                                                  x=="PPAAdj" ~ "Adjusted",
                                                  x=="IPWPPA" ~ "Overlap weighted",
                                                  TRUE ~ "None")))%>%
  arrange(Group, Analysis)

Label<-select(Plot2, Group, Analysis)

Label$Group[Label$Analysis=="Adjusted"|Label$Analysis=="Overlap weighted"]<-""

forestplot(labeltext = c(Label), boxsize=0.1, line.margin=0.1,xticks.digits = 1,ci.vertices=TRUE,
           hrzl_lines = list("1" = gpar(lty=2), "4" = gpar(lty=2), "7" = gpar(lty=2)), 
           mean = PPA12$estimate,
           lower = PPA12$`2.5 %`,
           upper = PPA12$`97.5 %`,
           zero=1, xlog=TRUE, lwd.ci=gpar(lwd=2), xlab="Hazard ratio", 
           clip = c(0.4,4),
           title = "Risk of psychiatric symptoms at 12 months in those prescribed BBB-P CCBs \n compared to amlodipine (per protocol)",
           xticks = c(0.4, 0.8, 1, 1.5, 2, 4),
           txt_gp = fpTxtGp(ticks=gpar(fontFamily="", fontsize=8, cex=1.5), label = gpar(fontFamily = "Arial", fontsize=8,cex=1.5), legend=gpar(fontFamily = "Arial", fontsize=8,cex=1.5), xlab=gpar(fontFamily = "Arial", fontsize=8,cex=1.5)))%>%
  fp_set_zebra_style("#f9f9f9")





 