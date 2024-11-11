#USDA project
#Tolerance data analyses - for PAA paper

#Last updated: 11/11/2024 MLR

#Set working directory
setwd("your folder") #Change for folder where you have the data

#Attach libraries
library(readxl)
library(ggplot2)
library(svglite)
library(psych)
library(agricolae)
library(dplyr)
library(rstatix)
library(FSA)
library(rcompanion)
library(ggpubr)

#### PAA 250 ppm data ####
### Biofilm and Planktonic together ###
#Input data
paa_both<-read_excel("Results_tolerance_PAA.xlsx", sheet=1, col_names = TRUE)
paa_both<-subset(paa_both, Timepoint!=2)
paa_both$Timepoint<-as.factor(paa_both$Timepoint)
paa_both$factorABC <- with(paa_both, interaction(Trt, Timepoint, Form))

#Make die-off line plots
#Calculate summary statistics - APC
stat_paa_both<-describeBy(paa_both$logAPC, list(paa_both$Trt, paa_both$Timepoint, paa_both$Form), mat = TRUE) #By Timepoint, treatement and type
stat_paa_both$group2<-as.numeric(stat_paa_both$group2)
stat_paa_both <- stat_paa_both[order(stat_paa_both$group1),]
stat_paa_both$Number<-c(rep(1,8),rep(2,16),rep(3,8), rep(2,8), rep(3,16), rep(4,8), rep(3,8),rep(4,16), rep(5,8),rep(2,8), rep(3,16), rep(4,8))
stat_paa_both<-subset(stat_paa_both, vars !="NA")

#Plot - APC
apc_paa_both<-ggplot(stat_paa_both, aes(x=group2, y=mean, group=interaction(group1,group3), color=group1, linetype=group3, shape=group3))+
  geom_point()+geom_line()+facet_wrap(vars(group1))+
  geom_text(data = subset(stat_paa_both, group2 == 2), aes(label = group1, colour = group1, x = 2.05, y = mean), hjust = -.1, size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, linetype=1)+
  geom_hline(yintercept = 0.6, color="grey80", linetype=2)+
  geom_hline(yintercept = 1.9, color="grey80", linetype=3)+
  scale_x_continuous(limits = c(0,1.5), breaks = c(0,0.5,1,1.5))+
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10))+
  theme(plot.margin=margin(t=0.5, b=0.5, l=0.5, r=0.5, unit = 'in'))+
  theme(axis.text = element_text(color='black', size=13), axis.ticks = element_line(color='black')) +
  theme(axis.title = element_text(size=13,color='black'), axis.text.x = element_text(angle=90)) +
  theme(panel.background = element_rect(fill='white', color = NA),
        plot.background = element_rect(fill = 'white',color = NA), 
        panel.border = element_rect(color='black',fill = NA,size=1))+
  #theme(panel.grid.major = element_line(color = "grey50"), panel.grid.minor.y = element_line(color = "grey50"))+
  theme(strip.background= element_blank(), strip.text = element_text(size=15),
        panel.border = element_rect(color="black", fill=NA))+
  theme(legend.position = "bottom")+
  ylab("log10 APC/peg biofilms or log 10 APC/ml planktonic")+xlab("Time (hours)")+
  ggtitle("PAA Aerobic plate count - Tolerance Both")+theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
apc_paa_both
ggsave("Tolerance_APC_PAA.png", plot=apc_paa_both, device="png", width=10, height=8, units="in", dpi=600)
ggsave("Tolerance_APC_PAA.svg", plot=apc_paa_both, device="svg", width=10, height=8, units="in", dpi=600)



#Calculate summary statistics - MPN
stat_mpn_paa_both<-describeBy(paa_both$logMPN, list(paa_both$Trt, paa_both$Timepoint, paa_both$Form), mat = TRUE) #By Timepoint, treatement and type
stat_mpn_paa_both$group2<-as.numeric(stat_mpn_paa_both$group2)
stat_mpn_paa_both <- stat_mpn_paa_both[order(stat_mpn_paa_both$group1),]
stat_mpn_paa_both$Number<-c(rep(1,8),rep(2,16),rep(3,8), rep(2,8), rep(3,16), rep(4,8), rep(3,8),rep(4,16), rep(5,8),rep(2,8), rep(3,16), rep(4,8))
stat_mpn_paa_both<-subset(stat_mpn_paa_both, vars !="NA")
stat_mpn_paa_both$group2<-as.numeric(stat_mpn_paa_both$group2)

mpn_paa_both<-ggplot(stat_mpn_paa_both, aes(x=group2, y=mean, group=interaction(group1,group3), color=group1, linetype=group3, shape=group3))+
  geom_point()+geom_line()+facet_wrap(vars(group1))+
  geom_text(data = subset(stat_mpn_paa_both, group2 == 2), aes(label = group1, colour = group1, x = 2.05, y = mean), hjust = -.1, size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2)+
  geom_hline(yintercept = 0.9, color="grey80", linetype=2)+ #LOD for biofilm
  geom_hline(yintercept = 1.15, color="darkgreen", linetype=3)+ #LOD for planktonic
  scale_x_continuous(limits = c(0,1.5), breaks = c(0,0.5,1,1.5))+
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10))+
  theme(plot.margin=margin(t=0.5, b=0.5, l=0.5, r=0.5, unit = 'in'))+
  theme(axis.text = element_text(color='black', size=13), axis.ticks = element_line(color='black')) +
  theme(axis.title = element_text(size=13,color='black')) +
  theme(panel.background = element_rect(fill='white', color = NA),
        plot.background = element_rect(fill = 'white',color = NA), 
        panel.border = element_rect(color='black',fill = NA,size=1))+
  #theme(panel.grid.major = element_line(color = "grey50"), panel.grid.minor.y = element_line(color = "grey50"))+
  theme(strip.background= element_blank(), strip.text = element_text(size=15),
        panel.border = element_rect(color="black", fill=NA))+
  theme(legend.position = "bottom")+
  ylab("log10 MPN/peg biofilms or log10 MPN/ml planktonic")+xlab("Time (hours)")+
  ggtitle("L. monocytogenes MPN - Tolerance PAA")+theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
mpn_paa_both
ggsave("Tolerance_MPN_PAA.png", plot=mpn_paa_both, device="png", width=10, height=8, units="in", dpi=600)
ggsave("Tolerance_MPN_PAA.svg", plot=mpn_paa_both, device="svg", width=10, height=8, units="in", dpi=600)



#Check ANOVA assumptions
#MPN
#Normality check using anova model residuals
paa_apc<-lm(logMPN ~ factorABC, data = paa_both)
ggqqplot(residuals(paa_apc))  
shapiro_test(residuals(paa_apc)) #significant Shapiro test for normality

#Check homogeneity of variace assumption
plot(paa_apc, 1)
paa_both %>% levene_test(logMPN ~ factorABC) #non-significant Levene test

#MPN
#Normality check using anova model residuals
paa_mpn<-lm(logMPN ~ factorABC, data = paa_both)
ggqqplot(residuals(paa_mpn))  
shapiro_test(residuals(paa_mpn)) #significant Shapiro test for normality

#Check homogeneity of variace assumption
plot(paa_mpn, 1)
paa_both %>% levene_test(logMPN ~ factorABC) #significant Levene test

#Data does not follow ANOVA assumptions.
#Run Kruskal-Wallis test on each independent data set



#Subset by treatment and form (i.e., plank and biofilm)
T1_biofilm<-subset(paa_both, Code=="T1" & Form=="Biofilm")
T6_biofilm<-subset(paa_both, Code=="T6" & Form=="Biofilm")
T7_biofilm<-subset(paa_both, Code=="T7" & Form=="Biofilm")
T8_biofilm<-subset(paa_both, Code=="T8" & Form=="Biofilm")
T9_biofilm<-subset(paa_both, Code=="T9" & Form=="Biofilm")
T10_biofilm<-subset(paa_both, Code=="T10" & Form=="Biofilm")
T11_biofilm<-subset(paa_both, Code=="T11" & Form=="Biofilm")
T12_biofilm<-subset(paa_both, Code=="T12" & Form=="Biofilm")
T13_biofilm<-subset(paa_both, Code=="T13" & Form=="Biofilm")
T14_biofilm<-subset(paa_both, Code=="T14" & Form=="Biofilm")
T15_biofilm<-subset(paa_both, Code=="T15" & Form=="Biofilm")
T16_biofilm<-subset(paa_both, Code=="T16" & Form=="Biofilm")
T17_biofilm<-subset(paa_both, Code=="T17" & Form=="Biofilm")
T18_biofilm<-subset(paa_both, Code=="T18" & Form=="Biofilm")
T19_biofilm<-subset(paa_both, Code=="T19" & Form=="Biofilm")
T20_biofilm<-subset(paa_both, Code=="T20" & Form=="Biofilm")

T1_plank<-subset(paa_both, Code=="T1" & Form=="Planktonic")
T6_plank<-subset(paa_both, Code=="T6" & Form=="Planktonic")
T7_plank<-subset(paa_both, Code=="T7" & Form=="Planktonic")
T8_plank<-subset(paa_both, Code=="T8" & Form=="Planktonic")
T9_plank<-subset(paa_both, Code=="T9" & Form=="Planktonic")
T10_plank<-subset(paa_both, Code=="T10" & Form=="Planktonic")
T11_plank<-subset(paa_both, Code=="T11" & Form=="Planktonic")
T12_plank<-subset(paa_both, Code=="T12" & Form=="Planktonic")
T13_plank<-subset(paa_both, Code=="T13" & Form=="Planktonic")
T14_plank<-subset(paa_both, Code=="T14" & Form=="Planktonic")
T15_plank<-subset(paa_both, Code=="T15" & Form=="Planktonic")
T16_plank<-subset(paa_both, Code=="T16" & Form=="Planktonic")
T17_plank<-subset(paa_both, Code=="T17" & Form=="Planktonic")
T18_plank<-subset(paa_both, Code=="T18" & Form=="Planktonic")
T19_plank<-subset(paa_both, Code=="T19" & Form=="Planktonic")
T20_plank<-subset(paa_both, Code=="T20" & Form=="Planktonic")


#Run Kruskal-Wallis test
#MPN-biof
T1_biof_apc.kruskal <- T1_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T1_biof_apc.kruskal #p = 0.0666

T6_biof_apc.kruskal <- T6_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T6_biof_apc.kruskal #p = 0.026

T7_biof_apc.kruskal <- T7_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T7_biof_apc.kruskal #p = 0.0254

T8_biof_apc.kruskal <- T8_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T8_biof_apc.kruskal #p = 0.0132

T9_biof_apc.kruskal <- T9_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T9_biof_apc.kruskal #p = 0.0254

T10_biof_apc.kruskal <- T10_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T10_biof_apc.kruskal #p = 0.0666

T11_biof_apc.kruskal <- T11_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T11_biof_apc.kruskal #p = 0.0176

T12_biof_apc.kruskal <- T12_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T12_biof_apc.kruskal #p = 0.0172

T13_biof_apc.kruskal <- T13_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T13_biof_apc.kruskal #p = 0.0254

T14_biof_apc.kruskal <- T14_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T14_biof_apc.kruskal #p = 0.0666

T15_biof_apc.kruskal <- T15_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T15_biof_apc.kruskal #p = 0.0132

T16_biof_apc.kruskal <- T16_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T16_biof_apc.kruskal #p = 0.0216

T17_biof_apc.kruskal <- T17_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T17_biof_apc.kruskal #p = 0.0145

T18_biof_apc.kruskal <- T18_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T18_biof_apc.kruskal #p = 0.0153

T19_biof_apc.kruskal <- T19_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T19_biof_apc.kruskal #p = 0.0254

T20_biof_apc.kruskal <- T20_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T20_biof_apc.kruskal #p = 0.0188


#MPN-Plank
T1_plank_apc.kruskal <- T1_plank %>% kruskal_test(logMPN ~ Timepoint)
T1_plank_apc.kruskal #p = 0.0132

T6_plank_apc.kruskal <- T6_plank %>% kruskal_test(logMPN ~ Timepoint)
T6_plank_apc.kruskal #p = 0.0132

T7_plank_apc.kruskal <- T7_plank %>% kruskal_test(logMPN ~ Timepoint)
T7_plank_apc.kruskal #p = 0.0132

T8_plank_apc.kruskal <- T8_plank %>% kruskal_test(logMPN ~ Timepoint)
T8_plank_apc.kruskal #p = 0.0132

T9_plank_apc.kruskal <- T9_plank %>% kruskal_test(logMPN ~ Timepoint)
T9_plank_apc.kruskal #p = 0.0132

T10_plank_apc.kruskal <- T10_plank %>% kruskal_test(logMPN ~ Timepoint)
T10_plank_apc.kruskal #p = 0.0132

T11_plank_apc.kruskal <- T11_plank %>% kruskal_test(logMPN ~ Timepoint)
T11_plank_apc.kruskal #p = 0.0132

T12_plank_apc.kruskal <- T12_plank %>% kruskal_test(logMPN ~ Timepoint)
T12_plank_apc.kruskal #p = 0.0132

T13_plank_apc.kruskal <- T13_plank %>% kruskal_test(logMPN ~ Timepoint)
T13_plank_apc.kruskal #p = 0.0132

T14_plank_apc.kruskal <- T14_plank %>% kruskal_test(logMPN ~ Timepoint)
T14_plank_apc.kruskal #p = 0.0132

T15_plank_apc.kruskal <- T15_plank %>% kruskal_test(logMPN ~ Timepoint)
T15_plank_apc.kruskal #p = 0.0132

T16_plank_apc.kruskal <- T16_plank %>% kruskal_test(logMPN ~ Timepoint)
T16_plank_apc.kruskal #p = 0.0132

T17_plank_apc.kruskal <- T17_plank %>% kruskal_test(logMPN ~ Timepoint)
T17_plank_apc.kruskal #p = 0.0145

T18_plank_apc.kruskal <- T18_plank %>% kruskal_test(logMPN ~ Timepoint)
T18_plank_apc.kruskal #p = 0.0132

T19_plank_apc.kruskal <- T19_plank %>% kruskal_test(logMPN ~ Timepoint)
T19_plank_apc.kruskal #p = 0.0132

T20_plank_apc.kruskal <- T20_plank %>% kruskal_test(logMPN ~ Timepoint)
T20_plank_apc.kruskal #p = 0.0132


#Calculate effect size
#MPN-biof
T1_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T6_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T7_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T8_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T9_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T10_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T11_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T12_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T13_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T14_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T15_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T16_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T17_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T18_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T19_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T20_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)

#MPN-plank
T1_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T6_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T7_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T8_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T9_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T10_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T11_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T12_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T13_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T14_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T15_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T16_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T17_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T18_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T19_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T20_plank %>% kruskal_effsize(logMPN ~ Timepoint)

# Pairwise comparisons
DT.apc_T1_plank = dunnTest(logMPN ~ Timepoint, data=T1_plank, method = "none")   
DT.apc_T6_plank = dunnTest(logMPN ~ Timepoint, data=T6_plank, method = "none")   
DT.apc_T7_plank = dunnTest(logMPN ~ Timepoint, data=T7_plank, method = "none")   
DT.apc_T8_plank = dunnTest(logMPN ~ Timepoint, data=T8_plank, method = "none")   
DT.apc_T9_plank = dunnTest(logMPN ~ Timepoint, data=T9_plank, method = "none")   
DT.apc_T10_plank = dunnTest(logMPN ~ Timepoint, data=T10_plank, method = "none")   
DT.apc_T11_plank = dunnTest(logMPN ~ Timepoint, data=T11_plank, method = "none")   
DT.apc_T12_plank = dunnTest(logMPN ~ Timepoint, data=T12_plank, method = "none")   
DT.apc_T13_plank = dunnTest(logMPN ~ Timepoint, data=T13_plank, method = "none")   
DT.apc_T14_plank = dunnTest(logMPN ~ Timepoint, data=T14_plank, method = "none")   
DT.apc_T15_plank = dunnTest(logMPN ~ Timepoint, data=T15_plank, method = "none")   
DT.apc_T16_plank = dunnTest(logMPN ~ Timepoint, data=T16_plank, method = "none")   
DT.apc_T17_plank = dunnTest(logMPN ~ Timepoint, data=T17_plank, method = "none")   
DT.apc_T18_plank = dunnTest(logMPN ~ Timepoint, data=T18_plank, method = "none")   
DT.apc_T19_plank = dunnTest(logMPN ~ Timepoint, data=T19_plank, method = "none")   
DT.apc_T20_plank = dunnTest(logMPN ~ Timepoint, data=T20_plank, method = "none")   

DT.apc_T1_biof = dunnTest(logMPN ~ Timepoint, data=T1_biofilm, method = "none")   
DT.apc_T6_biof = dunnTest(logMPN ~ Timepoint, data=T6_biofilm, method = "none")   
DT.apc_T7_biof = dunnTest(logMPN ~ Timepoint, data=T7_biofilm, method = "none")   
DT.apc_T8_biof = dunnTest(logMPN ~ Timepoint, data=T8_biofilm, method = "none")   
DT.apc_T9_biof = dunnTest(logMPN ~ Timepoint, data=T9_biofilm, method = "none")   
DT.apc_T10_biof = dunnTest(logMPN ~ Timepoint, data=T10_biofilm, method = "none")   
DT.apc_T11_biof = dunnTest(logMPN ~ Timepoint, data=T11_biofilm, method = "none")   
DT.apc_T12_biof = dunnTest(logMPN ~ Timepoint, data=T12_biofilm, method = "none")   
DT.apc_T13_biof = dunnTest(logMPN ~ Timepoint, data=T13_biofilm, method = "none")   
DT.apc_T14_biof = dunnTest(logMPN ~ Timepoint, data=T14_biofilm, method = "none")   
DT.apc_T15_biof = dunnTest(logMPN ~ Timepoint, data=T15_biofilm, method = "none")   
DT.apc_T16_biof = dunnTest(logMPN ~ Timepoint, data=T16_biofilm, method = "none")   
DT.apc_T17_biof = dunnTest(logMPN ~ Timepoint, data=T17_biofilm, method = "none")   
DT.apc_T18_biof = dunnTest(logMPN ~ Timepoint, data=T18_biofilm, method = "none")   
DT.apc_T19_biof = dunnTest(logMPN ~ Timepoint, data=T19_biofilm, method = "none")   
DT.apc_T20_biof = dunnTest(logMPN ~ Timepoint, data=T20_biofilm, method = "none")   

DunnLetters.apc_T1_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T1_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T6_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T6_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T7_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T7_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T8_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T8_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T9_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T9_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T10_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T10_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T11_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T11_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T12_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T12_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T13_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T13_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T14_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T14_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T15_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T15_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T16_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T16_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T17_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T17_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T18_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T18_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T19_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T19_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T20_biof<-cldList(P.adj ~ Comparison,data = DT.apc_T20_biof$res,threshold = 0.05, remove.zero = FALSE)

DunnLetters.apc_T1_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T1_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T6_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T6_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T7_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T7_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T8_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T8_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T9_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T9_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T10_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T10_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T11_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T11_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T12_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T12_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T13_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T13_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T14_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T14_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T15_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T15_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T16_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T16_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T17_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T17_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T18_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T18_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T19_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T19_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.apc_T20_plank<-cldList(P.adj ~ Comparison,data = DT.apc_T20_plank$res,threshold = 0.05, remove.zero = FALSE)


#MPN-biof
T1_biof_mpn.kruskal <- T1_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T1_biof_mpn.kruskal #p = 0.0132

T6_biof_mpn.kruskal <- T6_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T6_biof_mpn.kruskal #p = 0.0217

T7_biof_mpn.kruskal <- T7_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T7_biof_mpn.kruskal #p = 0.0254

T8_biof_mpn.kruskal <- T8_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T8_biof_mpn.kruskal #p = 0.0129

T9_biof_mpn.kruskal <- T9_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T9_biof_mpn.kruskal #p = 0.0132

T10_biof_mpn.kruskal <- T10_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T10_biof_mpn.kruskal #p = 0.0158

T11_biof_mpn.kruskal <- T11_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T11_biof_mpn.kruskal #p = 0.0226

T12_biof_mpn.kruskal <- T12_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T12_biof_mpn.kruskal #p = 0.0172

T13_biof_mpn.kruskal <- T13_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T13_biof_mpn.kruskal #p = 0.0254

T14_biof_mpn.kruskal <- T14_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T14_biof_mpn.kruskal #p = 0.0129

T15_biof_mpn.kruskal <- T15_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T15_biof_mpn.kruskal #p = 0.0132

T16_biof_mpn.kruskal <- T16_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T16_biof_mpn.kruskal #p = 0.029

T17_biof_mpn.kruskal <- T17_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T17_biof_mpn.kruskal #p = 0.0173

T18_biof_mpn.kruskal <- T18_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T18_biof_mpn.kruskal #p = 0.0252

T19_biof_mpn.kruskal <- T19_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T19_biof_mpn.kruskal #p = 0.0132

T20_biof_mpn.kruskal <- T20_biofilm %>% kruskal_test(logMPN ~ Timepoint)
T20_biof_mpn.kruskal #p = 0.0173


#MPN-Plank
T1_plank_mpn.kruskal <- T1_plank %>% kruskal_test(logMPN ~ Timepoint)
T1_plank_mpn.kruskal #p = 0.0132

T6_plank_mpn.kruskal <- T6_plank %>% kruskal_test(logMPN ~ Timepoint)
T6_plank_mpn.kruskal #p = 0.0132

T7_plank_mpn.kruskal <- T7_plank %>% kruskal_test(logMPN ~ Timepoint)
T7_plank_mpn.kruskal #p = 0.0132

T8_plank_mpn.kruskal <- T8_plank %>% kruskal_test(logMPN ~ Timepoint)
T8_plank_mpn.kruskal #p = 0.0132

T9_plank_mpn.kruskal <- T9_plank %>% kruskal_test(logMPN ~ Timepoint)
T9_plank_mpn.kruskal #p = 0.0132

T10_plank_mpn.kruskal <- T10_plank %>% kruskal_test(logMPN ~ Timepoint)
T10_plank_mpn.kruskal #p = 0.0132

T11_plank_mpn.kruskal <- T11_plank %>% kruskal_test(logMPN ~ Timepoint)
T11_plank_mpn.kruskal #p = 0.0132

T12_plank_mpn.kruskal <- T12_plank %>% kruskal_test(logMPN ~ Timepoint)
T12_plank_mpn.kruskal #p = 0.0132

T13_plank_mpn.kruskal <- T13_plank %>% kruskal_test(logMPN ~ Timepoint)
T13_plank_mpn.kruskal #p = 0.0132

T14_plank_mpn.kruskal <- T14_plank %>% kruskal_test(logMPN ~ Timepoint)
T14_plank_mpn.kruskal #p = 0.0129

T15_plank_mpn.kruskal <- T15_plank %>% kruskal_test(logMPN ~ Timepoint)
T15_plank_mpn.kruskal #p = 0.0132

T16_plank_mpn.kruskal <- T16_plank %>% kruskal_test(logMPN ~ Timepoint)
T16_plank_mpn.kruskal #p = 0.0132

T17_plank_mpn.kruskal <- T17_plank %>% kruskal_test(logMPN ~ Timepoint)
T17_plank_mpn.kruskal #p = 0.0132

T18_plank_mpn.kruskal <- T18_plank %>% kruskal_test(logMPN ~ Timepoint)
T18_plank_mpn.kruskal #p = 0.0132

T19_plank_mpn.kruskal <- T19_plank %>% kruskal_test(logMPN ~ Timepoint)
T19_plank_mpn.kruskal #p = 0.0132

T20_plank_mpn.kruskal <- T20_plank %>% kruskal_test(logMPN ~ Timepoint)
T20_plank_mpn.kruskal #p = 0.0132


#Calculate effect size
#MPN-biof
T1_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T6_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T7_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T8_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T9_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T10_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T11_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T12_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T13_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T14_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T15_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T16_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T17_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T18_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T19_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)
T20_biofilm %>% kruskal_effsize(logMPN ~ Timepoint)

#MPN-plank
T1_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T6_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T7_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T8_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T9_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T10_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T11_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T12_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T13_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T14_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T15_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T16_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T17_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T18_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T19_plank %>% kruskal_effsize(logMPN ~ Timepoint)
T20_plank %>% kruskal_effsize(logMPN ~ Timepoint)

# Pairwise comparisons
DT.mpn_T1_biof = dunnTest(logMPN ~ Timepoint, data=T1_biofilm, method = "none")   
DT.mpn_T6_biof = dunnTest(logMPN ~ Timepoint, data=T6_biofilm, method = "none")   
DT.mpn_T7_biof = dunnTest(logMPN ~ Timepoint, data=T7_biofilm, method = "none")   
DT.mpn_T8_biof = dunnTest(logMPN ~ Timepoint, data=T8_biofilm, method = "none")   
DT.mpn_T9_biof = dunnTest(logMPN ~ Timepoint, data=T9_biofilm, method = "none")   
DT.mpn_T10_biof = dunnTest(logMPN ~ Timepoint, data=T10_biofilm, method = "none")   
DT.mpn_T11_biof = dunnTest(logMPN ~ Timepoint, data=T11_biofilm, method = "none")   
DT.mpn_T12_biof = dunnTest(logMPN ~ Timepoint, data=T12_biofilm, method = "none")   
DT.mpn_T13_biof = dunnTest(logMPN ~ Timepoint, data=T13_biofilm, method = "none")   
DT.mpn_T14_biof = dunnTest(logMPN ~ Timepoint, data=T14_biofilm, method = "none")   
DT.mpn_T15_biof = dunnTest(logMPN ~ Timepoint, data=T15_biofilm, method = "none")   
DT.mpn_T16_biof = dunnTest(logMPN ~ Timepoint, data=T16_biofilm, method = "none")   
DT.mpn_T17_biof = dunnTest(logMPN ~ Timepoint, data=T17_biofilm, method = "none")   
DT.mpn_T18_biof = dunnTest(logMPN ~ Timepoint, data=T18_biofilm, method = "none")   
DT.mpn_T19_biof = dunnTest(logMPN ~ Timepoint, data=T19_biofilm, method = "none")   
DT.mpn_T20_biof = dunnTest(logMPN ~ Timepoint, data=T20_biofilm, method = "none")   

DT.mpn_T1_plank = dunnTest(logMPN ~ Timepoint, data=T1_plank, method = "none")   
DT.mpn_T6_plank = dunnTest(logMPN ~ Timepoint, data=T6_plank, method = "none")   
DT.mpn_T7_plank = dunnTest(logMPN ~ Timepoint, data=T7_plank, method = "none")   
DT.mpn_T8_plank = dunnTest(logMPN ~ Timepoint, data=T8_plank, method = "none")   
DT.mpn_T9_plank = dunnTest(logMPN ~ Timepoint, data=T9_plank, method = "none")   
DT.mpn_T10_plank = dunnTest(logMPN ~ Timepoint, data=T10_plank, method = "none")   
DT.mpn_T11_plank = dunnTest(logMPN ~ Timepoint, data=T11_plank, method = "none")   
DT.mpn_T12_plank = dunnTest(logMPN ~ Timepoint, data=T12_plank, method = "none")   
DT.mpn_T13_plank = dunnTest(logMPN ~ Timepoint, data=T13_plank, method = "none")   
DT.mpn_T14_plank = dunnTest(logMPN ~ Timepoint, data=T14_plank, method = "none")   
DT.mpn_T15_plank = dunnTest(logMPN ~ Timepoint, data=T15_plank, method = "none")   
DT.mpn_T16_plank = dunnTest(logMPN ~ Timepoint, data=T16_plank, method = "none")   
DT.mpn_T17_plank = dunnTest(logMPN ~ Timepoint, data=T17_plank, method = "none")   
DT.mpn_T18_plank = dunnTest(logMPN ~ Timepoint, data=T18_plank, method = "none")   
DT.mpn_T19_plank = dunnTest(logMPN ~ Timepoint, data=T19_plank, method = "none")   
DT.mpn_T20_plank = dunnTest(logMPN ~ Timepoint, data=T20_plank, method = "none")   

DunnLetters.mpn_T1_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T1_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T6_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T6_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T7_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T7_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T8_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T8_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T9_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T9_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T10_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T10_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T11_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T11_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T12_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T12_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T13_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T13_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T14_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T14_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T15_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T15_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T16_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T16_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T17_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T17_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T18_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T18_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T19_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T19_biof$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T20_biof<-cldList(P.adj ~ Comparison,data = DT.mpn_T20_biof$res,threshold = 0.05, remove.zero = FALSE)


DunnLetters.mpn_T1_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T1_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T6_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T6_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T7_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T7_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T8_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T8_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T9_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T9_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T10_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T10_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T11_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T11_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T12_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T12_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T13_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T13_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T14_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T14_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T15_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T15_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T16_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T16_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T17_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T17_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T18_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T18_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T19_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T19_plank$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.mpn_T20_plank<-cldList(P.adj ~ Comparison,data = DT.mpn_T20_plank$res,threshold = 0.05, remove.zero = FALSE)


#### BAC 200 ppm data ####

#Input data
bac<-read_excel("Results_tolerance_PAA.xlsx", sheet=3, col_names = TRUE)
bac$Timepoint<-as.factor(bac$Timepoint)
bac$factorAB <- with(bac, interaction(Trt, Timepoint))

#Make die-off line plot
#Calculate summary statistics - MPN
stat_mpn_bac<-describeBy(bac$logMPN, list(bac$Trt, bac$Timepoint), mat = TRUE) #By Timepoint, treatement and type
stat_mpn_bac$group2<-as.numeric(stat_mpn_bac$group2)
stat_mpn_bac <- stat_mpn_bac[order(stat_mpn_bac$group1),]
stat_mpn_bac$group2<-as.numeric(stat_mpn_bac$group2)

mpn_bac<-ggplot(stat_mpn_bac, aes(x=group2, y=mean, group=group1, color=group1))+
  geom_point()+geom_line()+facet_wrap(vars(group1))+
  #geom_text(data = subset(stat_mpn_bac, group2 == 2), aes(label = group1, colour = group1, x = 2.05, y = mean), hjust = -.1, size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1)+
  geom_hline(yintercept = 0.85, color="grey80", linetype=2)+ #LOD for biofilm
  scale_x_continuous(limits = c(0,45), breaks = c(0,15,30,45))+
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10))+
  theme(plot.margin=margin(t=0.5, b=0.5, l=0.5, r=0.5, unit = 'in'))+
  theme(axis.text = element_text(color='black', size=13), axis.ticks = element_line(color='black')) +
  theme(axis.title = element_text(size=13,color='black')) +
  theme(panel.background = element_rect(fill='white', color = NA),
        plot.background = element_rect(fill = 'white',color = NA), 
        panel.border = element_rect(color='black',fill = NA,size=1))+
  #theme(panel.grid.major = element_line(color = "grey50"), panel.grid.minor.y = element_line(color = "grey50"))+
  theme(strip.background= element_blank(), strip.text = element_text(size=15),
        panel.border = element_rect(color="black", fill=NA))+
  theme(legend.position = "bottom")+
  ylab("log10 MPN/peg")+xlab("Time (minutes)")+
  ggtitle("L. monocytogenes MPN - Tolerance BAC 200ppm")+theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
mpn_bac
ggsave("Tolerance_MPN_BAC200ppm.png", plot=mpn_bac, device="png", width=10, height=8, units="in", dpi=600)
ggsave("Tolerance_MPN_BAC200ppm.svg", plot=mpn_bac, device="svg", width=10, height=8, units="in", dpi=600)



#Check ANOVA assumptions
#MPN
#Normality check using anova model residuals
bac_mpn<-lm(logMPN ~ factorAB, data = bac)
ggqqplot(residuals(bac_mpn))  
shapiro_test(residuals(bac_mpn)) #significant Shapiro test for normality

#Check homogeneity of variace assumption
plot(bac_mpn, 1)
bac %>% levene_test(logMPN ~ factorAB) #significant Levene test

#Data does not follow ANOVA assumptions.
#Run Kruskal-Wallis test on each independent data set


#Subset by treatment 
T1<-subset(bac, Code=="T1")
T6<-subset(bac, Code=="T6")
T7<-subset(bac, Code=="T7")
T8<-subset(bac, Code=="T8")
T9<-subset(bac, Code=="T9")
T10<-subset(bac, Code=="T10")
T11<-subset(bac, Code=="T11")
T12<-subset(bac, Code=="T12")
T13<-subset(bac, Code=="T13")
T14<-subset(bac, Code=="T14")
T15<-subset(bac, Code=="T15")
T16<-subset(bac, Code=="T16")
T17<-subset(bac, Code=="T17")
T18<-subset(bac, Code=="T18")
T19<-subset(bac, Code=="T19")
T20<-subset(bac, Code=="T20")

#Run Kruskal-Wallis test
#MPN
T1.kruskal <- T1 %>% kruskal_test(logMPN ~ Timepoint)
T1.kruskal #p = 0.0766

T6.kruskal <- T6 %>% kruskal_test(logMPN ~ Timepoint)
T6.kruskal #p = 0.026

T7.kruskal <- T7 %>% kruskal_test(logMPN ~ Timepoint)
T7.kruskal #p = 0.108

T8.kruskal <- T8 %>% kruskal_test(logMPN ~ Timepoint)
T8.kruskal #p = 0.0782

T9.kruskal <- T9 %>% kruskal_test(logMPN ~ Timepoint)
T9.kruskal #p = 0.0719

T10.kruskal <- T10 %>% kruskal_test(logMPN ~ Timepoint)
T10.kruskal #p = 0.112

T11.kruskal <- T11 %>% kruskal_test(logMPN ~ Timepoint)
T11.kruskal #p = 0.0833

T12.kruskal <- T12 %>% kruskal_test(logMPN ~ Timepoint)
T12.kruskal #p = 0.3

T13.kruskal <- T13 %>% kruskal_test(logMPN ~ Timepoint)
T13.kruskal #p = 0.0804

T14.kruskal <- T14 %>% kruskal_test(logMPN ~ Timepoint)
T14.kruskal #p = 0.0782

T15.kruskal <- T15 %>% kruskal_test(logMPN ~ Timepoint)
T15.kruskal #p = 0.103

T16.kruskal <- T16 %>% kruskal_test(logMPN ~ Timepoint)
T16.kruskal #p = 0.135

T17.kruskal <- T17 %>% kruskal_test(logMPN ~ Timepoint)
T17.kruskal #p = 0.108

T18.kruskal <- T18 %>% kruskal_test(logMPN ~ Timepoint)
T18.kruskal #p = 0.149

T19.kruskal <- T19 %>% kruskal_test(logMPN ~ Timepoint)
T19.kruskal #p = 0.114

T20.kruskal <- T20 %>% kruskal_test(logMPN ~ Timepoint)
T20.kruskal #p = 0.104



#Calculate effect size
#MPN-biof
T1 %>% kruskal_effsize(logMPN ~ Timepoint)
T6 %>% kruskal_effsize(logMPN ~ Timepoint)
T7 %>% kruskal_effsize(logMPN ~ Timepoint)
T8 %>% kruskal_effsize(logMPN ~ Timepoint)
T9 %>% kruskal_effsize(logMPN ~ Timepoint)
T10 %>% kruskal_effsize(logMPN ~ Timepoint)
T11 %>% kruskal_effsize(logMPN ~ Timepoint)
T12 %>% kruskal_effsize(logMPN ~ Timepoint)
T13 %>% kruskal_effsize(logMPN ~ Timepoint)
T14 %>% kruskal_effsize(logMPN ~ Timepoint)
T15 %>% kruskal_effsize(logMPN ~ Timepoint)
T16 %>% kruskal_effsize(logMPN ~ Timepoint)
T17 %>% kruskal_effsize(logMPN ~ Timepoint)
T18 %>% kruskal_effsize(logMPN ~ Timepoint)
T19 %>% kruskal_effsize(logMPN ~ Timepoint)
T20 %>% kruskal_effsize(logMPN ~ Timepoint)


# Pairwise comparisons
DT.T1 = dunnTest(logMPN ~ Timepoint, data=T1, method = "none")   
DT.T6 = dunnTest(logMPN ~ Timepoint, data=T6, method = "none")   
DT.T7 = dunnTest(logMPN ~ Timepoint, data=T7, method = "none")   
DT.T8 = dunnTest(logMPN ~ Timepoint, data=T8, method = "none")   
DT.T9 = dunnTest(logMPN ~ Timepoint, data=T9, method = "none")   
DT.T10 = dunnTest(logMPN ~ Timepoint, data=T10, method = "none")   
DT.T11 = dunnTest(logMPN ~ Timepoint, data=T11, method = "none")   
DT.T12 = dunnTest(logMPN ~ Timepoint, data=T12, method = "none")   
DT.T13 = dunnTest(logMPN ~ Timepoint, data=T13, method = "none")   
DT.T14 = dunnTest(logMPN ~ Timepoint, data=T14, method = "none")   
DT.T15 = dunnTest(logMPN ~ Timepoint, data=T15, method = "none")   
DT.T16 = dunnTest(logMPN ~ Timepoint, data=T16, method = "none")   
DT.T17 = dunnTest(logMPN ~ Timepoint, data=T17, method = "none")   
DT.T18 = dunnTest(logMPN ~ Timepoint, data=T18, method = "none")   
DT.T19 = dunnTest(logMPN ~ Timepoint, data=T19, method = "none")   
DT.T20 = dunnTest(logMPN ~ Timepoint, data=T20, method = "none")   


DunnLetters.T1<-cldList(P.adj ~ Comparison,data = DT.T1$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T6<-cldList(P.adj ~ Comparison,data = DT.T6$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T7<-cldList(P.adj ~ Comparison,data = DT.T7$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T8<-cldList(P.adj ~ Comparison,data = DT.T8$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T9<-cldList(P.adj ~ Comparison,data = DT.T9$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T10<-cldList(P.adj ~ Comparison,data = DT.T10$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T11<-cldList(P.adj ~ Comparison,data = DT.T11$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T12<-cldList(P.adj ~ Comparison,data = DT.T12$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T13<-cldList(P.adj ~ Comparison,data = DT.T13$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T14<-cldList(P.adj ~ Comparison,data = DT.T14$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T15<-cldList(P.adj ~ Comparison,data = DT.T15$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T16<-cldList(P.adj ~ Comparison,data = DT.T16$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T17<-cldList(P.adj ~ Comparison,data = DT.T17$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T18<-cldList(P.adj ~ Comparison,data = DT.T18$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T19<-cldList(P.adj ~ Comparison,data = DT.T19$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T20<-cldList(P.adj ~ Comparison,data = DT.T20$res,threshold = 0.05, remove.zero = FALSE)

#### PAA 500 ppm data ####

#Input data
paa500<-read_excel("Results_tolerance_PAA.xlsx", sheet=2, col_names = TRUE)
paa500$Timepoint<-as.factor(paa500$Timepoint)
paa500$factorAB <- with(paa500, interaction(Trt, Timepoint))

#Make die-off line plot
#Calculate summary statistics - MPN
stat_mpn_paa500<-describeBy(paa500$logMPN, list(paa500$Trt, paa500$Timepoint), mat = TRUE) #By Timepoint, treatement and type
stat_mpn_paa500$group2<-as.numeric(stat_mpn_paa500$group2)
stat_mpn_paa500 <- stat_mpn_paa500[order(stat_mpn_paa500$group1),]
stat_mpn_paa500$group2<-as.numeric(stat_mpn_paa500$group2)
stat_mpn_paa500$Code<-c(rep(1,4),rep(5,4),rep(4,4),rep(11,4),rep(2,4),rep(8,4),rep(7,4),rep(14,4),rep(6,4),rep(13,4),rep(12,4),rep(16,4),rep(3,4),rep(10,4),rep(9,4),rep(15,4))

mpn_paa500<-ggplot(stat_mpn_paa500, aes(x=group2, y=mean, group=group1, color=group1))+
  geom_point()+geom_line()+facet_wrap(vars(reorder(group1,Code)))+
  #geom_text(data = subset(stat_mpn_paa500, group2 == 2), aes(label = group1, colour = group1, x = 2.05, y = mean), hjust = -.1, size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=1)+
  geom_hline(yintercept = 0.85, color="grey80", linetype=2)+ #LOD for biofilm
  scale_x_continuous(limits = c(0,45), breaks = c(0,15,30,45))+
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10))+
  theme(plot.margin=margin(t=0.5, b=0.5, l=0.5, r=0.5, unit = 'in'))+
  theme(axis.text = element_text(color='black', size=13), axis.ticks = element_line(color='black')) +
  theme(axis.title = element_text(size=13,color='black')) +
  theme(panel.background = element_rect(fill='white', color = NA),
        plot.background = element_rect(fill = 'white',color = NA), 
        panel.border = element_rect(color='black',fill = NA,linewidth=1))+
  #theme(panel.grid.major = element_line(color = "grey50"), panel.grid.minor.y = element_line(color = "grey50"))+
  theme(strip.background= element_blank(), strip.text = element_text(size=15),
        panel.border = element_rect(color="black", fill=NA))+
  theme(legend.position = "bottom")+
  ylab("log10 MPN/peg")+xlab("Time (minutes)")+
  ggtitle("L. monocytogenes MPN - Tolerance PAA 500ppm")+theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
mpn_paa500
ggsave("Tolerance_MPN_paa500ppm.png", plot=mpn_paa500, device="png", width=10, height=8, units="in", dpi=600)
ggsave("Tolerance_MPN_paa500ppm.svg", plot=mpn_paa500, device="svg", width=10, height=8, units="in", dpi=600)



#Check ANOVA assumptions
#MPN
#Normality check using anova model residuals
paa500_mpn<-lm(logMPN ~ factorAB, data = paa500)
ggqqplot(residuals(paa500_mpn))  
shapiro_test(residuals(paa500_mpn)) #significant Shapiro test for normality

#Check homogeneity of variace assumption
plot(paa500_mpn, 1)
paa500 %>% levene_test(logMPN ~ factorAB) #significant Levene test

#Data does not follow ANOVA assumptions.
#Run Kruskal-Wallis test on each independent data set


#Subset by treatment 
T1<-subset(paa500, Code=="T01")
T6<-subset(paa500, Code=="T06")
T7<-subset(paa500, Code=="T07")
T8<-subset(paa500, Code=="T08")
T9<-subset(paa500, Code=="T09")
T10<-subset(paa500, Code=="T10")
T11<-subset(paa500, Code=="T11")
T12<-subset(paa500, Code=="T12")
T13<-subset(paa500, Code=="T13")
T14<-subset(paa500, Code=="T14")
T15<-subset(paa500, Code=="T15")
T16<-subset(paa500, Code=="T16")
T17<-subset(paa500, Code=="T17")
T18<-subset(paa500, Code=="T18")
T19<-subset(paa500, Code=="T19")
T20<-subset(paa500, Code=="T20")

#Run Kruskal-Wallis test
#MPN
T1.kruskal <- T1 %>% kruskal_test(logMPN ~ Timepoint)
T1.kruskal #p = 0.0766

T6.kruskal <- T6 %>% kruskal_test(logMPN ~ Timepoint)
T6.kruskal #p = 0.0782

T7.kruskal <- T7 %>% kruskal_test(logMPN ~ Timepoint)
T7.kruskal #p = 0.0766

T8.kruskal <- T8 %>% kruskal_test(logMPN ~ Timepoint)
T8.kruskal #p = 0.0766

T9.kruskal <- T9 %>% kruskal_test(logMPN ~ Timepoint)
T9.kruskal #p = 0.0719

T10.kruskal <- T10 %>% kruskal_test(logMPN ~ Timepoint)
T10.kruskal #p = 0.0766

T11.kruskal <- T11 %>% kruskal_test(logMPN ~ Timepoint)
T11.kruskal #p = 0.0782

T12.kruskal <- T12 %>% kruskal_test(logMPN ~ Timepoint)
T12.kruskal #p = 0.0782

T13.kruskal <- T13 %>% kruskal_test(logMPN ~ Timepoint)
T13.kruskal #p = 0.0766

T14.kruskal <- T14 %>% kruskal_test(logMPN ~ Timepoint)
T14.kruskal #p = 0.0766

T15.kruskal <- T15 %>% kruskal_test(logMPN ~ Timepoint)
T15.kruskal #p = 0.0766

T16.kruskal <- T16 %>% kruskal_test(logMPN ~ Timepoint)
T16.kruskal #p = 0.0782

T17.kruskal <- T17 %>% kruskal_test(logMPN ~ Timepoint)
T17.kruskal #p = 0.0907

T18.kruskal <- T18 %>% kruskal_test(logMPN ~ Timepoint)
T18.kruskal #p = 0.1

T19.kruskal <- T19 %>% kruskal_test(logMPN ~ Timepoint)
T19.kruskal #p = 0.112

T20.kruskal <- T20 %>% kruskal_test(logMPN ~ Timepoint)
T20.kruskal #p = 0.0782



#Calculate effect size
#MPN-biof
T1 %>% kruskal_effsize(logMPN ~ Timepoint)
T6 %>% kruskal_effsize(logMPN ~ Timepoint)
T7 %>% kruskal_effsize(logMPN ~ Timepoint)
T8 %>% kruskal_effsize(logMPN ~ Timepoint)
T9 %>% kruskal_effsize(logMPN ~ Timepoint)
T10 %>% kruskal_effsize(logMPN ~ Timepoint)
T11 %>% kruskal_effsize(logMPN ~ Timepoint)
T12 %>% kruskal_effsize(logMPN ~ Timepoint)
T13 %>% kruskal_effsize(logMPN ~ Timepoint)
T14 %>% kruskal_effsize(logMPN ~ Timepoint)
T15 %>% kruskal_effsize(logMPN ~ Timepoint)
T16 %>% kruskal_effsize(logMPN ~ Timepoint)
T17 %>% kruskal_effsize(logMPN ~ Timepoint)
T18 %>% kruskal_effsize(logMPN ~ Timepoint)
T19 %>% kruskal_effsize(logMPN ~ Timepoint)
T20 %>% kruskal_effsize(logMPN ~ Timepoint)


# Pairwise comparisons
DT.T1 = dunnTest(logMPN ~ Timepoint, data=T1, method = "none")   
DT.T6 = dunnTest(logMPN ~ Timepoint, data=T6, method = "none")   
DT.T7 = dunnTest(logMPN ~ Timepoint, data=T7, method = "none")   
DT.T8 = dunnTest(logMPN ~ Timepoint, data=T8, method = "none")   
DT.T9 = dunnTest(logMPN ~ Timepoint, data=T9, method = "none")   
DT.T10 = dunnTest(logMPN ~ Timepoint, data=T10, method = "none")   
DT.T11 = dunnTest(logMPN ~ Timepoint, data=T11, method = "none")   
DT.T12 = dunnTest(logMPN ~ Timepoint, data=T12, method = "none")   
DT.T13 = dunnTest(logMPN ~ Timepoint, data=T13, method = "none")   
DT.T14 = dunnTest(logMPN ~ Timepoint, data=T14, method = "none")   
DT.T15 = dunnTest(logMPN ~ Timepoint, data=T15, method = "none")   
DT.T16 = dunnTest(logMPN ~ Timepoint, data=T16, method = "none")   
DT.T17 = dunnTest(logMPN ~ Timepoint, data=T17, method = "none")   
DT.T18 = dunnTest(logMPN ~ Timepoint, data=T18, method = "none")   
DT.T19 = dunnTest(logMPN ~ Timepoint, data=T19, method = "none")   
DT.T20 = dunnTest(logMPN ~ Timepoint, data=T20, method = "none")   


DunnLetters.T1<-cldList(P.adj ~ Comparison,data = DT.T1$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T6<-cldList(P.adj ~ Comparison,data = DT.T6$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T7<-cldList(P.adj ~ Comparison,data = DT.T7$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T8<-cldList(P.adj ~ Comparison,data = DT.T8$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T9<-cldList(P.adj ~ Comparison,data = DT.T9$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T10<-cldList(P.adj ~ Comparison,data = DT.T10$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T11<-cldList(P.adj ~ Comparison,data = DT.T11$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T12<-cldList(P.adj ~ Comparison,data = DT.T12$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T13<-cldList(P.adj ~ Comparison,data = DT.T13$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T14<-cldList(P.adj ~ Comparison,data = DT.T14$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T15<-cldList(P.adj ~ Comparison,data = DT.T15$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T16<-cldList(P.adj ~ Comparison,data = DT.T16$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T17<-cldList(P.adj ~ Comparison,data = DT.T17$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T18<-cldList(P.adj ~ Comparison,data = DT.T18$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T19<-cldList(P.adj ~ Comparison,data = DT.T19$res,threshold = 0.05, remove.zero = FALSE)
DunnLetters.T20<-cldList(P.adj ~ Comparison,data = DT.T20$res,threshold = 0.05, remove.zero = FALSE)



#### Sterilex data -APC & MPN ####

#Input data
sterilex<-read_excel("Results_tolerance_PAA.xlsx", sheet=4, col_names = TRUE)
sterilex$Timepoint<-as.factor(sterilex$Timepoint)
sterilex$factorAB <- with(sterilex, interaction(Trt, Timepoint))

#Make reduction bar plots
#Calculate summary statistics - APC
stat_apc_sterilex<-describeBy(sterilex$logAPC, list(sterilex$Trt, sterilex$Timepoint), mat = TRUE) #By Timepoint, treatement and type
stat_apc_sterilex <- stat_apc_sterilex[order(stat_apc_sterilex$group1),]
stat_apc_sterilex$group2<-as.factor(stat_apc_sterilex$group2)

apc_sterilex<-ggplot(stat_apc_sterilex, aes(x=group2, y=mean, fill=group2))+
  geom_bar(stat="identity", color = "black")+facet_grid(.~group1)+
  #geom_text(data = subset(stat_apc_bac, group2 == 2), aes(label = group1, colour = group1, x = 2.05, y = mean), hjust = -.1, size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
  geom_hline(yintercept = 0.85, color="grey80", linetype=2)+ #LOD for biofilm
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10))+
  theme(plot.margin=margin(t=0.5, b=0.5, l=0.5, r=0.5, unit = 'in'))+
  theme(axis.text = element_text(color='black', size=13), axis.text.x=element_text(angle=90), axis.ticks = element_line(color='black')) +
  theme(axis.title = element_text(size=13,color='black')) +
  theme(panel.background = element_rect(fill='white', color = NA),
        plot.background = element_rect(fill = 'white',color = NA), 
        panel.border = element_rect(color='black',fill = NA,size=1))+
  #theme(panel.grid.major = element_line(color = "grey50"), panel.grid.minor.y = element_line(color = "grey50"))+
  theme(strip.background= element_blank(), strip.text = element_text(size=10, angle=90),
        panel.border = element_rect(color="black", fill=NA))+
  theme(legend.position = "none")+
  ylab("log10 APC/peg")+xlab("Time (minutes)")+
  ggtitle("APC - Tolerance Sterilex")+theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
apc_sterilex
ggsave("Tolerance_APC_Sterilex.png", plot=apc_sterilex, device="png", width=10, height=8, units="in", dpi=600)
ggsave("Tolerance_APC_Sterilex.svg", plot=apc_sterilex, device="svg", width=10, height=8, units="in", dpi=600)

#Calculate summary statistics - MPN
stat_mpn_sterilex<-describeBy(sterilex$logMPN, list(sterilex$Trt, sterilex$Timepoint), mat = TRUE) #By Timepoint, treatement and type
stat_mpn_sterilex <- stat_mpn_sterilex[order(stat_mpn_sterilex$group1),]
stat_mpn_sterilex$group2<-as.factor(stat_mpn_sterilex$group2)

mpn_sterilex<-ggplot(stat_mpn_sterilex, aes(x=group2, y=mean, fill=group2))+
  geom_bar(stat="identity", color = "black")+facet_grid(.~group1)+
  #geom_text(data = subset(stat_mpn_bac, group2 == 2), aes(label = group1, colour = group1, x = 2.05, y = mean), hjust = -.1, size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
  geom_hline(yintercept = 0.85, color="grey80", linetype=2)+ #LOD for biofilm
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10))+
  theme(plot.margin=margin(t=0.5, b=0.5, l=0.5, r=0.5, unit = 'in'))+
  theme(axis.text = element_text(color='black', size=13), axis.text.x=element_text(angle=90), axis.ticks = element_line(color='black')) +
  theme(axis.title = element_text(size=13,color='black')) +
  theme(panel.background = element_rect(fill='white', color = NA),
        plot.background = element_rect(fill = 'white',color = NA), 
        panel.border = element_rect(color='black',fill = NA,size=1))+
  #theme(panel.grid.major = element_line(color = "grey50"), panel.grid.minor.y = element_line(color = "grey50"))+
  theme(strip.background= element_blank(), strip.text = element_text(size=10, angle=90),
        panel.border = element_rect(color="black", fill=NA))+
  theme(legend.position = "none")+
  ylab("log10 MPN/peg")+xlab("Time (minutes)")+
  ggtitle("L. monocytogenes MPN - Tolerance Sterilex")+theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
mpn_sterilex
ggsave("Tolerance_MPN_Sterilex.png", plot=mpn_sterilex, device="png", width=10, height=8, units="in", dpi=600)
ggsave("Tolerance_MPN_Sterilex.svg", plot=mpn_sterilex, device="svg", width=10, height=8, units="in", dpi=600)


#Run t-test 
#Subset by treatment
T1_sterilex<-subset(sterilex, Code =="T1")
T6_sterilex<-subset(sterilex, Code =="T6")
T7_sterilex<-subset(sterilex, Code =="T7")
T8_sterilex<-subset(sterilex, Code =="T8")
T9_sterilex<-subset(sterilex, Code =="T9")
T10_sterilex<-subset(sterilex, Code =="T10")
T11_sterilex<-subset(sterilex, Code =="T11")
T12_sterilex<-subset(sterilex, Code =="T12")
T13_sterilex<-subset(sterilex, Code =="T13")
T14_sterilex<-subset(sterilex, Code =="T14")
T15_sterilex<-subset(sterilex, Code =="T15")
T16_sterilex<-subset(sterilex, Code =="T16")
T17_sterilex<-subset(sterilex, Code =="T17")
T18_sterilex<-subset(sterilex, Code =="T18")
T19_sterilex<-subset(sterilex, Code =="T19")
T20_sterilex<-subset(sterilex, Code =="T20")

#APC
#check homogeneity of variance
bartlett.test(logAPC~Timepoint, data =T1_sterilex)
bartlett.test(logAPC~Timepoint, data =T6_sterilex)
bartlett.test(logAPC~Timepoint, data =T7_sterilex)
bartlett.test(logAPC~Timepoint, data =T8_sterilex)
bartlett.test(logAPC~Timepoint, data =T9_sterilex)
bartlett.test(logAPC~Timepoint, data =T10_sterilex)
bartlett.test(logAPC~Timepoint, data =T11_sterilex)
bartlett.test(logAPC~Timepoint, data =T12_sterilex)
bartlett.test(logAPC~Timepoint, data =T13_sterilex)
bartlett.test(logAPC~Timepoint, data =T14_sterilex)
bartlett.test(logAPC~Timepoint, data =T15_sterilex)
bartlett.test(logAPC~Timepoint, data =T16_sterilex)
bartlett.test(logAPC~Timepoint, data =T17_sterilex)
bartlett.test(logAPC~Timepoint, data =T18_sterilex)
bartlett.test(logAPC~Timepoint, data =T19_sterilex)
bartlett.test(logAPC~Timepoint, data =T20_sterilex)

#paired t-test
t.test(formula= logAPC ~Timepoint, data=T1_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.002824
t.test(formula= logAPC ~Timepoint, data=T6_sterilex, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95)#p=0.0001047
t.test(formula= logAPC ~Timepoint, data=T7_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.00147
t.test(formula= logAPC ~Timepoint, data=T8_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=4.898e-5
t.test(formula= logAPC ~Timepoint, data=T9_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.0009373
t.test(formula= logAPC ~Timepoint, data=T10_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.001141
t.test(formula= logAPC ~Timepoint, data=T11_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.0002679
t.test(formula= logAPC ~Timepoint, data=T12_sterilex, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95)#p=1.148e-5
t.test(formula= logAPC ~Timepoint, data=T13_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=8.668e-5
t.test(formula= logAPC ~Timepoint, data=T14_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.0003859
t.test(formula= logAPC ~Timepoint, data=T15_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.0004886
t.test(formula= logAPC ~Timepoint, data=T16_sterilex, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95)#p=0.002703
t.test(formula= logAPC ~Timepoint, data=T17_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.001177
t.test(formula= logAPC ~Timepoint, data=T18_sterilex, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95)#p=9.718e-5
t.test(formula= logAPC ~Timepoint, data=T19_sterilex, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95)#p=3.759e-5
t.test(formula= logAPC ~Timepoint, data=T20_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.0006348


#MPN
#check homogeneity of variance
bartlett.test(logMPN~Timepoint, data =T1_sterilex)
bartlett.test(logMPN~Timepoint, data =T6_sterilex)
bartlett.test(logMPN~Timepoint, data =T7_sterilex)
bartlett.test(logMPN~Timepoint, data =T8_sterilex)
bartlett.test(logMPN~Timepoint, data =T9_sterilex)
bartlett.test(logMPN~Timepoint, data =T10_sterilex)
bartlett.test(logMPN~Timepoint, data =T11_sterilex)
bartlett.test(logMPN~Timepoint, data =T12_sterilex)
bartlett.test(logMPN~Timepoint, data =T13_sterilex)
bartlett.test(logMPN~Timepoint, data =T14_sterilex)
bartlett.test(logMPN~Timepoint, data =T15_sterilex)
bartlett.test(logMPN~Timepoint, data =T16_sterilex)
bartlett.test(logMPN~Timepoint, data =T17_sterilex)
bartlett.test(logMPN~Timepoint, data =T18_sterilex)
bartlett.test(logMPN~Timepoint, data =T19_sterilex)
bartlett.test(logMPN~Timepoint, data =T20_sterilex)

#paired t-test
t.test(formula= logMPN ~Timepoint, data=T1_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.005057
t.test(formula= logMPN ~Timepoint, data=T6_sterilex, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95)#p=3.61e-5
t.test(formula= logMPN ~Timepoint, data=T7_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.0003645
t.test(formula= logMPN ~Timepoint, data=T8_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.003959
t.test(formula= logMPN ~Timepoint, data=T9_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.000605
t.test(formula= logMPN ~Timepoint, data=T10_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.005628
t.test(formula= logMPN ~Timepoint, data=T11_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=8.525e-5
t.test(formula= logMPN ~Timepoint, data=T12_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.001646
t.test(formula= logMPN ~Timepoint, data=T13_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.0003031
t.test(formula= logMPN ~Timepoint, data=T14_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.005247
t.test(formula= logMPN ~Timepoint, data=T15_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.004963
t.test(formula= logMPN ~Timepoint, data=T16_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.01895
t.test(formula= logMPN ~Timepoint, data=T17_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.0001269
t.test(formula= logMPN ~Timepoint, data=T18_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.001753
t.test(formula= logMPN ~Timepoint, data=T19_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.003495
t.test(formula= logMPN ~Timepoint, data=T20_sterilex, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95)#p=0.0004746


#### Sterilex data - Crystal violet ####
setwd("G:/My Drive/Penn State/Research/File for R/Thesis/10_PAA paper/")

#Input data
sterilex_cv<-read_excel("Results_tolerance_PAA.xlsx", sheet=5, col_names = TRUE)
sterilex_cv$Timepoint<-as.factor(sterilex_cv$Timepoint)

#Make reduction bar plots
#Calculate summary statistics - APC
stat_cv_sterilex<-describeBy(sterilex_cv$Absorbance, list(sterilex_cv$Trt, sterilex_cv$Timepoint), mat = TRUE) #By Timepoint, treatement and type
stat_cv_sterilex <- stat_cv_sterilex[order(stat_cv_sterilex$group1),]
stat_cv_sterilex$group2<-as.factor(stat_cv_sterilex$group2)
stat_cv_sterilex$Order<-c(rep(1,2),rep(5,2),rep(4,2),rep(11,2),rep(2,2),rep(8,2),rep(7,2),rep(14,2),rep(6,2),rep(13,2),rep(12,2),rep(16,2),rep(3,2),rep(10,2),rep(9,2),rep(15,2))

cv_sterilex<-ggplot(stat_cv_sterilex, aes(x=group2, y=mean, fill=group2))+
  geom_bar(stat="identity", color = "black")+facet_grid(.~reorder(group1,Order))+
  #geom_text(data = subset(stat_apc_bac, group2 == 2), aes(label = group1, colour = group1, x = 2.05, y = mean), hjust = -.1, size=3)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2)+
  scale_y_continuous(limits = c(0,16), breaks = c(0,2,4,6,8,10,12,14,16))+
  theme(plot.margin=margin(t=0.5, b=0.5, l=0.5, r=0.5, unit = 'in'))+
  theme(axis.text = element_text(color='black', size=13), axis.text.x=element_text(angle=90), axis.ticks = element_line(color='black')) +
  theme(axis.title = element_text(size=13,color='black')) +
  theme(panel.background = element_rect(fill='white', color = NA),
        plot.background = element_rect(fill = 'white',color = NA), 
        panel.border = element_rect(color='black',fill = NA,size=1))+
  #theme(panel.grid.major = element_line(color = "grey50"), panel.grid.minor.y = element_line(color = "grey50"))+
  theme(strip.background= element_blank(), strip.text = element_text(size=10, angle=90),
        panel.border = element_rect(color="black", fill=NA))+
  theme(legend.position = "none")+
  ylab("Absorbance")+xlab("Time (minutes)")+
  ggtitle("CV - Tolerance Sterilex-average")+theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
cv_sterilex
ggsave("Tolerance_CV_Sterilex_average.png", plot=cv_sterilex, device="png", width=10, height=8, units="in", dpi=600)
ggsave("Tolerance_CV_Sterilex_average.svg", plot=cv_sterilex, device="svg", width=10, height=8, units="in", dpi=600)



#Run t-test 
#Subset by treatment
T1_sterilex_cv<-subset(sterilex_cv, Code =="T1")
T6_sterilex_cv<-subset(sterilex_cv, Code =="T6")
T7_sterilex_cv<-subset(sterilex_cv, Code =="T7")
T8_sterilex_cv<-subset(sterilex_cv, Code =="T8")
T9_sterilex_cv<-subset(sterilex_cv, Code =="T9")
T10_sterilex_cv<-subset(sterilex_cv, Code =="T10")
T11_sterilex_cv<-subset(sterilex_cv, Code =="T11")
T12_sterilex_cv<-subset(sterilex_cv, Code =="T12")
T13_sterilex_cv<-subset(sterilex_cv, Code =="T13")
T14_sterilex_cv<-subset(sterilex_cv, Code =="T14")
T15_sterilex_cv<-subset(sterilex_cv, Code =="T15")
T16_sterilex_cv<-subset(sterilex_cv, Code =="T16")
T17_sterilex_cv<-subset(sterilex_cv, Code =="T17")
T18_sterilex_cv<-subset(sterilex_cv, Code =="T18")
T19_sterilex_cv<-subset(sterilex_cv, Code =="T19")
T20_sterilex_cv<-subset(sterilex_cv, Code =="T20")

#APC
#check homogeneity of variance
bartlett.test(Absorbance~Timepoint, data =T1_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T6_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T7_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T8_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T9_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T10_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T11_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T12_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T13_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T14_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T15_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T16_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T17_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T18_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T19_sterilex_cv)
bartlett.test(Absorbance~Timepoint, data =T20_sterilex_cv)

#t-test
t.test(formula= Absorbance~Timepoint, data=T1_sterilex_cv, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95) #p=0.167
t.test(formula= Absorbance~Timepoint, data=T6_sterilex_cv, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95) #p=4.903e-6
t.test(formula= Absorbance~Timepoint, data=T7_sterilex_cv, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95) #p=0.000186
t.test(formula= Absorbance~Timepoint, data=T8_sterilex_cv, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95) #p=0.06085
t.test(formula= Absorbance~Timepoint, data=T9_sterilex_cv, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95) #p=0.7065
t.test(formula= Absorbance~Timepoint, data=T10_sterilex_cv, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95) #p=4.471e-6
t.test(formula= Absorbance~Timepoint, data=T11_sterilex_cv, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95) #p=4.959e-8
t.test(formula= Absorbance~Timepoint, data=T12_sterilex_cv, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95) #p=0.0001715
t.test(formula= Absorbance~Timepoint, data=T13_sterilex_cv, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95) #p=0.137
t.test(formula= Absorbance~Timepoint, data=T14_sterilex_cv, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95) #p=0.6088
t.test(formula= Absorbance~Timepoint, data=T15_sterilex_cv, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95) #p=0.1728
t.test(formula= Absorbance~Timepoint, data=T16_sterilex_cv, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95) #p=1.313e-7
t.test(formula= Absorbance~Timepoint, data=T17_sterilex_cv, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95) #p=0.0004803
t.test(formula= Absorbance~Timepoint, data=T18_sterilex_cv, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95) #p=0.0001614
t.test(formula= Absorbance~Timepoint, data=T19_sterilex_cv, alternative="two.sided", mu=0, var.equal=FALSE, conf.level=0.95) #p=0.009216
t.test(formula= Absorbance~Timepoint, data=T20_sterilex_cv, alternative="two.sided", mu=0, var.equal=TRUE, conf.level=0.95) #p=2.305e-5


#### Poster Figure - all sanitizers biofilms - ####
#Input data
tolerance<-read_excel("Results_tolerance_PAA.xlsx", sheet=5, col_names = TRUE)
tolerance$Timepoint<-as.factor(tolerance$Timepoint)
tolerance$factorABC <- with(tolerance, interaction(Trt,Timepoint,Antimicrobial))

#Make die-off line plot
#Calculate summary statistics - MPN
stat_mpn_tolerance<-describeBy(tolerance$logMPN, list(tolerance$Trt, tolerance$Timepoint, tolerance$Antimicrobial), mat = TRUE) #By Timepoint, treatement and type
stat_mpn_tolerance$group2<-as.numeric(stat_mpn_tolerance$group2)
stat_mpn_tolerance <- stat_mpn_tolerance[order(stat_mpn_tolerance$group1),]
stat_mpn_tolerance$factorABC <- with(stat_mpn_tolerance, interaction(group1,group3))
stat_mpn_tolerance2<-subset(stat_mpn_tolerance, vars != is.na(TRUE))
stat_mpn_tolerance2<-subset(stat_mpn_tolerance2, group2!=90 )
stat_mpn_tolerance2<-subset(stat_mpn_tolerance2, group3!="12.5 ppm BAC" )

mpn_tolerance<-ggplot(stat_mpn_tolerance2, aes(x=group2, y=mean, group=factorABC, color=group1, shape=group3, linetype=group3))+
  geom_point()+geom_line()+facet_grid(group3~group1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se))+
  geom_hline(yintercept = 0.85, color="grey80", linetype=2)+ #LOD for biofilm
  scale_x_continuous(limits = c(0,150), breaks = c(0,30,60,90,120,150))+
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme(plot.margin=margin(t=0.5, b=0.5, l=0.5, r=0.5, unit = 'in'))+
  theme(axis.text = element_text(color='black', size=13), axis.ticks = element_line(color='black'), axis.text.x=element_text(angle=90)) +
  theme(axis.title = element_text(size=13,color='black')) +
  theme(panel.background = element_rect(fill='white', color = NA),
        plot.background = element_rect(fill = 'white',color = NA), 
        panel.border = element_rect(color='black',fill = NA,linewidth = 1))+
  #theme(panel.grid.major = element_line(color = "grey50"), panel.grid.minor.y = element_line(color = "grey50"))+
  theme(strip.background= element_blank(), strip.text = element_text(size=15),
        panel.border = element_rect(color="black", fill=NA))+
  theme(legend.position = "bottom")+
  ylab("Biofilm log10 MPN/peg")+xlab("Time (minutes)")+
  ggtitle("L. monocytogenes MPN - Tolerance All Sanitizers")+theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
mpn_tolerance
ggsave("Tolerance_MPN_Poster.png", plot=mpn_tolerance, device="png", width=21, height=11, units="in", dpi=600)
ggsave("Tolerance_MPN_Poster.svg", plot=mpn_tolerance, device="svg", width=21, height=11, units="in", dpi=600)

