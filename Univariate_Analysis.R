# Title : Cox univariate analysis 
# Author : Unger A.
# Date of creation : 13/02/23
# Last modification : 15/02/23

setwd("/Users/alexandreunger/R")

# librayr used 
library(compareGroups)
library(survival)
library(survivalAnalysis)
library(ggplot2)
library(survminer) # range of nice plotting functions for survival analysis

# -------------- Import Data
load("donnees/Data_ICM_managed.RData")
###----- # DESCRIBE BY SUBGROUPS
df2$death # bien laisser en NUM

# univariate Cox plot : utiliser select - demander à Solenn comment changer l'IC sur compareGroups - bien hide les valeurs qui nous intéressent pas

df2$LVEF_di <- cut(df2$LVEF, breaks = c(10, 15, 20,25,30,35,40,45,50))
df2$LVEF_di <- factor(df2$LVEF_di, levels=rev(levels(df2$LVEF_di)))

df2$Diast_di <- cut_number(df2$LVEDiastV, n=4)

df2$VarSurv <- with(df2, Surv(FUtime, death == '1'))
attr(df2$VarSurv,"label") <- "Time to death or censoring"


VarAnalysis <- c("Age", "Genre",  "BMI", "Obesity", "HTN", "Diabetes", "Dyslipi", "SmokingHx","FamHxCAD","KnownMI","HxPCI","HxCABG","HxKidneyInjury","HxHospitHF","HxHospitAF",
                 "Sinusrythm","LGE_Presence","LGE_Focal_or_Multiple_Gen","LGE_Topo_Gen","LGE_Nb_Segment","LVEF_di","Diast_di")

HR_univ_995  = createTable(
  compareGroups(VarSurv ~ . ,data = df2, method = NA, conf.level = 0.995), 
  hide = "0",
  show.ratio = T, show.p.overall = T) # changed the conf level ; hide ref ; DONT UNDERSTAND WHY LEVDiastV n'apparait pas ... 
HR_univ_995["Diast_di"]
export2csv(HR_univ_995[VarAnalysis], file="/Users/alexandreunger/R/results/ICM_results/Descriptive_ICM/univ_HR_995.csv") # exports to csv format

