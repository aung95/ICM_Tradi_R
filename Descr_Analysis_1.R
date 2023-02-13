# TABLE 1 FIRST --> Descriptive analysis of data
setwd("/Users/alexandreunger/R")
library(readxl) # pour importer le document
library(compareGroups)
library(ggplot2)
library(psych) # to use descrbeBy

# -------------- Import Data
load("donnees/Data_ICM_managed.RData")

###----- describe(df2)
sum(is.na(df2)) # check si dans les news data y en a des missing !
missingTable(compareGroups(LGE_Presence ~ ., df2)) # give a table of missing values

# DESCRIBE BY SUBGROUPS
General_descr = createTable(
  compareGroups(LGE_Presence ~ Age + Genre + BMI , data=df2, method = NA), hide = c(Genre="0"), show.all=T, show.p.overall = T)
CoronaryRiskFactor_descr = createTable(
  compareGroups(LGE_Presence ~ Diabetes + HTN + Obesity + Dyslipi + SmokingHx + FamHxCAD, data=df2, method = NA), 
  hide = c(Diabetes="0",HTN = "0", Obesity ="0", Dyslipi ="0", SmokingHx="0", FamHxCAD="0"), 
  show.all=T, show.p.overall = T)
ICM_descr = createTable(
  compareGroups(LGE_Presence ~ KnownMI + HxPCI + HxCABG, data=df2, method = NA), 
  hide = c(KnownMI="0",HxPCI = "0", HxCABG ="0"), 
  show.all=T, show.p.overall = T)
MedHxCVDisease_descr = createTable(
  compareGroups(LGE_Presence ~ PeriphAtheroma + IschStroke + PCMK + HxKidneyInjury + HxHospitHF + HxHospitAF, data=df2, method = NA), 
  hide = c(PeriphAtheroma="0",IschStroke = "0", PCMK ="0", HxKidneyInjury ="0", HxHospitHF ="0", HxHospitAF ="0"), 
  show.all=T, show.p.overall = T)
NYHA_descr = createTable(
  compareGroups(LGE_Presence ~ NYHA_3_4, data=df2, method = NA), 
  hide = c(NYHA_3_4="0"), 
  show.all=T, show.p.overall = T)
CardiacRythm_descr = createTable(
  compareGroups(LGE_Presence ~ SinusRythm, data=df2, method = NA), 
  show.all=T, show.p.overall = T) # pas de hide car on veut les 2
MRIParam_descr = createTable(
  compareGroups(LGE_Presence ~ LVEF + LVEDiastV + LVESystV + LVMass + RVDysf, data=df2, method = NA), 
  hide = c(LVEF="0",LVEDiastV = "0", LVESystV ="0", LVMass ="0", RVDysf ="0"), 
  show.all=T, show.p.overall = T)
TopoLGE_descr = createTable(
  compareGroups(LGE_Presence ~ LGE_Topo_Gen + LGE_Topo_No_LGE  + LGE_Topo_Transmural + LGE_Topo_Subendocardial + LGE_Topo_MidWall + LGE_isch_AND_midW, data=df2, method = NA), # df2$LGE_Topo_Gen (afin d'avoir le graphe global)
  hide = c(LGE_Topo_No_LGE = "0", LGE_Topo_Transmural = "0", LGE_Topo_Subendocardial="0", LGE_Topo_MidWall ="0",LGE_isch_AND_midW ="0"),
  show.all=T, show.p.overall = T)
NumberAreaLGE_descr = createTable(
  compareGroups(LGE_Presence ~ LGE_Focal_or_Multiple_Gen + LGE_focal + LGE_multiple, data=df2, method = NA), 
  hide = c(LGE_focal ="0", LGE_multiple = "0"),
  show.all=T, show.p.overall = T)
LocationLGE_descr = createTable(
  compareGroups(LGE_Presence ~ LGE_Anterior + LGE_Septal + LGE_Inferior + LGE_Lateral + LGE_Apical, data=df2, method = NA), 
  hide = c(LGE_Anterior="0",LGE_Septal = "0", LGE_Inferior ="0", LGE_Lateral ="0", LGE_Apical ="0"),
  show.all=T, show.p.overall = T)
NumberSegmLGE_descr  = createTable(
  compareGroups(LGE_Presence ~ LGE_Mean_of_segments, data=df2, method = 1), 
  show.all=T, show.p.overall = T) # forced to consider as normal

table_results <- rbind(
  "General Description"=General_descr, 
  "Coronary risk factors" = CoronaryRiskFactor_descr,
  "ICM" = ICM_descr,
  "Medical history of CV disease, n (%)" = MedHxCVDisease_descr,
  "Symptoms" = NYHA_descr,
  "Cardiac rythm" = CardiacRythm_descr,
  "MRI parameters" = MRIParam_descr,
  "Topography of LGE" = TopoLGE_descr,
  "Number of areas with LGE" = NumberAreaLGE_descr,
  "Location of LGE (ischemic only or midwall too ?" = LocationLGE_descr,
  "Number of segments of LGE, mean ± SD" = NumberSegmLGE_descr
  )

# DESCRIPTIVE ANALYSIS 
print(table_results, which.table =  'avail')
print(table_results, which.table = 'descr')

dir.create("./results/ICM_results")
dir.create("./results/ICM_results/Descriptive_ICM")
dir.create("./results/ICM_results/Descriptive_ICM/table1_graphs_descr_ICM")
file_graph <- "./results/ICM_results/Descriptive_ICM/table1_graphs_descr_ICM"
dir.create("./results/ICM_results/Descriptive_ICM/table1_graphs_descr_ICM/uni_number")
dir.create("./results/ICM_results/Descriptive_ICM/table1_graphs_descr_ICM/uni_F")
dir.create("./results/ICM_results/Descriptive_ICM/table1_graphs_descr_ICM/multi_F")

plot(table_results, perc = F, file="./results/ICM_results/Descriptive_ICM/table1_graphs_descr_ICM/uni_number/uni_N", type="png") # in number
plot(table_results, perc = T, file="./results/ICM_results/Descriptive_ICM/table1_graphs_descr_ICM/uni_F/uni_F",type="png") # Convertir en ggplot ce qu'il faudra. # , bivar = T # soucis avec les calculs de p-value de la normalité
plot(table_results, perc = T, bivar = T, file="./results/ICM_results/Descriptive_ICM/table1_graphs_descr_ICM/multi_F/multi_F",type="png") # bivar afin d'avoir la comparaison avec le LGE

# TERMINAL / CREATED DATA and EXPORTATION 
export2csv(table_results, file="/Users/alexandreunger/R/results/ICM_results/Descriptive_ICM/table_1_demographics_analysis.csv") # exports to csv format
