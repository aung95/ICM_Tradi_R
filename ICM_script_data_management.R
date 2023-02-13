# DATA MANAGEMENT 
# ------------ Library used -------------------------
library(readxl) # pour importer le document
library(ggplot2) # Pour faire de beaux graphiques
library(dplyr) # Pour ???
library(Hmisc) # To use the describe () of the variable ? reverif
library(gmodels) # use to describe qualitative variables ? reverif
library(psych) # to use describeby
# ------------ Import variables ---------------------
setwd("/Users/alexandreunger/R")
view_df <- read_excel("donnees/ICM_data.xlsx") # Tibble --> better visualization 
df1 <- as.data.frame(read_excel("donnees/ICM_data.xlsx")) # df 1 => tibble > data.frame
# ------------ New dataframe to export results -------
df2 <- data.frame(df1$`N° Patients`) # create the first line
colnames(df2) <- c("NumPatient") # put a name
label(df2$NumPatient)<- c("Numéro des patients")
# ------------ DATA MANAGEMENT  ---------------------- NB : Check à q fois si data are missing !!!
# ---------- OUTCOMES ----
# Table 1 / LGE vs No LGE ... (variable to divide patients)
df2$LGE_Presence <- as.factor(df1$`Presence of any pattern of LGE ischemic or mid-wall (0= No; 1=Yes)`)# on voit que c'est un integer et il faut transformer en categorical variables avec as.factor
label(df2$LGE_Presence) <- c("Presence of any pattern of LGE ischemic or mid-wall (0= No; 1=Yes)`") # afin de l'avoir dans les graphes
# Table 1 bis / without LGE / Number of segments
df2$LGE_Nb_Segment <-df1$`Number of segments of ischemic LGE` + df1$`Number of segments of mid-wall LGE` # here we did it -> ask theo if okay
df2$LGE_Nb_Segment<- ifelse(df2$LGE_Nb_Segment == 0, "NoLGE", 
                            ifelse(df2$LGE_Nb_Segment == 1 | df2$LGE_Nb_Segment == 2,"1_2LGE", 
                                   ifelse(df2$LGE_Nb_Segment == 3 |df2$LGE_Nb_Segment == 4 | df2$LGE_Nb_Segment == 5, "3_5LGE",
                                          ifelse(df2$LGE_Nb_Segment >= 6, "6_moreLGE", NA))))
df2$LGE_Nb_Segment <- factor(df2$LGE_Nb_Segment, levels = c("NoLGE","1_2LGE","3_5LGE","6_moreLGE"))
label(df2$LGE_Presence) <- c("Number of segments of ischemic LGE AND midwall LGE")
# Table 1 ter / Baseline and CMR Characteristics of the Study stratified by death or not (N=X)
df2$death <- factor(df1$`Death (0=No; 1=Yes)`)
levels(df2$death) <- c("Alive", "Death")
label(df2$death) <- c("df1$`Death (0=No; 1=Yes)`)")
# ------- PREDICTORS -----
# 1. Age 
df2$Age <- df1$`Age at baseline (years)` # on sauve la nouvelle variable 
label(df2$Age) <- c("Age at baseline")
# 2. Males
df2$Genre <- as.factor(df1$`Sex (0= Females; 1=Males)`)
label(df2$Genre) <- c("Sex (0= Females; 1=Males)")
# 3. BMI
df2$BMI <- df1$`BMI (kg/m2)`
label(df2$BMI) <- c("Body Mass Index (BMI) in kg/m2")
# 4.1 Diabetes
df2$Diabetes <- as.factor(df1$`Diabetes (0=No; 1=Yes)`)
label(df2$Diabetes) <- c("Diabetes (0=No; 1=Yes)")
# 4.2 Hypertension
df2$HTN <- as.factor(df1$`Hypertension (0=No; 1=Yes)`)
label(df2$HTN) <- c("Hypertension (0=No; 1=Yes)")
# 4.3 Obesity
df2$Obesity <- as.factor(df1$`Obesity (0=No; 1=Yes)`)
label(df2$Obesity) <- c("`Obesity (0=No; 1=Yes)`")
# 4.4 Hyperchol
df2$Dyslipi <- as.factor(df1$`Dyslipidemia (0=No; 1=Yes)`)
label(df2$Dyslipi) <- c("Dyslipidemia (0=No; 1=Yes)")
# 4.5 Current or previous smoking : 3 levels au factor donc je l'ai converti en 2
smoke <- c()
smoke[df1$`Smoking (0= no; 1=current smoker; 2=previous smoker)` == 1 | 
        df1$`Smoking (0= no; 1=current smoker; 2=previous smoker)` == 2] <- 1 
smoke[is.na(smoke)] <- 0
df2$SmokingHx <- as.factor(smoke)
label(df2$SmokingHx) <- c("Smoking (0= no; 1=current smoker or previous smoker)")
# 4.6 Family history of CAD
df2$FamHxCAD <- as.factor(df1$`Family History of CAD (0=No; 1=Yes)`)
label(df2$FamHxCAD) <- c("Family History of CAD (0=No; 1=Yes)")
# 5.1 Known MI
df2$KnownMI <- as.factor(df1$`Known MI (0=No; 1=Yes)`)
label(df2$KnownMI) <- c("Known MI (0=No; 1=Yes)")
# 5.2 History of PCI
df2$HxPCI <- as.factor(df1$`History of PCI (0=No; 1=Yes)`)
label(df2$HxPCI) <- c("History of PCI (0=No; 1=Yes)")
# 5.3 History of CABG
df2$HxCABG <- as.factor(df1$`History of CABG (0=No; 1=Yes)`)
label(df2$HxCABG) <- c("History of CABG (0=No; 1=Yes)")
# 6.1 Peripheral atheroma
df2$PeriphAtheroma <- as.factor(df1$`Peripheral atheroma disease (0=No; 1=Yes)`)
label(df2$PeriphAtheroma) <- c("Peripheral atheroma disease (0=No; 1=Yes)")
# 6.2 Ischemic stroke
df2$IschStroke <- factor(df1$`Stroke (0=No; 1=Yes)`)
label(df2$IschStroke) <- c("Stroke (0=No; 1=Yes)")
# 6.3 Pacemaker
df2$PCMK <- factor(df1$`Pacemaker (0=No; 1=Yes)`)
label(df2$PCMK) <- c("Pacemaker (0=No; 1=Yes)")
# 6.4 History of renal failure
df2$HxKidneyInjury <- factor(df1$`Renal failure, GFR < 60 (0=No; 1=Yes)`)
label(df2$HxKidneyInjury) <- c("Renal failure, GFR < 60 (0=No; 1=Yes)")
# 6.5 History of hospitalization for HF 
df2$HxHospitHF <- factor(df1$`History of hospitalization for Heart Failure  (0=No; 1=Yes)`)
label(df2$HxHospitHF) <- c("History of hospitalization for Heart Failure  (0=No; 1=Yes)")
# 6.6 History of atrial fibrillation
df2$HxHospitAF <- factor(df1$`History of Atrial Fibrillation (0=No; 1= Yes)`)
label(df2$HxHospitAF) <- c("History of Atrial Fibrillation (0=No; 1= Yes)")
# 7.1 NYHA 
df2$NYHA_3_4 <- factor(df1$`Dyspnea, NYHA II+ (0=No; 1=Yes)`)
label(df2$NYHA_3_4) <- c("Dyspnea, NYHA II+ (0=No; 1=Yes)")
# 8.1 Sinus Rythm or Atrial fibrilation : NB : could be improved by creating two differnts categories
df2$SinusRythm <- df1$`Cardiac rhythm during the CMR exam (0= Sinus rhythm; 1=Atrial fibrillation/supraventricular; 2=Sinus rhythm with extrasystoles)`
SinusRythm <- c()
SinusRythm[df1$`Cardiac rhythm during the CMR exam (0= Sinus rhythm; 1=Atrial fibrillation/supraventricular; 2=Sinus rhythm with extrasystoles)` == 1] <- 1 
SinusRythm[is.na(SinusRythm)] <- 0 # O et 2 dans la même catégories
df2$SinusRythm <- as.factor(SinusRythm)
label(df2$SinusRythm) <- c("Cardiac rhythm during the CMR exam (0= Sinus rhythm (ESV included); 1=Atrial fibrillation/supraventricular)")
# 9.1 LV ejection fraction 
df2$LVEF <- df1$`LVEF (%)`
label(df2$LVEF) <- c("LVEF (%)")
# 9.2 LV end-diastolic volume index, ml/m2 
df2$LVEDiastV <- df1$`LV end-diastolic volume index EDVi, ml/m2`
label(df2$LVEDiastV) <- c("LV end-diastolic volume index EDVi, ml/m2")
# 9.3 LV end-systolic volume index, ml/m2 LV mass indexed, g/m2
df2$LVESystV <- df1$`LV end-systolic volume index ESVi, ml/m2`
label(df2$LVESystV) <- c("LV end-systolic volume index ESVi, ml/m2")
# 9.4 LV Mass index
df2$LVMass <- df1$`LV mass indexed (g/m2)`
label(df2$LVMass) <- c("LV mass indexed, g/m2")
# 9.5 RV dysfunction, n (%)
df2$RVDysf <- factor(df1$`RV dysfunction (0= No; 1=Yes)`)
label(df2$RVDysf) <- c("RV dysfunction (0= No; 1=Yes)")

# 10 Topography of LGE (SOUCIS : FAIRE VARIABLE POUR q type ! mais garder une variable originnelle qu'on pourra enlever dans le tableau
df2$LGE_Topo_Gen <- as.factor(df1$`Topography of ischemic LGE (0= No LGE; 1=subendocardial <50%; 2=subendocardial 50-99%; 3=transmural)`)
round(100*(prop.table(table(df2$LGE_Topo_Gen, df2$LGE_Presence))),1)
label(df2$LGE_Topo_Gen) <- c("Topography of ISCHEMIC LGE (0= No LGE; 1=subendocardial <50%; 2=subendocardial 50-99%; 3=transmural)")
# 10.1 No LGE (on prend l'autre variable mais on s'assure que ca match avec celle d'ouctomes)
Topo <- c()
Topo[df1$`Topography of ischemic LGE (0= No LGE; 1=subendocardial <50%; 2=subendocardial 50-99%; 3=transmural)` == 0] <- 1 
Topo[is.na(Topo)] <- 0 
df2$LGE_Topo_No_LGE<- factor(Topo)
label(df2$LGE_Topo_No_LGE) <- c("Topography of ischemic LGE (0= Other ; 1 =No Ischemic LGE)")
# 10.2 Transmural
Topo <- c()
Topo[df1$`Topography of ischemic LGE (0= No LGE; 1=subendocardial <50%; 2=subendocardial 50-99%; 3=transmural)` == 3] <- 1 
Topo[is.na(Topo)] <- 0 
df2$LGE_Topo_Transmural<- factor(Topo)
label(df2$LGE_Topo_Transmural) <- c("Topography of ischemic LGE (0= Other ; 1 =transmural)")
# 10.3 Subendocardial
Topo <- c()
Topo[df1$`Topography of ischemic LGE (0= No LGE; 1=subendocardial <50%; 2=subendocardial 50-99%; 3=transmural)` == 1 |
       df1$`Topography of ischemic LGE (0= No LGE; 1=subendocardial <50%; 2=subendocardial 50-99%; 3=transmural)` == 2] <- 1 
Topo[is.na(Topo)] <- 0 
df2$LGE_Topo_Subendocardial<- factor(Topo)
label(df2$LGE_Topo_Subendocardial) <- c("Topography of ischemic LGE (0= Other ; 1 =subendocardial (fusion or 2 and 3))")
# 10.4  Mid-wall, n (%) (autre variable de df1)
df2$LGE_Topo_MidWall <- factor(df1$`Presence of mid-wall LGE (0= No; 1=Yes)`)
label(df2$LGE_Topo_MidWall) <- c("`Presence of mid-wall LGE (0= No; 1=Yes)")
#10.5 PResence of mid-wall and ischemic LGE together
MidIsch <- c()
MidIsch[df1$`Presence of ischemic LGE (0= No; 1=Yes)` == 1 & 
       df1$`Presence of mid-wall LGE (0= No; 1=Yes)` == 1] <- 1 # & --> on veut les 2
MidIsch[is.na(MidIsch)] <- 0 
df2$LGE_isch_AND_midW<- factor(MidIsch)
# 11 Number of areas with LGE : Focal or multiples ? 
df2$LGE_Focal_or_Multiple_Gen <- as.factor(df1$`Focal or Multiple ischemic LGE (0= no LGE; 1= Focal; 2=Multiple)`)
label(df2$LGE_Focal_or_Multiple_Gen) <- c("Focal or Multiple ischemic LGE (0= no LGE; 1= Focal; 2=Multiple)")
# 11.1 Area Focal
Area <- c()
Area[df1$`Focal or Multiple ischemic LGE (0= no LGE; 1= Focal; 2=Multiple)` == 1] <- 1 
Area[is.na(Area)] <- 0 # O et 2 dans la même catégories
Area
df2$LGE_focal<- factor(Area)
label(df2$LGE_focal) <- c("Focal ischemic LGE (0= no LGE or multiple ! ; 1= Focal")
# 11.2 Area Multiples
Area <- c()
Area[df1$`Focal or Multiple ischemic LGE (0= no LGE; 1= Focal; 2=Multiple)` == 2] <- 1 
Area[is.na(Area)] <- 0 
df2$LGE_multiple<- factor(Area)
label(df2$LGE_multiple) <- c("Focal ischemic LGE (0= no LGE or focal ! ; 1= Multiple")
# ---------------------------------------------
# 10.5 Location of LGE
# Anterior (n%)
Anterior <- c()
Anterior[df1$`Presence of mid-wall LGE in Anterior (0= No; 1=Yes)` == 1 | 
              df1$`Presence of ischemic LGE in Anterior (0= No; 1=Yes)` == 1] <- 1 
Anterior[is.na(Anterior)] <- 0
df2$LGE_Anterior <- as.factor(Anterior)
label(df2$LGE_Anterior) <- c("Presence of LGE in Anterior (MIDWALL AND ISCHEMIC) (0= No; 1=Yes)")
# 12.2 Septal, n (%)
Septal <- c()
Septal[df1$`Presence of mid-wall LGE in Septal (0= No; 1=Yes)` == 1 | 
           df1$`Presence of ischemic LGE in Septal (0= No; 1=Yes)` == 1] <- 1 
Septal[is.na(Septal)] <- 0
df2$LGE_Septal <- as.factor(Septal)
label(df2$LGE_Septal) <- c("Presence of LGE in Septal (MIDWALL AND ISCHEMIC) (0= No; 1=Yes)")
# 12.3 Inferior, n (%)
Inferior <- c()
Inferior[df1$`Presence of mid-wall LGE in Inferior (0= No; 1=Yes)` == 1 | 
           df1$`Presence of ischemic LGE in Inferior (0= No; 1=Yes)` == 1] <- 1 
Inferior[is.na(Inferior)] <- 0
df2$LGE_Inferior <- as.factor(Inferior)
label(df2$LGE_Inferior) <- c("Presence of LGE in INFERIOR (MIDWALL AND ISCHEMIC) (0= No; 1=Yes)")
# 13.4 Lateral, n (%)
Lateral <- c()
Lateral[df1$`Presence of mid-wall LGE in Lateral (0= No; 1=Yes)` == 1 | 
           df1$`Presence of ischemic LGE in Lateral (0= No; 1=Yes)` == 1] <- 1 
Lateral[is.na(Lateral)] <- 0
df2$LGE_Lateral <- as.factor(Lateral)
label(df2$LGE_Lateral) <- c("Presence of LGE in LATERAL (MIDWALL AND ISCHEMIC) (0= No; 1=Yes)")
# 12.5 Apical, n (%)
Apical <- c()
Apical[df1$`Presence of mid-wall LGE in Apical (0= No; 1=Yes)` | 
           df1$`Presence of ischemic LGE in Apical (0= No; 1=Yes)`] <- 1 
Apical[is.na(Apical)] <- 0
df2$LGE_Apical <- as.factor(Apical)
label(df2$LGE_Apical) <- c("Presence of LGE in APICAL (MIDWALL AND ISCHEMIC) (0= No; 1=Yes)")
# 13.1 Number of segments of LGE, mean ± SD (ischemic or mid-wall ?) # to improve
df2$LGE_Mean_of_segments <- df1$`Number of segments of ischemic LGE`
label(df2$LGE_Mean_of_segments) <- c("Number of segments of ischemic LGE (Mean and SD)")
# ------------------ export new report for df2 ----
dir.create("./results/ICM_results")
dir.create("./results/ICM_results/Descriptive_ICM")
plot(describe(df2)) # export ces files 
# write continuous variable
write.table(round(describeBy(df2[,c("Age","BMI","LVEF","LVEDiastV","LVESystV","LVMass","LGE_Mean_of_segments")]),2), file = "/Users/alexandreunger/R/results/ICM_results/Descriptive_ICM/describe_continuous_variable_df2.csv", sep=",")
  # ------------------- Export ------------------
save(df2, file = "donnees/Data_ICM_managed.RData") # 

