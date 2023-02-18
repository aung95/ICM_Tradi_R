# Title : Cox multivariate analysis 
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
library(mfp)

# -------------- Import Data
load("donnees/Data_ICM_managed.RData")
###----- # DESCRIBE BY SUBGROUPS
df2$death # bien laisser en NUM

# ------ # Assessing crude relations between predictors and the outcome - IS IT NEEDED FOR COX ??? 
# 1/ Between categorical variable and the binary outcomes
by_gender <- table(factor(df2$Genre), factor(df2$death)) # # cross tabulation 
by_gender_prop <- prop.table(by_gender, margin = 1) # proportion of death by gender 
odds_gender <- by_gender_prop[, "1"]/by_gender_prop[, "0"] # calculate the odds of having diabetes by gender 
logodds_gender <- log(odds_gender) # calculate the log odds 
dotchart(logodds_gender)# plot the log odds of having diabetes by gender 

# 2/ Between continuous variable and the binary outcomes (not grouped)
dm_by_age <- table(factor(df2$Age), factor(df2$death)) # create a cross tabulation of age and diabetes status  - not including NA values because there aren't that many 
dm_by_age_prop <- prop.table(dm_by_age, margin = 1) # output the frequencies of diabetes status by age 
odds_age <- dm_by_age_prop[, "1"]/dm_by_age_prop[, "0"] # calculate the odds of having diabetes 
logodds_age <- log(odds_age) # calculate the log odds 
plot(rownames(dm_by_age_prop), logodds_age) # plot the ages found in the sample against the log odds of having diabetes 

# univariate Cox plot : utiliser select - demander à Solenn comment changer l'IC sur compareGroups - bien hide les valeurs qui nous intéressent pas

df2$LVEF_di <- cut(df2$LVEF, breaks = c(10, 15, 20,25,30,35,40,45,50))
df2$LVEF_di <- factor(df2$LVEF_di, levels=rev(levels(df2$LVEF_di)))
df2$Diast_di <- cut_number(df2$LVEDiastV, n=4)
df2$VarSurv <- with(df2, Surv(FUtime, death == '1'))
attr(df2$VarSurv,"label") <- "Time to death or censoring"

VarAnalysis <- c("Age", "Genre",  "BMI", "Obesity", "HTN", "Diabetes", "Dyslipi", "SmokingHx","FamHxCAD","KnownMI","HxPCI","HxCABG","HxKidneyInjury","HxHospitHF","HxHospitAF",
                 "Sinusrythm","LGE_Presence","LGE_Focal_or_Multiple_Gen","LGE_Topo_Gen","LGE_Nb_Segment","LVEF_di","Diast_di")

# selection de variable / marche po ... 
mfp(Surv(FUtime, death ~ 
           fp(Age, df = 1, select = 0.05)+
           fp(Genre, df = 1, select = 0.05) +
           fp(BMI, df = 1, select = 0.05) + 
           fp(Obesity, df = 1, select = 0.05) + 
           fp(Diabetes, df = 1, select = 0.05) +
           fp(HTN, df = 1, select = 0.05), family = cox, data = df2))
           
           
#            
#            fp(Smoking, df = 1, select = 0.05) + 
#            fp(Dyslipidemia, df = 1, select = 0.05) + 
#            fp(Stroke, df = 1, select = 0.05) +
#            fp(Obesity, df = 1, select = 0.05) +
#            fp(Cardiac_Rhythm, df = 1, select = 0.05) +
#            fp(Renal_Failure, df = 1, select = 0.05) +
#            fp(Peripheral_Atheroma, df = 1, select = 0.05) +
#            fp(Symptomatic_Angina, df = 1, select = 0.05) +
#            fp(Dyspnea, df = 1, select = 0.05) +
#            fp(Pacemaker, df = 1, select = 0.05) +
#            fp(History_Hospit_HF, df = 1, select = 0.05) +
#            fp(Segments_Stenosis_Sup_70percent, df = 1, select = 0.05) +
#            fp(Segments_Stenosis_Sup_50percent, df = 1, select = 0.05) +
#            fp(Segments_Noncalcified_Plaques, df = 1, select = 0.05) +
#            fp(Vessels_Obstructive_CAD, df = 1, select = 0.05) +
#            fp(Proximal_Segments_Stenosis_Sup_70percent, df = 1, select = 0.05) +
#            fp(Proximal_Segments_Stenosis_Sup_50percent, df = 1, select = 0.05) +
#            fp(Segments_Calcified_Plaques, df = 1, select = 0.05) +
#            fp(Segments_Mixed_Plaques, df = 1, select = 0.05) +
#            fp(Family_History_CAD, df = 1, select = 0.05), family = cox, data = data_CT)
# print(f)

# COX MULTIVARIATE 
cox <- coxph(VarSurv ~ Age + Genre + BMI + Obesity + HTN + Diabetes, 
             data = df2)
summary(cox)

# check if the model is fine or not etc 

# export2csv(HR_univ_995[VarAnalysis], file="/Users/alexandreunger/R/results/ICM_results/Descriptive_ICM/univ_HR_995.csv") # exports to csv format

