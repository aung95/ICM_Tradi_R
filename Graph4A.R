# Fig 4A selon taux de mortalité --> J'Y ARRIVE PAS 
setwd("/Users/alexandreunger/R")
library(readxl) # pour importer le document
library(compareGroups)
library(ggplot2)
library(psych) # to use descrbeBy

# -------------- Import Data
load("donnees/Data_ICM_managed.RData")

###----- # DESCRIBE BY SUBGROUPS
General_descr = createTable(
  compareGroups(LGE_Nb_Segment ~ death , data=df2, method = NA), hide = c("Alive"), show.all=T, show.p.trend = T )
General_descr
### graph

df <- df2[c("death","LGE_Nb_Segment")]
df

Table_death <- table(df2$death, df2$LGE_Nb_Segment) 
Table_death
Table_death[2,]
Prop_table <- round(100*prop.table(Table_death, margin =2 ),2)
barplot(Prop_table[2,], xlab = "Number of segments of LGE", ylab = "Proportion of death", main = "Pourcentage of death among different extension of lesion in CMR")
death <- Prop_table[2,]

ggplot(df, aes(x = "LGE_Nb_Segment")) +  
  geom_bar(aes(y = (sum(df$death == "1"))/sum(df$death))

# https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/

# margin = 2 comme ça on analyse par colonne 

