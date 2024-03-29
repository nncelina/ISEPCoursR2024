
#Calculons la quantité achetée en Kg
##-Preparation de la base
library(haven)
cereales <- read_dta(paste0("C:\\Users\\Celina\\Desktop\\Célina❤\\ISEP\\ISEP2 2023-2024\\Semestre 4\\Traitement Statistique avec R\\ISEPCoursR2024\\cereales.dta"))

library("tidyverse")
glimpse(cereales)

#--Renommer variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")

##Gestion des NA

##Suppression des ménages ne consommant pas de céréales
attach(cereales)
anyNA(Qtty_cons)

##Création d'une variable temporaire
cereales$t <- ifelse(is.na(Qtty_cons)==1,1,0)
table(cereales$t)
cereales_na <- cereales[cereales$t==1,] ##Sous la base cereales
View(cereales_na)

##Suppression des ménages n'ayant pas déclaré les qtités cons

cereales <- cereales[cereales$t==0,]
dim(cereales)
View(cereales)
cereales$t <- NULL

#Faire un autre merge avec l'unité acheté et la taille achetée et le produit' pour avoir le poids acheté
###Créons dans la base cereales et la base table_de_conversion une variable real_id_achat qui est la concaténation des valeurs de produitID, UniteID et TailleID
library(haven)
table_de_conversion <- read_csv2(paste0("C:\\Users\\Celina\\Desktop\\Célina❤\\ISEP\\ISEP2 2023-2024\\Semestre 4\\Traitement Statistique avec R\\ISEPCoursR2024\\Table_de_conversion_phase_2.csv"))   #read_csv2 car le séparateur de champ de mon fichier csv est le point-virgule
View(table_de_conversion)
table_de_conversion[8:25]<- NULL
View(cereales)
cereales$real_id_achat <- paste(cereales$cereales__id, cereales$Unite_achat, cereales$Taille_achat)
View(cereales)
table_de_conversion$real_id_achat <- paste(table_de_conversion$produitID, table_de_conversion$uniteID, table_de_conversion$tailleID)
View(table_de_conversion)


###Nous pouvons maintenant fusionner la base cereales et la base table_de_conversion par la variable real_id
basefusionnee2 <- merge(cereales, table_de_conversion, by= "real_id_achat", all.x = TRUE, na.rm= TRUE)
View(basefusionnee2)
library(dplyr)



#Calculons la quantité achetée en kg
basefusionnee2$poids <- as.numeric(basefusionnee2$poids)
basefusionnee2$Qtty_achat <- as.numeric(basefusionnee2$Qtty_achat)
basefusionnee2$Quantite_achetee_en_kg <- (basefusionnee2$Qtty_achat*basefusionnee2$poids)/1000


#Calculons le prix unitaire
basefusionnee2$prix_unitaire= basefusionnee$Value_achat/basefusionnee$Quantite_achetee_en_kg

#Detection des valeurs aberrantes
##Traçage d'un boxplot
boxplot(basefusionnee2$Qtty_cons, 
        main = "Boxplot des quantités consommées", # Titre du graphique
        ylab = "Quantité consommée", # Nom de l'axe des ordonnées
        col = "pink", 
        border = "black", 
        horizontal=TRUE)
#Calcul de l'intervalle interquartile et remplacement des valeurs aberrantes par la médiane
summary(basefusionnee2$Qtty_cons)
Q1= quantile(basefusionnee2$Qtty_cons, 0.25)
Q3= quantile(basefusionnee2$Qtty_cons, 0.75)
IQR<- Q3-Q1
ifelse(basefusionnee2$Qtty_cons < 3-1.5*IQR & basefusionnee2$Qtty_cons > 14+1.5*IQR, basefusionnee2 <- median(basefusionnee2$Qtty_cons), basefusionnee2$Qtty_cons <-basefusionnee2$Qtty_cons)



