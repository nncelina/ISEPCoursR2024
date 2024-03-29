#Création d'une variable ID_recodé qui a comme valeurs les étiquettes de la vairiable cereales__id
c_val <- names(attr(cereales[["cereales__id"]], "labels"))
names(c_val) <- attr(cereales[["cereales__id"]],"labels")
c_val
attach(cereales)

library(dplyr)
# Utilisation de mutate pour créer une nouvelle variable B
cereales <- mutate(cereales, 
                   B = unname(c_val[as.character(cereales[["cereales__id"]])]))

attr(cereales$B,"labels") <- c_val # codage...
attr(cereales$B, "labels") <- paste0(attr(cereales$cereales__id, "label"),"_recoded") # label
ID_recodé<- cereales$B # Ajout à la liste


ID_recodé <- as.data.frame(ID_recodé)
View(ID_recodé)
cereales <- cbind.data.frame(cereales,ID_recodé)
cereales$B <- NULL
View(cereales)


#   Création de Unité_achat_recodée qui est une variable qui donne l'unité de chaque quantité de la variable  
c_val <- names(attr(cereales[["Unite_achat"]], "labels"))
names(c_val) <- attr(cereales[["Unite_achat"]],"labels")
c_val
attach(cereales)
library(dplyr)
#Utilisation de mutate pour créer une nouvelle variable A
cereales <- mutate(cereales, 
                   A = unname(c_val[as.character(cereales[["Unite_achat"]])]))

attr(cereales$A,"labels") <- c_val # codage...
attr(cereales$A, "labels") <- paste0(attr(cereales$Unite_achat, "label"),"_recoded") # label
Unite_achat_recodée<- cereales$A # Ajout à la liste


Unite_achat_recodée <- as.data.frame(Unite_achat_recodée)
View(Unite_achat_recodée)
cereales <- cbind.data.frame(cereales,Unite_achat_recodée)
cereales$A <- NULL
View(cereales)

#   Création de DernierAchat_recodé qui porte le label de la variable DernierAchat
c_val <- names(attr(cereales[["DernierAchat"]], "labels"))
names(c_val) <- attr(cereales[["DernierAchat"]],"labels")
c_val
attach(cereales)
library(dplyr)
#Utilisation de mutate pour créer une nouvelle variable A
cereales <- mutate(cereales, 
                   C = unname(c_val[as.character(cereales[["DernierAchat"]])]))

attr(cereales$C,"labels") <- c_val # codage...
attr(cereales$C, "labels") <- paste0(attr(cereales$DernierAchat, "label"),"_recoded") # label
DernierAchat_recodé<- cereales$C # Ajout à la liste


DernierAchat_recodé <- as.data.frame(DernierAchat_recodé)
View(DernierAchat_recodé)
cereales <- cbind.data.frame(cereales,DernierAchat_recodé)
cereales$C <- NULL
View(cereales)



#Changeons la variable numérique labellisé DernierAchat en variable textuelle
attach(cereales)
to_character(DernierAchat)
Taille_cons
Taille_achat
