## Completer votre base en ajoutant des varibles(15) et des observations(300)
install.packages("readxl")
library(readxl)

#Importer la base depuis Excel
chemin_fichier <- "C:\\Users\\CELINA\\Desktop\\Base.xlsx"
Bases <- read_excel(chemin_fichier)

# Afficher les premières lignes de la base de données
head(Bases)



## Faire des stats desc sur les variables
install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

#Stat desc
summary(Bases)

# Créons des variables de classe pour l'âge et la moyenne
limites_Âge <- seq(15, 25, by = 5)
limites_Moyenne <- seq(9, 21, by = 3)
labels_Âge <- paste("[", head(limites_Âge, -1), ";", tail(limites_Âge, -1), "]", sep = "")
labels_Moyenne <- paste("[", head(limites_Moyenne, -1), ";", tail(limites_Moyenne, -1), "]", sep = "")
Bases$ClasseAge <- cut(Bases$Âge, breaks = limites_Âge, include.lowest = TRUE, labels = labels_Âge)
Bases$ClasseMoyenne <- cut(Bases$Moyenne, breaks = limites_Moyenne, include.lowest = TRUE, labels = labels_Moyenne)

# Fréquence de chaque classe d'âge
freq_Âge <- table(Bases$ClasseAge)
print(freq_Âge)
# Histogramme des classes d'âge
ggplot(Bases, aes(x=ClasseAge)) +
  geom_histogram(stat="count", fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title="Distribution des classes d'âge", x="Classe d'âge", y="Fréquence")

# Fréquence de chaque classe de Moyenne
freq_Moyenne <- table(Bases$ClasseMoyenne)
print(freq_Moyenne)
# Histogramme des classes de Moyene
ggplot(Bases, aes(x=ClasseMoyenne)) +
  geom_histogram(stat="count", fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title="Distribution des classes de taille", x="Classe de taille", y="Fréquence")

# Diagramme à secteurs
part <- table(Bases$Sexe)
pie(part, labels = names(part), main = "Répartition par sexe")



##Implementer manuellement le test de khi-deux

# Tableau de contingence des variables Sexe et Apte
tableau <- table(Bases$Sexe, Bases$Apte)
print(tableau)

# Tableau des fréquences attendues
lignes <- rowSums(tableau)
colonnes <- colSums(tableau)
total <- sum(tableau)
attendu <- outer(lignes, colonnes) / total
print(attendu)

# Calculons la statistique du test du khi-2
khi2 <- sum((tableau - attendu)^2 / attendu)
print(khi2)