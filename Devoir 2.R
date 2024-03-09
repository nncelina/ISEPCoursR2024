##Créer une base de données fictive ayant au moins 5 variables avec les types numériques, caractères, facteurs …
Donne <- data.frame(
  Nom = c("Ange","Aissatou","Awa","Niass","Ameth","Celina","Jeanne","Malick"),
  Age = c(19,16,20,21,22,25,29,35),
  Sexe = c("M","F","F","M","M","F","F","M"),
  Matim = c("Celibataire","Marié(e)","Celibataire","Celibataire","Marié(e)","Marié(e)","Celibataire","Marié(e)"),
  Nb_enf = c(6,0,3,0,9,0,1,1))
View(Donne)



##créer une matrice à partir de été ase de données, renommer les lignes et les colonnes.
Matrice <- as.matrix(Donne)
colnames(Matrice)<- c("Nom","Age","Sexe","Situation matrimoniale","Nombre d'enfants")
rownames(Matrice)<- c("Individu 1","Individu 2","Individu 3","Individu 4","Individu 5","Individu 6","Individu 7","Individu 8")
View(Matrice)



##Faites des statistiques descrptives
summary(Donne)



##Construisez des Graphiques

# Diagramme à secteurs
part <- table(Donne$Sexe)
pie(part, labels = names(part), main = "Répartition par sexe")

# Diagramme en barres
breaks <- seq(15, max(Donne$Age), by = 5)
labels <- paste("[", breaks[-length(breaks)], "-", breaks[-1], "]", sep="")
Donne$classe_age <- cut(Donne$Age, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
ggplot(Donne, aes(x=classe_age)) +
  geom_bar() +
  xlab("Classe d'âge") +
  ylab("Nombre") +
  ggtitle("Diagramme en barres de la variable 'classe_age'")



##Résoudre à la main un problème facile d’optimisation.

# Vecteur de coût
c <- c(-2, -3, -4)

# Matrice de contraintes
A <- matrix(c(3, 2, 1,
              2, 5, 3,
              4, 2, 2), nrow = 3, byrow = TRUE)

# Côtés droits des contraintes
b <- c(10, 15, 18)

# Solution initiale
B <- matrix(c(1, 0, 0,
              0, 1, 0,
              0, 0, 1), nrow = 3, byrow = TRUE)

# Indices des variables de base initiales
s <- c(1, 2, 3)

# Fonction Simplexe
simplex <- function(c, A, b, B, s) {
  i = 0
  j = 1
  sum = 0
  max = -1
  min = 1000000
  entryVariable = -1
  exitVariable = -1
  entryVariable.relative = -1
  exitVariable.relative = -1
  cb <- c()
  entryCriterion <- c()
  
  # Etape 1: Initialisation
  invB = solve(B)# Inversion de la matrice
  xb <- invB %*% b# Tableau de solution initiale
  
  for(i in s){# Tableau des indices de la solution
    cb <- c(cb, c[i])
  }
  cb[is.na(cb)] <- 0
  
  nos <- c()# Indices des variables candidats
  for(i in 1:3){
    if(!i %in% s){
      nos <- c(nos, i)
    }
  }
  
  # Itération par l'algorithme
  while(TRUE){
    # Etape 2: Critère d'entrée
    for(i in nos){     # On obtient le critère pour décider quelle variable va entrer dans la solution
      ac <- A[, i]
      y <- invB %*% ac
      
      candidateVariableCost = c[i]
      if(is.na(candidateVariableCost))  candidateVariableCost = 0
      entryCriterion <- c(entryCriterion, cb %*% y - candidateVariableCost)
    }
    
    for(i in entryCriterion){   # Maximum (la variable qui va entrer est obtenue)
      if(i <= 0){
        sum = sum + 1
      } else if(i > max){
        max = i
        entryVariable.relative = j
      }
      j = j + 1
    }
    
    if(sum == length(entryCriterion)){ # Une solution optimale a été trouvée
      print("[ Optimal solution ]")
      break
    }
    
    entryVariable = nos[entryVariable.relative] # L'index de la variable d'entrée est obtenu
    
    # Etape 3: Critère de sortie
    y <- c()
    sum = 0
    j = 1
    y <- invB %*% A[, entryVariable]
    
    for(i in y){
      if(i <= 0){
        sum = sum + 1
      } else if(xb[j] / i < min){
        min = xb[j] / i
        exitVariable.relative = j
      }
      j = j + 1
    }
    
    exitVariable = s[exitVariable.relative]
    
    if(sum == length(A[, entryVariable])){
      return("[ Unbounded problem ]")
    }
    
    # Etape 4: La solution est recalculée
    B[, exitVariable.relative] = A[, entryVariable]
    
    invB = solve(B)               # Inverse de la matrice B
    xb <- invB %*% b              # La solution est obtenue
    s[exitVariable.relative] = entryVariable 
    nos[which(nos == entryVariable)] = exitVariable
    cb[exitVariable.relative] = c[entryVariable]
    if(is.na(cb[exitVariable.relative]))  cb[exitVariable.relative] = 0
    
    # Les variables temporaires sont nettoyées
    i = 0
    j = 1
    sum = 0
    max = -1
    min = 1000000
    entryVariable = -1
    exitVariable = -1
    entryVariable.relative = -1
    exitVariable.relative = -1
    entryCriterion <- c()
  }
  
  # Retour des valeurs
  z = cb[i] %*% xb[i]
  return(list("Valeur des variables" = xb, "Coût minimal" = z, "Base" = s))
}

# Exécution de la fonction Simplexe avec les nouvelles données
simplex(c, A, b, B, s)