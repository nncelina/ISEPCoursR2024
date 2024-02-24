######Courbe de la fonction
# Pour rendre le graphe interactif
install.packages("plotly")
library(plotly)
library(magrittr)
install.packages("tidyr")
install.packages("ggplot2")# Load the magrittr package

# Create the interactive 3D plot
# Expression de la fonction
f <- function(x, y) {
  x*x + cos(x+y) + (y*y + (1/(1 + x*x)))^0.5
}
# Délimitation des intervalles
x <- seq(-2, 2, 0.1)  # Séquence de -10 à 10 avec un pas de 0.1
y <- seq(-2, 2, 0.1)  # Séquence de -10 à 10 avec un pas de 0.1
z <- outer(x, y, f)

# Création du graphique
plot_ly(x = x, y = y, z = z, type = "surface") %>%
  layout(title = "Courbe de la fonction")

######Courbe de la dérivée première par rapport à x
D(expression(x*x + cos(x+y) + (y*y + (1/(1 + x*x)))^0.5), "x")
# Pour rendre le graphe interactif
install.packages("plotly")
library(plotly)
library(magrittr)  # Load the magrittr package

# Create the interactive 3D plot
# Expression de la fonction
f <- function(x, y) {
  x + x - sin(x + y) - 0.5 * ((x + x)/(1 + x * x)^2 * (y * y + (1/(1 + x * x)))^-0.5)
}
# Délimitation des intervalles
x <- seq(-2, 2, 0.1)  # Séquence de -10 à 10 avec un pas de 0.1
y <- seq(-2, 2, 0.1)  # Séquence de -10 à 10 avec un pas de 0.1
z <- outer(x, y, f)
# Création du graphique
plot_ly(x = x, y = y, z = z, type = "surface") %>%
  layout(title = "Courbe de la dérivée première par rapport à x")

######Courbe de la dérivée première par rapport à y
D(expression(x*x + cos(x+y) + (y*y + (1/(1 + x*x)))^0.5), "y")
# Pour rendre le graphe interactif
install.packages("plotly")
library(plotly)
library(magrittr)  # Load the magrittr package

# Create the interactive 3D plot
# Expression de la fonction
f <- function(x, y) {
  0.5 * ((y + y) * (y * y + (1/(1 + x * x)))^-0.5) - sin(x + y)
}
# Délimitation des intervalles
x <- seq(-2, 2, 0.1)  # Séquence de -10 à 10 avec un pas de 0.1
y <- seq(-2, 2, 0.1)  # Séquence de -10 à 10 avec un pas de 0.1
z <- outer(x, y, f)

# Création du graphique
plot_ly(x = x, y = y, z = z, type = "surface") %>%
  layout(title = "Courbe de la dérivée première par rapport à y")