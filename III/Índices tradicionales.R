## ?NDICES TRADICIONALES
library(entropart)
library(iNEXT)
library(vegan)
library(dplyr)
library(BiodiversityR)

# Base de datos por default
data(BCI)

# Pueden cargar su base de datos
BCI <- read.csv(choose.files())
View(BCI)
dim(BCI)

## MARGALEF Y MENHINICK
# Vector para almacenar el n?mero de especies por unidad de muestreo
S <- c()

# Ciclo para recorrer la matriz de datos
for(i in 1:nrow(BCI)){ # Ciclo desde 1 hasta el total de filas de los datos (unidades de muestreo)
  S.tmp <- length(which(BCI[i, ] > 0)) # N?mero de especies (columnas) con abundancia > 0 por unidad de muestreo
  S <- append(S, S.tmp) # A?adimos el n?mero de especies del sitio i al vector de especies
}

# Suma de las abundancias por unidad de muestreo (filas)
N <- rowSums(BCI)

# ?ndice de Margalef
Margalef <- (S - 1) / log(N)

# ?ndice de Menhinick
Menhinick <- S / sqrt(N)

# Combinamos ambos ?ndices en una misma tabla
indices <- cbind(Ma = Margalef, Me = Menhinick)

head(indices, 10)
indices <- as.data.frame(indices)

max(indices$Ma)
min(indices$Ma)

## Creamos gr?ficos para interpretaci?n de valores
# Histograma ?ndice de Margalef
hist(indices[, 1], # Valores de todas las filas de la primera columna de la tabla ?ndices
     xlab = "Margalef", # Etiqueta del eje X
     main = NA) # Sin  t?tulo principal

# Histograma ?ndice de Menhinick
hist(indices[, 2], # Valores de todas las filas de la segunda columna de la tabla ?ndices
     xlab = "Menhinick", # Etiqueta del eje Y
     main = NA)


## ?NDICES DE DOMINANCIA Y EQUITATIVIDAD
library(vegan)
# ?ndice de Gini-Simpson
Simpson <- diversity(BCI, index  = "simpson")

# ?ndice de Shannon
Shannon <- diversity(BCI, index = "shannon")

# ?ndice de Pielou
Pielou <- Shannon / log(S) 

# Combinamos los ?ndices en una tabla
indices <- cbind(Simpson = Simpson, Shannon = Shannon, Pielou = Pielou)
head(indices, 10)
indices <- as.data.frame(indices)
indices

## Creamos gr?ficos para interpretaci?n de valores
#Histograma ?ndice de Simpson
hist(indices[, 1],
     xlab = "Simpson",
     main = NA)

#Histograma ?ndice de Shannon
hist(indices[, 2],
     xlab = "Shannon",
     main = NA)

#Histograma ?ndice de Pielou
hist(indices[, 3],
     xlab = "Pielou",
     main = NA)

