## En este script están contenidas las funciones necesarias para el
## curso de Ecología en R

# Cargamos las librerías necesarias
library(dplyr)

# Leemos la base de datos directamente desde la URL
url <- "https://raw.githubusercontent.com/RBiologos/Ecologia/main/Especies.csv"
base <- read.csv(url, header = TRUE)

# Observamos los datos en una nueva pestaña
View(base)

# Y reconocemos los tipos de datos que tenemos para cada variable
# Recordemos que las dos primeras columnas son de los sitios muestreados
# Así, podremos conocer también los nombres de nuestras especies
str(base)

# Reconocemos que los sitios tengan la mismca cantidad de muestras para
# poder compararlos
base %>%
  count(Sitio, sort = TRUE)

# Observamos la abundancia de cada especie en cada bosque
grp_sitio <- base %>%
  group_by(Sitio) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE)
View(grp_sitio)

# Observamos las abundancia de todas las especie en cada bosque
abu_sitio <- base %>%
  group_by(Sitio) %>%
  summarise(Total = sum(across(where(is.numeric))))
View(abu_sitio)
abu_sitio

# Observamos el total de especies en cada bosque
grp_sitio[-1] %>%
  summarise(Especies_totales = rowSums(.!=0))
