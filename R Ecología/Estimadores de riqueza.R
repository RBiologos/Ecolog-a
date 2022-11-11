library(entropart)
library(BiodiversityR)

data(BCI)
View(BCI)
dim(BCI)


# Estimadores de riqueza por unidad de muestreo
est.Sites <- estimateR(BCI)
View(est.Sites)

# Estimadores de riqueza para la comunidad entera
est.Comm <- estimateR(colSums(BCI))
est.Comm <- specpool(BCI, smallsample = TRUE)
View(est.Comm)
