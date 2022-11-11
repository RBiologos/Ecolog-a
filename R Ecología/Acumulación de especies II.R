library(entropart)

# Calculo de la diversidad acumulada para 100 permutaciones
accum <- estaccumR(BCI, permutations = 100)

# Diversidades acumuladas promedio
accum.mean <- as.data.frame(accum$means) 

# Abundancias acumuladas
accum.abundance <- cumsum(rowSums(BCI))

plot(x = accum.mean$N, # Valores de X
     y = accum.mean$S, # Valores de Y
     xlab = "Unidades de muestreo", # Etiqueta del eje X
     ylab = "Riqueza acumulada", # Etiqueta del eje Y
     type = "l", # Tipo de línea (sólida)
     ylim = c(80, 240), # Límites del eje Y (según los datos)
     yaxt = "n") # Remover la división por defecto del eje Y (para usar personalizada)

# Intervalo personalizado del eje Y de la grafica (según los datos)
axis(side = 2, at = seq(80, 240, 40))

# Curva del estimador Chao
lines(x = accum.mean$N, # Valores de X
      y = accum.mean$Chao, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "red") # Color de la línea

# Curva del estimador ACE
lines(x = accum.mean$N, # Valores de X
      y = accum.mean$ACE, # Valores de Y
      type = "l", # Tipo de línea (sólida)
      col = "blue") # Color de la línea

# Leyenda de la grafica
legend(x = "bottomright", # Posición
       legend = c("SObs", "Chao 1", "ACE"), # Texto de la leyenda
       lty = c(1, 1, 1), # Tipos de línea de los símbolos
       col = c("black", "red", "blue"),  # Colores de los símbolos
       lwd = 2, # Grosor de las líneas de los símbolos
       inset = c(0.025, 0.05)) # Márgenes de la leyenda
