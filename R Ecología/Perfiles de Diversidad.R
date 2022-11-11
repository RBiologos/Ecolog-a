library(dplyr)
library(ggplot2)

aranias <- read.csv(choose.files(), header = TRUE, 
                    stringsAsFactors = FALSE)

## Eliminar columnas no deseadas
especies <- select(aranias, -mes, -sitio, -transecto)

## Obtener abundancias totales
numerosEspecies <- colSums(especies) 

q <- iNEXT(numerosEspecies, datatype = "abundance")

perfil <- as.data.frame(q$AsyEst)
nombperfil <- rownames(perfil)
perfil <- data.frame(names= nombperfil, perfil)
rownames(perfil)<- NULL

### Filtrar estimadores
perfil$orden <- c("Q0", "Q1", "Q2")
perfil$ordenNumerico <- 1:3
perfil <- select(perfil, names, orden, ordenNumerico,
                              Estimator, X95..Lower, X95..Upper)

## Graficar estimadores con intervalos de confianza al 95%
ggplot(perfil, aes(x = ordenNumerico, y= Estimator))+
  geom_point(shape="*", size=8)+
  geom_line()+
  scale_x_continuous(breaks = 1:3, labels = c("q0", "q1", "q2"))+
  xlab("Órdenes de diversidad")+
  ylab("Estimadores")+
  geom_text(aes(x = ordenNumerico, y= Estimator, label=Estimator),
            hjust=0.5, vjust=-0.5)+
  geom_errorbar(aes(ymin=X95..Lower, ymax=X95..Upper),
                linetype="dotted")+
  scale_y_continuous(breaks = seq(0, 60, 5), limits = c(0, 60))+
  theme(axis.title.x = element_text(family = "sans", size = 18))+
  theme(axis.text.x = element_text(family = "sans", size = 18))+
  theme(axis.title.y = element_text(family = "sans", size = 18))+
  theme(axis.text.y = element_text(family = "sans", size = 18))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", size = 1))+
  theme(panel.background = element_blank()) 
