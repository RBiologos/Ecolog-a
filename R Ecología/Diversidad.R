library(ggplot2)
library(iNEXT)
library(cowplot)
library(ggpubr)
library(grid)
library(extrafont) 
loadfonts(device = "win") ## load fonts from windows

dungbeetles= read.csv("RA.csv")
dungbeetles<-dungbeetles[-1]

q0 <- iNEXT(dungbeetles, q=c(0), datatype="abundance", endpoint=1000)
q1 <- iNEXT(dungbeetles, q=c(1), datatype="abundance", endpoint=1000)
q2 <- iNEXT(dungbeetles, q=c(2), datatype="abundance", endpoint=1000)

# q=0, Type 1 -------------------------------------------------------------
dfq0t1 <- fortify(q0, type=1)
# Ordenar por sitios (factores)
dfq0t1$site <- factor(dfq0t1$site, levels = c("BS", "BM", "P"), ordered = TRUE)
df.pointq0t1 <- dfq0t1[which(dfq0t1$method=="observed"),]
df.lineq0t1 <- dfq0t1[which(dfq0t1$method!="observed"),]
# Cambiamos nombres para el gráfico
df.lineq0t1$method <- factor(df.lineq0t1$method,
                             c("interpolated", "extrapolated"),
                             c("Interpolación", "Extrapolación"))

q0t1 <- ggplot(dfq0t1, aes(x=x, y=y, colour=site)) +
        geom_point(aes(shape = site), size=4, data=df.pointq0t1) +
        geom_line(aes(linetype = method), lwd=1, data=df.lineq0t1) +
        geom_ribbon(aes(ymin = y.lwr, ymax = y.upr,
                        fill = site, colour = NULL), alpha = 0.2) +
        scale_colour_manual(values=c("grey30", "grey30","grey30"))+
        scale_fill_manual(values=c("aquamarine4", "aquamarine3","aquamarine2"))+
        theme(axis.text.x = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(axis.text.y = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(axis.title.x = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(axis.title.y = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
        theme(panel.background = element_blank(), legend.position = "none")+
        labs(x = "Cobertura de muestreo", y = "Número de especies",
             family = "Arial", size = 14)+
        theme(legend.position="right", legend.title=element_blank(),
              text=element_text(size=30), legend.key=element_blank())
legend.q0t1 <- cowplot::get_legend(q0t1)
leg.q0t1 <- as_ggplot(legend.q0t1)

# q=0, Type 2 -------------------------------------------------------------
dfq0t2 <- fortify(q0, type=2)
# Ordenar por sitios (factores)
dfq0t2$site <- factor(dfq0t2$site, levels = c("BS", "BM", "P"), ordered = TRUE)
df.point2 <- dfq0t2[which(dfq0t2$method=="observed"),]
df.line2 <- dfq0t2[which(dfq0t2$method!="observed"),]
# Cambiamos nombres para el gráfico
df.line2$method <- factor(df.line2$method,
                         c("interpolated", "extrapolated"),
                         c("Interpolación", "Extrapolación"))

q0t2 <- ggplot(dfq0t2, aes(x=x, y=y, colour=site)) +
        geom_point(aes(shape = site), size=4, data=df.point2) +
        geom_line(aes(linetype = method), lwd=1, data=df.line2) +
        geom_ribbon(aes(ymin = y.lwr, ymax = y.upr,
                        fill = site, colour = NULL), alpha = 0.2) +
        scale_colour_manual(values=c("grey30", "grey30","grey30"))+
        scale_fill_manual(values=c("aquamarine4", "aquamarine3","aquamarine2"))+
        theme(axis.text.x = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(axis.text.y = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(axis.title.x = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(axis.title.y = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
        theme(panel.background = element_blank(), legend.position = "none")+
        labs(x = "Número de individuos", y = "Cobertura de muestreo",
             family = "Arial", size = 14) +
        theme(legend.position="right", legend.title=element_blank(),
              text=element_text(size=30), legend.key=element_blank())
legend.q0t2 <- cowplot::get_legend(q0t2)
leg.q0t2 <- as_ggplot(legend.q0t2)

# q=0, Type 3  ------------------------------------------------------------
dfq0t3 <- fortify(q0, type = 3)
# Ordenar por sitios (factores)
dfq0t3$site <- factor(dfq0t3$site, levels = c("BS", "BM", "P"), ordered = TRUE)
df.pointq0t3 <- dfq0t3[which(dfq0t3$method=="observed"),]
df.lineq0t3 <- dfq0t3[which(dfq0t3$method!="observed"),]
# Cambiamos nombres para el gráfico
df.lineq0t3$method <- factor(df.lineq0t3$method,
                         c("interpolated", "extrapolated"),
                         c("Interpolación", "Extrapolación"))

q0t3 <- ggplot(dfq0t3, aes(x=x, y=y, colour=site)) +
        geom_point(aes(shape = site), size=4, data=df.pointq0t3) +
        geom_line(aes(linetype = method), lwd=1, data=df.lineq0t3) +
        geom_ribbon(aes(ymin = y.lwr, ymax = y.upr,
                        fill = site, colour = NULL), alpha = 0.2) +
        scale_colour_manual(values=c("grey30", "grey30","grey30"))+
        scale_fill_manual(values=c("aquamarine4", "aquamarine3","aquamarine2"))+
        theme(axis.text.x = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(axis.text.y = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(axis.title.x = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(axis.title.y = element_text(family = "Arial", size = 14, colour = "black"))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
        theme(panel.background = element_blank(), legend.position = "none")+
        labs(x = "Cobertura de muestreo", y = "Número de especies",
             family = "Arial", size = 14)+
        theme(legend.position="right", legend.title=element_blank(),
              text=element_text(size=30), legend.key=element_blank())
legend.q0t3 <- cowplot::get_legend(q0t3)
leg.q0t3 <- as_ggplot(legend.q0t3)

# Plot --------------------------------------------------------------------
png("tipoI.png", width = 1800, height = 1500, res = 200)
# Move to a new page
print(q0t1)

dev.off()
