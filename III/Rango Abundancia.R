source(choose.files()) # DetAbu
source(choose.files()) # UndAbu
source(choose.files()) # SpecDist
library(iNEXT)
library(Rcpp)
library(ggplot2)
library(ggrepel)
library(extrafont) 
loadfonts(device = "win") ## load fonts from windows

ra<-read.csv(choose.files(),header=T)
data.frame(ra)
names(ra)
attach(ra)

##Potrero
out1 <- SpecDist(P, "abundance")
out1 <- subset(out1, probability > 0)
totalP <- dim(out1)[1]
i=1
P1 <- sort(P, decreasing = TRUE)
for (i in 1:totalP) {
  out1$number[i] <- i
  out1$sp[i] <- X[as.numeric(rownames(out1))][i]
}
out1$Sitio  <- "Potrero"



##Bosque Mixto
out2 <- SpecDist(BM, "abundance")
out2 <- subset(out2, probability > 0)
totalBM <- dim(out2)[1]
i=1
for (i in 1:totalBM) {
  out2$number[i] <- i
  out2$sp[i] <- X[as.numeric(rownames(out2))][i]
}
out2$Sitio  <- "Bosque mixto"
  

##Bosque Secundario
out3 <- SpecDist(BS, "abundance")
out3 <- subset(out3, probability>0)
totalBS <- dim(out3)[1]
i=1
for (i in 1:totalBS) {
  out3$number[i] <- i
  out3$sp[i] <- X[as.numeric(rownames(out3))][i]
}
out3$Sitio  <- "Bosque secundario"

## Unimos
outtotal <- rbind(out1, out2, out3)

dodge <- position_dodge(1)

## Graficamos
ggplot(out1, aes(x = number, y = probability, color = Sitio))+ 
  geom_line(lwd = 1)+
  geom_point(aes(shape = Sitio, color = Sitio), size = 4)+ 
  scale_color_grey(start = 0.8, end = 0.2)+
  scale_x_continuous("Rango de especies")+
  scale_y_continuous("Probabilidad")+
  theme(axis.title.x = element_text(family = "sans", size = 18))+
  theme(axis.text.x = element_text(family = "sans", size = 18))+
  theme(axis.title.y = element_text(family = "sans", size = 18))+
  theme(axis.text.y = element_text(family = "sans", size = 18))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", size = 1))+
  theme(panel.background = element_blank())+ 
  theme(legend.key = element_rect(colour = NA, fill = NA),
        legend.justification = c(1,0), legend.position = c(0.95,0.6),
        legend.text = element_text(family = "sans", size = 14),
        legend.title = element_text(family = "sans", size = 16))
  # Si queremos nombres
  geom_text_repel(mapping = aes(x = number, y=probability, label=sp),
    position = dodge, size = 4, alpha = 0.9, segment.size = .25,
    segment.alpha = .8, force = 1, colour = "black")

