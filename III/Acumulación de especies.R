library(BiodiversityR)
library(vegan)
library(ggplot2)
library(extrafont) 
loadfonts(device = "win") ## load fonts from windows

data(BCI)
data <- BCI

head(data)
dim(data)
data <- data[-1]

curva <- specaccum(data, ci = 2, permutations = 1000)

# Plot base
plot(curva)

# Replantear plot
dataset <- data.frame(matrix(ncol = 5, nrow = 50))
namecol <- c("sites", "richness", "sd", "upper", "lower")
colnames(dataset) <- namecol
dataset$sites<-curva$sites
dataset$richness <- curva$richness
dataset$sd <- curva$sd
dataset$upper <- dataset$sd+dataset$richness
dataset$lower <- dataset$richness-dataset$sd
dataset$sites <- factor(dataset$sites, 
                        levels = c(1:50), 
                        ordered = TRUE)

p <- ggplot(dataset, aes(x=sites, y=richness, group = 1))+
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 0.7)+
        geom_line(size = 0.7, color = "grey")+
        geom_point()+
        scale_y_continuous()+
        theme(axis.text.x = element_text(family = "Arial", size = 16))+
        theme(axis.text.y = element_text(family = "Arial", size = 16))+
        theme(axis.title.x = element_text(family = "Arial", size = 16))+
        theme(axis.title.y = element_text(family = "Arial", size = 16))+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
        theme(panel.background = element_blank(), legend.position = "none")+
        labs(x = "Sitios", y = "Riqueza de especies",
             family = "Arial", size = 16)

png("AcumSp.png", width = 1800, height = 1500, res = 200)
print(p)
dev.off()

