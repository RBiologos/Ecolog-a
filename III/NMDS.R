library(ggplot2)
library(GGally)
library(CCA)
library(MASS)
library(vegan)
library(dplyr)
library(readxl)

os <- read_xlsx(choose.files(), sheet = "n=27" , col_names = TRUE)
os <- data.frame(os)
names(os)

# Erase place column
yox <- os[,-1]
yox

#nmds
nmds <- metaMDS(yox, k = 2, distance = "bray")
nmds

#bondad de ajuste para nmds
bdan <- goodness(nmds)
print(bdan)
sum(bdan)
mean(bdan)  

nmds$points
nmds.scores <- scores(nmds)
nmds.scores

# Graph to place
# Convert to data frame
place <- data.frame(nmds.scores)
# Add place
place$sitio <- os$Sitio

place$Cobertura <- c("P", "P", "P",
                 "P", "P",
                 "P", "P", "P",
                 "BM", "BM", "BM",
                 "BM", "BM", "BM",
                 "BM", "BM", "BM",
                 "BS", "BS", "BM",
                 "BS", "BS", "BS",
                 "BS", "BS", "BS")


place %>%
ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(shape=Cobertura, color=Cobertura), size = 5)+
  scale_color_grey(start = 0.9, end = 0.2)+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", size = 1))+ 
  theme(axis.title.x = element_text(family = "sans", size = 18))+
  theme(axis.text.x = element_text(family = "sans", size = 18))+
  theme(axis.title.y = element_text(family = "sans", size = 18))+
  theme(axis.text.y = element_text(family = "sans", size = 18))+
  theme(legend.key = element_rect(colour = NA, fill = NA),
        legend.text = element_text(family = "sans", size = 14),
        legend.title = element_text(family = "sans", size = 16))
