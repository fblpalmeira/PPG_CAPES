Sys.setlocale("LC_ALL","pt_BR.UTF-8")

library (readxl)

#Bot?nica
y1 <- read_excel("quantitativo_instituicao_ensino_bot3.xlsx", na = "-")
names(y1) <- y1[1,]#delete the first header and include the second one as the first
y1 <- y1[-c(1),]#delete the first row which was as a duplicate row
y1 <- head(y1, - 1)#delete the last row which represented a subtotal
y1$Nota <- as.numeric(3)#add a new column wiht value
View(y1)

y2 <- read_excel("quantitativo_instituicao_ensino_bot4.xlsx", na = "-")
names(y2) <- y2[1,]
y2 <- y2[-c(1),]
y2 <- head(y2, - 1) 
y2$Nota <- as.numeric(4)
View(y2)

y3 <- read_excel("quantitativo_instituicao_ensino_bot5.xlsx", na = "-")
names(y3) <- y3[1,]
y3 <- y3[-c(1),]
y3 <- head(y3, - 1) 
y3$Nota <- as.numeric(5)
View(y3)

y4 <- read_excel("quantitativo_instituicao_ensino_bot6.xlsx", na = "-")
names(y4) <- y4[1,]
y4 <- y4[-c(1),]
y4 <- head(y4, - 1) 
y4$Nota <- as.numeric(6)
View(y4)

y5 <- read_excel("quantitativo_instituicao_ensino_bot7.xlsx", na = "-")
names(y5) <- y5[1,]
y5 <- y5[-c(1),]
y5 <- head(y5, - 1) 
y5$Nota <- as.numeric(7)
View(y5)

y<-rbind(y1,y2,y3,y4,y5)
y$Conhecimento <- as.character("Botânica")
bot <- y

#Ecologia
y1 <- read_excel("quantitativo_instituicao_ensino_eco3.xlsx", na = "-")
names(y1) <- y1[1,]
y1 <- y1[-c(1),]
y1 <- head(y1, - 1) 
y1$Nota <- as.numeric(3)
View(y1)

y2 <- read_excel("quantitativo_instituicao_ensino_eco4.xlsx", na = "-")
names(y2) <- y2[1,]
y2 <- y2[-c(1),]
y2 <- head(y2, - 1) 
y2$Nota <- as.numeric(4)
View(y2)

y3 <- read_excel("quantitativo_instituicao_ensino_eco5.xlsx", na = "-")
names(y3) <- y3[1,]
y3 <- y3[-c(1),]
y3 <- head(y3, - 1) 
y3$Nota <- as.numeric(5)
View(y3)

y4 <- read_excel("quantitativo_instituicao_ensino_eco6.xlsx", na = "-")
names(y4) <- y4[1,]
y4 <- y4[-c(1),]
y4 <- head(y4, - 1) 
y4$Nota <- as.numeric(6)
View(y4)

y5 <- read_excel("quantitativo_instituicao_ensino_eco7.xlsx", na = "-")
names(y5) <- y5[1,]
y5 <- y5[-c(1),]
y5 <- head(y5, - 1) 
y5$Nota <- as.numeric(7)
View(y5)

y<-rbind(y1,y2,y3,y4,y5)
y$Conhecimento <- as.character("Ecologia")
eco <- y

#Oceanografia
y1 <- read_excel("quantitativo_instituicao_ensino_oce4.xlsx", na = "-")
names(y1) <- y1[1,]
y1 <- y1[-c(1),]
y1 <- head(y1, - 1) 
y1$Nota <- as.numeric(4)
View(y1)

y2 <- read_excel("quantitativo_instituicao_ensino_oce5.xlsx", na = "-")
names(y2) <- y2[1,]
y2 <- y2[-c(1),]
y2 <- head(y2, - 1) 
y2$Nota <- as.numeric(5)
View(y2)

y3 <- read_excel("quantitativo_instituicao_ensino_oce7.xlsx", na = "-")
names(y3) <- y3[1,]
y3 <- y3[-c(1),]
y3 <- head(y3, - 1) 
y3$Nota <- as.numeric(7)
View(y3)

y<-rbind(y1,y2,y3)
y$Conhecimento <- as.character("Oceanografia")
oce <- y

#Zoologia
y1 <- read_excel("quantitativo_instituicao_ensino_zoo3.xlsx", na = "-")
names(y1) <- y1[1,]
y1 <- y1[-c(1),]
y1 <- head(y1, - 1) 
y1$Nota <- as.numeric(3)
View(y1)

y2 <- read_excel("quantitativo_instituicao_ensino_zoo4.xlsx", na = "-")
names(y2) <- y2[1,]
y2 <- y2[-c(1),]
y2 <- head(y2, - 1) 
y2$Nota <- as.numeric(4)
View(y2)

y3 <- read_excel("quantitativo_instituicao_ensino_zoo5.xlsx", na = "-")
names(y3) <- y3[1,]
y3 <- y3[-c(1),]
y3 <- head(y3, - 1) 
y3$Nota <- as.numeric(5)
View(y3)

y4 <- read_excel("quantitativo_instituicao_ensino_zoo6.xlsx", na = "-")
names(y4) <- y4[1,]
y4 <- y4[-c(1),]
y4 <- head(y4, - 1) 
y4$Nota <- as.numeric(6)
View(y4)

y5 <- read_excel("quantitativo_instituicao_ensino_zoo7.xlsx", na = "-")
names(y5) <- y5[1,]
y5 <- y5[-c(1),]
y5 <- head(y5, - 1) 
y5$Nota <- as.numeric(7)
View(y5)

y<-rbind(y1,y2,y3,y4,y5)
y$Conhecimento <- as.character("Zoologia")
zoo <- y

y<-rbind(bot,eco,oce,zoo)
y$Avaliacao <- as.character("Biodiversidade")
View(y)

##############

library (dplyr)

y1<-y%>%
  select(Conhecimento,Nota) %>% 
  count(Conhecimento,Nota, sort=T)
View(y1)

##############

library(ggplot2)
library(tidyverse)

png(file="boxplot_biodiversidade_sucupira.png", width = 1000, height = 1000)
y$Conhecimento <- factor(y$Conhecimento, levels = c("Oceanografia","Botânica","Zoologia","Ecologia"))
data=data.frame(y$Conhecimento,y$Conhecimento,y$Nota)
levels(data$y.Conhecimento)
p<-ggplot(data,aes(x=reorder(y.Conhecimento,-y.Nota,FUN=median,na.rm = TRUE), 
                   y=y.Nota,color=y$Conhecimento)) + 
  geom_boxplot(position = position_dodge(preserve = "single")) +  xlab("Área de Conhecimento") + ylab("Nota") + 
  ggtitle("Biodiversidade") +
  geom_hline(yintercept = 7, colour = "gray", linetype="dashed") + 
  geom_point()+
  scale_color_manual(
                    values = c("steelblue", "#009E73", "#CC79A7", "orange"))+
  theme_bw()+
  theme (plot.title = element_text(colour="darkgreen", size=30, hjust = 0.5),
         legend.position = "none",
         legend.title = element_blank()) +
  theme (panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank())+
  theme (panel.grid.major.y = element_blank(), 
         panel.grid.minor.y = element_blank())+
  theme (axis.title.x=element_text(size=20))+ 
  theme (axis.title.y=element_text(size=20))+
  theme (axis.text.x=element_text(size=14))+ 
  theme (axis.text.y=element_text(size=14))
p
dev.off()
