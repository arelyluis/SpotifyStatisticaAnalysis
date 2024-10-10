#install.packages("tidyr")
#install.packages("ggridges")
library(tidyr)
library(ggridges)
library(ggplot2)
library(corrplot)
library(RColorBrewer)

setwd("C:/Users/anaba/OneDrive/Documents/MATERIAS/Estadística R/spotify")
m1<-read.table("Spotify31.csv", sep=",", header=TRUE)
attach(m1)

# Gráficas de caja y Bigotes
p1<-ggplot(m1, aes(x=period, y=valence, fill=Generos)) + 
  geom_boxplot() +
  facet_wrap(~Generos, scale="free") + ggtitle("Valencia vs Periodos por Géneros")
p1

p2<-ggplot(m1, aes(x=period, y=popularity, fill=Generos)) + 
  geom_boxplot() +
  facet_wrap(~Generos, scale="free") + ggtitle("Popularidad vs Periodos por Géneros ")
p2

p3<-ggplot(m1, aes(x=period, y=duration_min, fill=Generos)) + 
  geom_boxplot() +
  facet_wrap(~Generos, scale="free") + ggtitle("Duración vs Periodos por Géneros")
p3

p4<-ggplot(m1, aes(x=period, y=danceability, fill=Generos)) + 
  geom_boxplot() +
  facet_wrap(~Generos, scale="free") + ggtitle("Bailabilidad vs Periodos por Géneros")
p4

p5<-ggplot(m1, aes(x=period, y=duration_min)) + 
  geom_boxplot() + ggtitle("Duración vs Periodo")
p5

p6<-ggplot(m1, aes(x=Generos, y=mode)) + 
  geom_boxplot() + ggtitle("Duración vs Periodo")
p6

p7<-ggplot(m1, aes(x=period, y=tempo, fill=Generos)) + 
  geom_boxplot() + 
  facet_wrap(~Generos, scale="free") + ggtitle("Tempo vs Periodos por Géneros")
p7

p8<-ggplot(m1, aes(x=period, y=tempo)) + 
  geom_boxplot()  + ggtitle("A boxplot with jitter")
p8

p9<-ggplot(m1, aes(x=period, y=speechiness, fill=Generos)) + 
  geom_boxplot() +
  facet_wrap(~Generos, scale="free") + ggtitle("Palabras vs Periodos por Géneros")
p9

p10<-ggplot(m1, aes(x=period, y=loudness, fill=Generos)) + 
  geom_boxplot() +
  facet_wrap(~Generos, scale="free") + ggtitle("Ruido vs Periodos por Géneros")
p10

p10<-ggplot(m1, aes(x=period, y=energy, fill=Generos)) + 
  geom_boxplot() +
  facet_wrap(~Generos, scale="free") + ggtitle("Energía vs Periodos por Géneros")
p10

p8<-ggplot(m1, aes(x=period, y=duration_min, fill = period)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue", "salmon", "lightgreen","pink")) + 
  ggtitle("Duración por Periodo") 
p8

p8<-ggplot(m1, aes(x=period, y=valence, fill = period)) + 
  geom_boxplot() + scale_fill_manual(values = c("skyblue", "salmon", "lightgreen","pink")) + 
  ggtitle("Valencia por Periodo") 
p8

#Correlográmas de cada Género con las demás variables
names(m1)

Pop<-m1[m1$Genero== "Pop", c(1:11)]
M<-cor(Pop, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Pop", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black") 

Rock<-m1[m1$Genero== "Rock", c(1:11)]
M<-cor(Rock, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Rock", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Country<-m1[m1$Genero== "Country", c(1:11)]
M<-cor(Country, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Country", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Metal<-m1[m1$Genero== "Metal", c(1:11)]
M<-cor(Metal, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Metal", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Hiphop<-m1[m1$Genero== "Hip-Hop", c(1:11)]
M<-cor(Hiphop, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Hip-Hop", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

RB<-m1[m1$Genero== "R&B", c(1:11)]
M<-cor(RB, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "R&B", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Dance<-m1[m1$Genero== "Dance/Electronic", c(1:11)]
M<-cor(Dance, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Dance/Electronic", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Folk<-m1[m1$Genero== "Folk/Acoustic", c(1:11)]
M<-cor(Folk, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Folk/Acoustic", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Easy<-m1[m1$Genero== "Easy-Listening", c(1:11)]
M<-cor(Easy, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Easy-Listening", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Latin<-m1[m1$Genero== "Latin", c(1:11)]
M<-cor(Latin, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Latin", mar=c(0,0,2,0), tl.cex = 0.8, tl.col = "black")

Blues<-m1[m1$Genero== "Blues", c(1:11)]
M<-cor(Blues, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Blues", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

World<-m1[m1$Genero== "World-Tradicional", c(1:11)]
M<-cor(World, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original", 
         col=brewer.pal(n=8, name="RdYlBu"), title = "World-Tradicional", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")


# Estadística descriptiva

FisherAssymetry<-function(x){sum((x-mean(x))^3) / (sd(x)^3*length(x)) }

descriptiva<-function(x){c(mean(x), 
                           median(x),
                           sd(x),
                           var(x),
                           max(x),
                           min(x),
                           max(x)-min(x),
                           FisherAssymetry(x))}

labels<-c("media", "mediana", "desviación estandar", "varianza", "max", "min", 
          "rango", "CAF")
# Tabla de dataframe en general 
descripcion<-data.frame(apply(m1[,1:11], 2, descriptiva))
rownames(descripcion)<-labels
descripcion

names(m1)

period1<-m1[period== "1-98/04", c(1:11)]
period2<-m1[period== "2-05/10", c(1:11)]
period3<-m1[period== "3-11/15", c(1:11)]
period4<-m1[period== "4-16/20", c(1:11)]

descripcion1<-data.frame(apply(period1, 2, descriptiva))
rownames(descripcion1)<-labels
descripcion1

descripcion2<-data.frame(apply(period2, 2, descriptiva))
rownames(descripcion2)<-labels
descripcion2

descripcion3<-data.frame(apply(period3, 2, descriptiva))
rownames(descripcion3)<-labels
descripcion3

descripcion4<-data.frame(apply(period4, 2, descriptiva))
rownames(descripcion4)<-labels
descripcion4


descripcionMetal<-data.frame(apply(Metal, 2, descriptiva))
rownames(descripcionMetal)<-labels
descripcionMetal

descripcionPop<-data.frame(apply(Pop, 2, descriptiva))
rownames(descripcionPop)<-labels
descripcionPop



boxplots_final<-m1[,1:11] %>% pivot_longer(cols = c('duration_min', 'year', 'popularity', 'danceability', 'energy', 
                                                       'loudness', 'mode', 'speechiness', 'liveness', 
                                                       'valence', 'tempo'), names_to = "stats",
                                              values_to = "points")


#ridgeline(boxplots_final$points, boxplots_final$stats)

ggplot(m5, aes(x = m5$valence, y = m5$Generos, fill = color)) +
  geom_density_ridges(fill = "lightblue", alpha = 0.5)


m2<-m1[c("danceability","energy","speechiness","liveness","valence","period")]
prueba <- gather(m2, key = "Measurement", value = "Value", -period)

P<-ggplot(prueba, aes(x = Value, y = Measurement, fill = prueba$period)) +
  geom_density_ridges(alpha = 0.5, bandwidth = 0.5)+
  labs(title = "Distribuciones de las mediciones musicales por periodo",
       x = "Valor", 
       y = "Medición")

P

m2<-m1[c("danceability","energy","speechiness","liveness","valence","Generos")]
prueba <- gather(m2, key = "Measurement", value = "Value", -Generos)


P<-ggplot(m1, aes(x = valence, y = Generos, fill = Generos)) +
  geom_density_ridges(alpha = 0.5, bandwidth = 0.5)+
  labs(title = "Distribuciones de las mediciones musicales por Genero",
       x = "Popularidad") 

P
P<-ggplot(m1, aes(x = year, y = Generos, fill = Generos)) +
  geom_density_ridges(alpha = 0.5, bandwidth = 0.5)+
  labs(title = "Distribución de Géneros por Año",
       x = "Year") 

P
