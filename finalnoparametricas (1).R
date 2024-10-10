m1<- read.csv("Spotify31.csv")
attach(m1)
names(m1)
library(ggplot2)

kruskal.test(m1[,3],m1[,13],data=m1) #Si hay diferencias significativas entre ciertos periodos
pairwise.wilcox.test(x=popularity, g=period, p.adjust.method = "holm")
ggplot(m1, aes(x=period, y=popularity, fill = period)) + 
  geom_boxplot() + ggtitle("Popularidad por periodo")

kruskal.test(m1[,1],m1[,13], data=m1) #duración en base al periodo
pairwise.wilcox.test(x=duration_min, g=period, p.adjust.method = "holm")
ggplot(m1, aes(x=period, y=duration_min, fill = period)) + 
  geom_boxplot() + ggtitle("Duración por periodo")

kruskal.test(m1[,10],m1[,13],data=m1) # si hay diferencias entre las valencias conforme los periodos
pairwise.wilcox.test(x=valence, g=period, p.adjust.method = "holm")
ggplot(m1, aes(x=period, y=valence, fill = period)) + 
  geom_boxplot() + ggtitle("Valencia por periodo") 

kruskal.test(m1[,4],m1[,13],data=m1) # si hay en bailabilidad por periodo
pairwise.wilcox.test(x=danceability, g=period, p.adjust.method = "holm")
ggplot(m1, aes(x=period, y=danceability, fill = period)) + 
  geom_boxplot() + ggtitle("Bailabilidad por periodo") 

kruskal.test(m1[,5],m1[,13],data=m1) #si hay en energía por periodo
pairwise.wilcox.test(x=energy, g=period, p.adjust.method = "holm")
ggplot(m1, aes(x=period, y=energy, fill = period)) + 
  geom_boxplot() + ggtitle("Energía por periodo") 

kruskal.test(m1[,6],m1[,13],data=m1) # si hay ruido por periodo
pairwise.wilcox.test(x=loudness, g=period, p.adjust.method = "holm")
ggplot(m1, aes(x=period, y=loudness, fill = period)) + 
  geom_boxplot() + ggtitle("Ruido por periodo") 

kruskal.test(m1[,8],m1[,13],data=m1) #Si existe entre palabras por periodos
pairwise.wilcox.test(x=speechiness, g=period, p.adjust.method = "holm")
ggplot(m1, aes(x=period, y=speechiness, fill = period)) + 
  geom_boxplot() + ggtitle("Palabras por periodo") 

kruskal.test(m1[,9],m1[,13],data=m1) #Si existe liveness por periodo
pairwise.wilcox.test(x=liveness, g=period, p.adjust.method = "holm")
ggplot(m1, aes(x=period, y=liveness, fill = period)) + 
  geom_boxplot() + ggtitle("Audiencia por periodo") 

kruskal.test(m1[,11],m1[,13],data=m1) 
pairwise.wilcox.test(x=tempo, g=period, p.adjust.method = "holm")
ggplot(m1, aes(x=period, y=tempo, fill = period)) + 
  geom_boxplot() + ggtitle("Tempo por periodo") 

wilcox.test(m1[energy>0.5,3], m1[energy<0.5,3])# no hay diferencias significativas
boxplot(m1[energy>0.5,3], m1[energy<0.5,3],
        col = c("#09cce6", "#7f09e6"), 
        main = "Inferencia de Energy",                    
        xlab = "Energy",                               
        ylab = "Popularity",                               
        names = c("Mayor", "Menor"))


wilcox.test(m1[danceability>0.5,3], m1[danceability<0.5,3])# no hay diferencias significativas
boxplot(m1[danceability>0.5,3], m1[danceability<0.5,3],
        col = c("#09cce6", "#7f09e6"), 
        main = "Inferencia de Danceability",                    
        xlab = "Danceability",                               
        ylab = "Popularity",                               
        names = c("Mayor", "Menor"))


wilcox.test(m1[loudness>-5,3], m1[loudness<(-5),3])# no hay diferencias significativas
boxplot(m1[loudness>-5,3], m1[loudness<(-5),3], 
        col = c("#09cce6", "#7f09e6"), 
        main = "Inferencia de Loudness",                    
        xlab = "Loudness",                               
        ylab = "Popularity",                               
        names = c("Mayor", "Menor"))


wilcox.test(m1[tempo>119,3], m1[tempo<119,3])# no hay diferencias significativas
boxplot(m1[tempo>119,3], m1[tempo<119,3],
        col = c("#09cce6", "#7f09e6"), 
        main = "Inferencia de Tempo",                    
        xlab = "Loudness",                               
        ylab = "Popularity",                               
        names = c("Mayor", "Menor"))

wilcox.test(m1[valence>0.5,3], m1[valence<0.5,3])
boxplot(m1[valence>0.5,3], m1[valence<0.5,3],
        col = c("#09cce6", "#7f09e6"), 
        main = "Inferencia de Valence",                    
        xlab = "Loudness",                               
        ylab = "Popularity",                               
        names = c("Mayor", "Menor"))

wilcox.test(m1[mode == 1, 3], m1[mode == 0, 3]) #No hay diferencia significativa
boxplot(m1[mode == 1, 3], m1[mode == 0, 3],
        col = c("#09cce6", "#7f09e6"), 
        main = "Inferencia de Mode",                    
        xlab = "Loudness",                               
        ylab = "Popularity",                               
        names = c("Mayor", "Menor"))

kruskal.test(m1[,3],m1[,12],data=m1) #Si hay diferencias significativas entre ciertos generos
pairwise.wilcox.test(x=popularity, g=Generos, p.adjust.method = "holm")
boxplot(m1[Generos == "Rock", 3], m1[Generos == "Dance/Electronic", 3])
boxplot(m1[Generos == "Hip-Hop", 3], m1[Generos == "Dance/Electronic", 3])
boxplot(m1[Generos == "Metal", 3], m1[Generos == "Country", 3])
boxplot(m1[Generos == "Metal", 3], m1[Generos == "Dance/Electronic", 3])
boxplot(m1[Generos == "Pop", 3], m1[Generos == "Rock", 3])
boxplot(m1[Generos == "Hip-Hop", 3], m1[Generos == "R&B", 3])
boxplot(m1[Generos == "Metal", 3], m1[Generos == "R&B", 3])
boxplot(m1[Generos == "Rock", 3], m1[Generos == "R&B", 3])
ggplot(m1, aes(x=Generos, y=popularity, fill = Generos)) + 
  geom_boxplot() + ggtitle("Inferencias popularidad por Géneros") +coord_flip()
