library(tidyr)
library(ggridges)
library(ggplot2)
library(corrplot)
library(RColorBrewer)

m1<-read.table("Spotify31.csv", sep=",", header=TRUE)
attach(m1)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}



M<-cor(m1[1:11], method = "spearman", use= "pairwise.complete.obs")
p.mat<-cor.mtest(m1[1:11], method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163', 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Total", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

#Correlográmas de cada Género con las demás variables
names(m1)

Pop<-m1[m1$Genero== "Pop", c(1:11)]
p.mat<-cor.mtest(Pop, method = "spearman", use= "pairwise.complete.obs")
M<-cor(Pop, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163',
         col=brewer.pal(n=8, name="RdYlBu"), title = "Pop", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black") 

Rock<-m1[m1$Genero== "Rock", c(1:11)]
p.mat<-cor.mtest(Rock, method = "spearman", use= "pairwise.complete.obs")
M<-cor(Rock, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163', 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Rock", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Country<-m1[m1$Genero== "Country", c(1:11)]
p.mat<-cor.mtest(Country, method = "spearman", use= "pairwise.complete.obs")
M<-cor(Country, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163', 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Country", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Metal<-m1[m1$Genero== "Metal", c(1:11)]
p.mat<-cor.mtest(Metal, method = "spearman", use= "pairwise.complete.obs")
M<-cor(Metal, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163', 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Metal", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Hiphop<-m1[m1$Genero== "Hip-Hop", c(1:11)]
p.mat<-cor.mtest(Hiphop, method = "spearman", use= "pairwise.complete.obs")
M<-cor(Hiphop, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163',
         col=brewer.pal(n=8, name="RdYlBu"), title = "Hip-Hop", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

RB<-m1[m1$Genero== "R&B", c(1:11)]
p.mat<-cor.mtest(RB, method = "spearman", use= "pairwise.complete.obs")
M<-cor(RB, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163', 
         col=brewer.pal(n=8, name="RdYlBu"), title = "R&B", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Dance<-m1[m1$Genero== "Dance/Electronic", c(1:11)]
p.mat<-cor.mtest(Dance, method = "spearman", use= "pairwise.complete.obs")
M<-cor(Dance, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163', 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Dance/Electronic", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Folk<-m1[m1$Genero== "Folk/Acoustic", c(1:11)]
p.mat<-cor.mtest(Folk, method = "spearman", use= "pairwise.complete.obs")
M<-cor(Folk, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163', 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Folk/Acoustic", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Easy<-m1[m1$Genero== "Easy-Listening", c(1:11)]
p.mat<-cor.mtest(Easy, method = "spearman", use= "pairwise.complete.obs")
M<-cor(Easy, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",,p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163',
         col=brewer.pal(n=8, name="RdYlBu"), title = "Easy-Listening", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

Latin<-m1[m1$Genero== "Latin", c(1:11)]
p.mat<-cor.mtest(Latin, method = "spearman", use= "pairwise.complete.obs")
M<-cor(Latin, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163',
         col=brewer.pal(n=8, name="RdYlBu"), title = "Latin", mar=c(0,0,2,0), tl.cex = 0.8, tl.col = "black")

Blues<-m1[m1$Genero== "Blues", c(1:11)]
p.mat<-cor.mtest(Blues, method = "spearman", use= "pairwise.complete.obs")
M<-cor(Blues, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163', 
         col=brewer.pal(n=8, name="RdYlBu"), title = "Blues", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

World<-m1[m1$Genero== "World-Tradicional", c(1:11)]
p.mat<-cor.mtest(World, method = "spearman", use= "pairwise.complete.obs")
M<-cor(World, method = "spearman", use= "pairwise.complete.obs")
corrplot(M, type="upper", order="original",p.mat = p.mat, sig.level = 0.05, insig = "blank",addCoef.col = '#5f6163', 
         col=brewer.pal(n=8, name="RdYlBu"), title = "World-Tradicional", mar=c(0,0,1,0), tl.cex = 0.8, tl.col = "black")

