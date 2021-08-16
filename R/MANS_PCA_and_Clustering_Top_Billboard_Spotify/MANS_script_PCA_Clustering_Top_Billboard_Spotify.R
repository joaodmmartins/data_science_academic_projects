#############################################################################
# UC: Métodos de Aprendizagem Não Supervisionada
#
# Trabalho de Grupo: Análise de Componentes Principais e de Clustering do 
#   dataset "Top Spotify songs from 2010-2019 - BY YEAR"
#
#
# Catarina Castanheira, n.º 92478
# João Martins, n.º 93259
# Joel Paula, n.º 93392
#############################################################################
#############################################################################

library(corrplot)
library(psych)
library(car)
library(cluster)
library(mclust)
library(tidyverse)
library(caret)
library(factoextra)

set.seed(42)

## PARTE 0: LIMPEZA DE DADOS E ANÁLISE EXPLORATÓRIA

df_spotify <- read.csv("top10s.csv", header=TRUE)
head(df_spotify)

sapply(df_spotify, class) # obter o tipo de cada variável
summary(df_spotify) # estatísticas descritivas

#nr de artists:
nrow(table(df_spotify$artist))
# nr de genres:
nrow(table(df_spotify$top.genre))
gnrs <- sort(table(df_spotify$top.genre), decreasing = TRUE)
barplot(gnrs, las=2)
library(lessR)
others <- nrow(gnrs)-23
gnrs <- gnrs[0:22]
gnrs[23] <- 602-sum(gnrs)
names(gnrs)[23] <- paste(others, "others")
PieChart(gnrs, hole = 0, values = "%", main = "top genres", labels_cex=0.9, oma=c(2,2,2,2))

# quantas músicas por year:
sort(table(df_spotify$year), decreasing = TRUE)
barplot(table(df_spotify$year), las=2)

# Procura de zeros:
df_spotify[df_spotify$bpm == 0 | df_spotify$nrgy == 0 | 
             df_spotify$dnce == 0 | df_spotify$val == 0 | 
             df_spotify$acous == 0| df_spotify$spch == 0 |
             df_spotify$pop == 0 | df_spotify$live == 0,]
# zero para acoustics parece legítimo

# Example of acoustic:
df_spotify[df_spotify$acous == 99,]
# only acoustic instruments (piano, guitar)

# restantes zeros:
df_spotify[df_spotify$bpm == 0 | df_spotify$nrgy == 0 | 
             df_spotify$dnce == 0 | df_spotify$val == 0 | 
             df_spotify$spch == 0 | df_spotify$pop == 0 | df_spotify$live == 0,]
# Música "Million Years Ago" (index 443) de "Adele" parece ter 0 (zero) para
# cada atributo, excepto para loudness (dB) e duration.

# removemos a observação do dataset?
df_spotify = df_spotify[-443,]

# Processamento de missing values:
which(is.na(df_spotify)) # não existem missing values

# # Duplicados?
# df_spotify %>% select(-year, -X) %>% anyDuplicated()
# ## 55 duplicates
# # remover duplicados
# dups <- df_spotify %>% select(-year, -X) %>% duplicated()
# df_spotify <- df_spotify[!dups,]
# desistimos pois só vai piorar o facto de o género "dance pop" ser preponderante

df_mt_vars <- df_spotify[, 5:length(colnames(df_spotify))] # dataframe só com as 
#   variáveis métricas (excepto a year, que está também aqui, embora seja 
#   conceptualmente categórica)

#Desvio padrão
apply(df_mt_vars, 2, sd)


# artistas por estilo
table(df_spotify[df_spotify$artist=="Adele",]$top.genre)
table(df_spotify[df_spotify$artist=="Lady Gaga",]$top.genre)
table(df_spotify[df_spotify$artist=="Katy Perry",]$top.genre)
table(df_spotify[df_spotify$artist=="Justin Bieber",]$top.genre)
table(df_spotify[df_spotify$artist=="Bruno Mars",]$top.genre)
table(df_spotify[df_spotify$artist=="Maroon 5",]$top.genre)
table(df_spotify[df_spotify$artist=="Christina Aguilera",]$top.genre)
table(df_spotify[df_spotify$artist=="Jennifer Lopez",]$top.genre)
# Só parecem estar associados a um estilo

# estudar as covariâncias para as variáveis métricas:
var(df_mt_vars)
# análise de distribuições:
#scatterplotMatrix(df_mt_vars)
pairs(df_mt_vars, lower.panel = NULL)

# estudar as correlações para as variáveis métricas:
corrs <- cor(df_mt_vars) # matriz de correlações

# Identificação das correlações 'fortes':
corrs[(corrs[,'year'] >= 0.5 | corrs[,'year'] <= -0.5), 'year'] # correlações médias e elevadas com 'year'
corrs[(corrs[,'bpm'] >= 0.5 | corrs[,'bpm'] <= -0.5), 'bpm'] # correlações médias e elevadas com 'bpm'
corrs[(corrs[,'nrgy'] >= 0.5 | corrs[,'nrgy'] <= -0.5), 'nrgy'] # correlações médias e elevadas com 'nrgy'
corrs[(corrs[,'dnce'] >= 0.5 | corrs[,'dnce'] <= -0.5), 'dnce'] # correlações médias e elevadas com 'dnce'
corrs[(corrs[,'dB'] >= 0.5 | corrs[,'dB'] <= -0.5), 'dB'] # correlações médias e elevadas com 'dB'
corrs[(corrs[,'live'] >= 0.5 | corrs[,'live'] <= -0.5), 'live'] # correlações médias e elevadas com 'live'
corrs[(corrs[,'val'] >= 0.5 | corrs[,'val'] <= -0.5), 'val'] # correlações médias e elevadas com 'val'
corrs[(corrs[,'dur'] >= 0.5 | corrs[,'dur'] <= -0.5), 'dur'] # correlações médias e elevadas com 'dur'
corrs[(corrs[,'acous'] >= 0.5 | corrs[,'acous'] <= -0.5), 'acous'] # correlações médias e elevadas com 'acous'
corrs[(corrs[,'spch'] >= 0.5 | corrs[,'spch'] <= -0.5), 'spch'] # correlações médias e elevadas com 'spch'
corrs[(corrs[,'pop'] >= 0.5 | corrs[,'pop'] <= -0.5), 'pop'] # correlações médias e elevadas com 'pop'

# as correlações mais fortes que temos são entre os pares: 
#   nrgy, acous (-0.5765065) 
#   nrgy, dB (0.6636233)
# todos os restantes pares de variáveis têm correlações inferiores a 0.5 em valor 
#   absoluto
# isto poderá constituir um desafio na redução de dimensionalidade, uma vez que 
#   para implementarmos o PCA, quantas mais as correlações fortes, maior a 
#   potencial variância explicada por cada componente criada

# plot das correlações:
par(oma = c(0, 0, 0, 0))
corrplot.mixed(corrs, order = "hclust", tl.col = 'black', tl.pos = "lt", upper = "ellipse")

# Variáveis de input são: Beats Per Minute, Energy, Danceability, Loudness, 
# Liveness, Valence, Acousticness, Speechiness, Popularity, e Duration
df_input <- df_mt_vars[, 2:length(colnames(df_mt_vars))]
head(df_input)


# Estandardização dos dados:
df_stdZ <- scale(df_input) # (média=0, desv. padrão =1)
head(df_stdZ)
head(df_input) 
corrs2 <- cor(df_stdZ) # matriz de correlações
corrplot.mixed(corrs2, order = "hclust", tl.col = 'black', tl.pos = "lt", 
               upper = "ellipse")
#############################################################################

## PARTE 1: ANÁLISE DE COMPONENTES PRINCIPAIS

# Teste de Bartlett
cortest.bartlett(corrs2)
# como o p-value é muito próximo de zero, rejeitamos hipótese nula; significa 
#   que pelo menos uma das variáveis é correlacionada com uma outra na população

# Teste de KMO
KMO(corrs2)
# os valores para este indicador são na sua generalidade baixos; somente os das 
#   variáveis dB, live, val, dur, acous são acima de 0.6 


# Analisar número de componentes como sendo igual ao número de variáveis input:
(componentes_total <- principal(df_stdZ, nfactors=length(colnames(df_stdZ)), 
                                rotate="none", scores=TRUE))

# Critério de Kaiser - análise dos loadings
round(componentes_total$values,3)
# de acordo com este critério, deveremos escolher as primeiras 4 componentes, 
#   pois têm valor próprio > 1

# Screeplot 
plot(componentes_total$values, type = "b", main = "Scree plot - Spotify",
     xlab = "Nr. Componentes Principais", ylab = "Valor Próprio")   
# segundo este critério "do cotovelo", o número de componentes principais a 
#   escolher deverão ser 2

# Analisar a variância acumulada das componentes:
componentes_total$loadings
# é possível perceber, olhando para a variância acumulada, que: 
#   entre a PC4 e PC5 existe uma diferença de ~0.1;
#   entre a PC5 e PC6 a diferença é de ~0.08;
#   entre a PC6 e PC7 a diferença é de ~0.07.
# Por este motivo, de acordo com este critério, o número ideal de componentes 
#   principais poderá ser 5


# Vamos testar com extração de 2, 4 e 5 componentes principais:

# Extração de 2 PC
(pc2 <- principal(df_stdZ, nfactors=2, rotate="none"))
# 2 PC rodadas:
pc2r <- principal(df_stdZ, nfactors=2, rotate="varimax")
pc2r$loadings
# neste cenário perdemos cerca de 60% da variância dos dados originais...
pc2r$communality
# e só uma das variáveis, nrgy tem mais de 60% da sua variância explicada 
#   através de 2 componentes...

# Extração de 4 PC
(pc4 <- principal(df_stdZ, nfactors=4, rotate="none"))
# 4 PC rodadas:
pc4r <- principal(df_stdZ, nfactors=4, rotate="varimax")
pc4r$loadings
# neste cenário perdemos perto de 40% da variância dos dados originais
pc4r$communality
# as variáveis bpm, nrgy, dnce, dB, val, spch têm mais de 60% da sua 
#   variância explicada através de 4 componentes; contudo, só nrgy 
#   está cima dos 80%
# Parece ainda pouco...

# Extração de 5 PC
(pc5 <- principal(df_stdZ, nfactors=5, rotate="none"))
# 5 PC rodadas:
pc5r <- principal(df_stdZ, nfactors=5, rotate="varimax")
pc5r$loadings
# neste cenário perdemos perto de 30% da variância dos dados originais
pc5r$communality
# as variáveis live, dur, acoust não conseguem 60% da variância explicada através
# de 5 componentes
# RC1: componente energia/eletrónica
# RC2: componente live/duração?
# RC3: componente vocal (hip hop?)
# RC4: componente de dança
# RC5: componente de popularidade

# Extração de 6 PC
(pc6 <- principal(df_stdZ, nfactors=6, rotate="none"))
# 6 PC rodadas:
pc6r <- principal(df_stdZ, nfactors=6, rotate="varimax")
pc6r$loadings
# neste cenário perdemos perto de 20% da variância dos dados originais
pc6r$communality


# Parece que 5 poderá ser um número adequado de componentes principais para o 
#   dataset que temos

# Scores para PC=5
pc5sc <- principal(df_stdZ, nfactors=5, rotate="none", scores=TRUE)  
mean(pc5sc$scores[,1])
sd(pc5sc$scores[,1])

# Acrescentar os scores ao df original
df_spotify$pc1 <- pc5sc$scores[,1]
df_spotify$pc2 <- pc5sc$scores[,2]
df_spotify$pc3 <- pc5sc$scores[,3]
df_spotify$pc4 <- pc5sc$scores[,4]
df_spotify$pc5 <- pc5sc$scores[,5]
head(df_spotify)

# Top 10 for each component
head(df_spotify[order(df_spotify$pc1, decreasing = TRUE),c(1:5)],10)
head(df_spotify[order(df_spotify$pc2, decreasing = TRUE),c(1:5)],10)
head(df_spotify[order(df_spotify$pc3, decreasing = TRUE),c(1:5)],10)
head(df_spotify[order(df_spotify$pc4, decreasing = TRUE),c(1:5)],10)
head(df_spotify[order(df_spotify$pc5, decreasing = TRUE),c(1:5)],10)

names(df_spotify)[16:20] = c("Energetic/electronic","Live/Long","Vocal","Dance","Popularity")
head(df_spotify)

summary(df_spotify[16:20])
# density for each of the components
for (name in names(df_spotify)[16:20]) {
  u <- unlist(df_spotify[name])
  d <- density(u)
  plot(d, main = paste("Density for ",name))
  xfit <- seq(min(u), max(u), length = 40) 
  yfit <- dnorm(xfit, mean=mean(u), sd=sd(u)) 
  #yfit <- yfit * diff(h$mids[1:2]) * length(g) 
  
  lines(xfit, yfit, col="darkblue", lwd=1, add=TRUE, yaxt="n", lty=2)
  quants <- quantile(u, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  print(quants)
  for (qn in names(quants)) {
    
    abline(v=quants[qn], col="darkblue", lty=2)
    text(quants[qn], y=0, qn, col="darkblue")
  }
}


#############################################################################

## PARTE 2: ANÁLISE DE CLUSTERING

#############################################################################
## Probabilistic clustering

# 1. grab just the interesting part of the dataset
data <- df_spotify[16:20]

# 2. Perform Bayesian Information Criterion analysis
BIC <- mclustBIC(data)
plot(BIC)

# 3. GMM with 6 components
results.G6 <- Mclust(data, G = 6)
summary(results.G6, parameters = TRUE)

results.G6$modelName                # Optimal selected model
results.G6$G  			                # Optimal number of cluster 
head(round(results.G6$z,2), 10)     # Probability to belong to a given cluster
head(results.G6$classification, 10) # Cluster assignment of each observation

# Add the cluster classification to the original dataset
df_spotify = df_spotify %>%
  mutate(cluster = results.G6$classification)

#Barplot of position distribution within each cluster
barplot(prop.table(table(subset(df_spotify,cluster==1)[,4])),main= "Cluster 1 vs. top.genre", las=2)
barplot(prop.table(table(subset(df_spotify,cluster==2)[,4])),main= "Cluster 2 vs. top.genre", las=2)
barplot(prop.table(table(subset(df_spotify,cluster==3)[,4])),main= "Cluster 3 vs. top.genre", las=2)
barplot(prop.table(table(subset(df_spotify,cluster==4)[,4])),main= "Cluster 4 vs. top.genre", las=2)
barplot(prop.table(table(subset(df_spotify,cluster==5)[,4])),main= "Cluster 5 vs. top.genre", las=2)
barplot(prop.table(table(subset(df_spotify,cluster==6)[,4])),main= "Cluster 6 vs. top.genre", las=2)

barplot(head(sort(prop.table(table(subset(df_spotify,cluster==1)[,3])), decreasing = TRUE), 10),main= "Cluster 1 vs. artist", las=2)
barplot(head(sort(prop.table(table(subset(df_spotify,cluster==2)[,3])), decreasing = TRUE), 10),main= "Cluster 2 vs. artist", las=2)
barplot(head(sort(prop.table(table(subset(df_spotify,cluster==3)[,3])), decreasing = TRUE), 10),main= "Cluster 3 vs. artist", las=2)
barplot(head(sort(prop.table(table(subset(df_spotify,cluster==4)[,3])), decreasing = TRUE), 10),main= "Cluster 4 vs. artist", las=2)
barplot(head(sort(prop.table(table(subset(df_spotify,cluster==5)[,3])), decreasing = TRUE), 10),main= "Cluster 5 vs. artist", las=2)
barplot(head(sort(prop.table(table(subset(df_spotify,cluster==6)[,3])), decreasing = TRUE), 10),main= "Cluster 6 vs. artist", las=2)


#crosstab
table(df_spotify[,4],df_spotify$cluster)
table(df_spotify[,3],df_spotify$cluster)

ggplot(df_spotify, aes(x=Popularity,y=`Energetic/electronic`, shape=cluster, color=cluster))+
  scale_shape_identity()+ 
  geom_point(size=4, show.legend=TRUE)+
  theme_minimal()

ggplot(df_spotify,aes(Popularity,`Energetic/electronic`,group=cluster))+
  geom_point(aes(shape=cluster, color=cluster))+
  scale_shape_identity()+ 
  theme_minimal()

# looking at features vs clusters
aggregate(df_spotify,list(df_spotify$cluster), mean)

# 3.1 GMM with 4 components
results.G4 <- Mclust(data, G = 4)
summary(results.G4, parameters = TRUE)

results.G4$modelName                # Optimal selected model
results.G4$G  			                # Optimal number of cluster 
head(round(results.G4$z,2), 10)     # Probability to belong to a given cluster
head(results.G4$classification, 10) # Cluster assignment of each observation

# Add the cluster classification to the original dataset
df_spotify = df_spotify %>%
  mutate(cluster = results.G4$classification)

#Barplot of position distribution within each cluster
barplot(prop.table(table(subset(df_spotify,cluster==1)[,4])),main= "Cluster 1 vs. top.genre", las=2)
barplot(prop.table(table(subset(df_spotify,cluster==2)[,4])),main= "Cluster 2 vs. top.genre", las=2)
barplot(prop.table(table(subset(df_spotify,cluster==3)[,4])),main= "Cluster 3 vs. top.genre", las=2)
barplot(prop.table(table(subset(df_spotify,cluster==4)[,4])),main= "Cluster 4 vs. top.genre", las=2)

barplot(head(sort(prop.table(table(subset(df_spotify,cluster==1)[,3])), decreasing = TRUE), 10),main= "Cluster 1 vs. artist", las=2)
barplot(head(sort(prop.table(table(subset(df_spotify,cluster==2)[,3])), decreasing = TRUE), 10),main= "Cluster 2 vs. artist", las=2)
barplot(head(sort(prop.table(table(subset(df_spotify,cluster==3)[,3])), decreasing = TRUE), 10),main= "Cluster 3 vs. artist", las=2)
barplot(head(sort(prop.table(table(subset(df_spotify,cluster==4)[,3])), decreasing = TRUE), 10),main= "Cluster 4 vs. artist", las=2)

boxplot(subset(df_spotify,cluster==1)[16:20], main="Cluster 1 vs Principal Components", col = terrain.colors(4)[1])
boxplot(subset(df_spotify,cluster==2)[16:20], main="Cluster 2 vs Principal Components", col = terrain.colors(4)[2])
boxplot(subset(df_spotify,cluster==3)[16:20], main="Cluster 3 vs Principal Components", col = terrain.colors(4)[3])
boxplot(subset(df_spotify,cluster==4)[16:20], main="Cluster 4 vs Principal Components", col = terrain.colors(4)[4])

#crosstab
table(df_spotify[,4],df_spotify$cluster)

# 3.2 GMM with 2 components
results.G2 <- Mclust(data, G = 2)
summary(results.G2, parameters = TRUE)

results.G2$modelName                # Optimal selected model
results.G2$G  			                # Optimal number of cluster 
head(round(results.G2$z,2), 10)     # Probability to belong to a given cluster
head(results.G2$classification, 10) # Cluster assignment of each observation

# Add the cluster classification to the original dataset
df_spotify = df_spotify %>%
  mutate(cluster = results.G2$classification)

#Barplot of position distribution within each cluster
barplot(prop.table(table(subset(df_spotify,cluster==1)[,4])),main= "Cluster 1 vs. top.genre", las=2)
barplot(prop.table(table(subset(df_spotify,cluster==2)[,4])),main= "Cluster 2 vs. top.genre", las=2)

#crosstab
table(df_spotify[,4],df_spotify$cluster)


##############################################################################
## Let's try hierarchical clustering

# head(df_spotify)
# d_spotify components are already scaled
spotdist <- dist(df_spotify[16:20]) # compute distance
#hclust_spot <- hclust(spotdist,method='ward.D2')
hclust_spot <- hclust(spotdist, method="ward.D2")
plot(hclust_spot,label=df_spotify$top.genre,hang=-1)

# Cut the dendrogram K = 4
groups.k4 <- cutree(hclust_spot, k=4) # cut tree into 4 clusters
rect.hclust(hclust_spot, k=4, border="red") 
round(aggregate(df_spotify,list(groups.k4), mean),2)
summary(df_spotify[16:20])

df_spotify = df_spotify %>%
  mutate(cluster = groups.k4)

boxplot(subset(df_spotify,cluster==1)[16:20], main="Cluster 1 vs Principal Components", col = terrain.colors(4)[1])
boxplot(subset(df_spotify,cluster==2)[16:20], main="Cluster 2 vs Principal Components", col = terrain.colors(4)[2])
boxplot(subset(df_spotify,cluster==3)[16:20], main="Cluster 3 vs Principal Components", col = terrain.colors(4)[3])
boxplot(subset(df_spotify,cluster==4)[16:20], main="Cluster 4 vs Principal Components", col = terrain.colors(4)[4])


#Silhouette
plot(silhouette(groups.k4,spotdist)) 

# Cut the dendrogram K = 5
groups.k5 <- cutree(hclust_spot, k=5) # cut tree into 5 clusters
rect.hclust(hclust_spot, k=5, border="red") 
round(aggregate(df_spotify,list(groups.k5), mean),2)

#Silhouette
plot(silhouette(groups.k5,spotdist)) 

# Cut the dendrogram K = 2
groups.k2 <- cutree(hclust_spot, k=2) # cut tree into 2 clusters
rect.hclust(hclust_spot, k=2, border="red") 
aggregate(df_spotify,list(groups.k2), mean)

#Silhouette
plot(silhouette(groups.k2,spotdist)) 

# Cut the dendrogram K = 3
groups.k3 <- cutree(hclust_spot, k=3) # cut tree into 3 clusters
rect.hclust(hclust_spot, k=3, border="red") 
aggregate(df_spotify,list(groups.k3), mean)

#Silhouette
plot(silhouette(groups.k3,spotdist)) 

# Cut the dendrogram K = 10
groups.k10 <- cutree(hclust_spot, k=10) # cut tree into 5 clusters
rect.hclust(hclust_spot, k=10, border="red") 
aggregate(df_spotify,list(groups.k10), mean)

#Silhouette
plot(silhouette(groups.k10,spotdist)) 


### K-means
kmeans.k4 <- kmeans(df_spotify[16:20], 4,nstart=100) 
# k = 4 from hclust, nstart = initial random solutions
kmeans.k4$centers
kmeans.k4$cluster
kmeans.k4$size

#Silhouette
plot(silhouette(kmeans.k4$cluster,spotdist)) 

# K-Means: number of clusters
fviz_nbclust(df_spotify[16:20], kmeans, method ="silhouette", k.max=20)+theme_minimal()
fviz_nbclust(df_spotify[16:20], kmeans, method ="wss", k.max=20)+theme_minimal()

# wssplot <- function(xx, nc=15, seed=1234){
#   wss <- (nrow(xx)-1)*sum(apply(xx,2,var))
#   for (i in 2:nc){
#     set.seed(seed)
#     wss[i] <- sum(kmeans(xx, centers=i)$withinss)}
#   plot(1:nc, wss, type="b", xlab="Number of Clusters",
#        ylab="Within groups sum of squares")}
# 
# wssplot(df_spotify[16:20], nc=10) 

# K=6
kmeans.k6 <- kmeans(df_spotify[16:20], 6,nstart=100) 
kmeans.k6$centers
kmeans.k6$cluster
kmeans.k6$size

#Silhouette
plot(silhouette(kmeans.k6$cluster,spotdist)) 


## list of cluster assignments
o <- order(kmeans.k6$cluster)
table(df_spotify[o,4],kmeans.k6$cluster[o])
# data.frame(df_spotify$top.genre[o],kmeans.k6$cluster[o])
fviz_cluster(kmeans.k6, data = df_spotify[16:20],
             geom = "point",
             ellipse = TRUE,
             ellipse.type = "confidence",
             show.clust.cent = TRUE,
             main=paste("K-means K=",nrow(kmeans.k6$centers)),
             ggtheme = theme_minimal()
)

# K=3
kmeans.k3 <- kmeans(df_spotify[16:20], 3, nstart=100) 
kmeans.k3$centers
kmeans.k3$cluster
kmeans.k3$size


## list of cluster assignments
o <- order(kmeans.k3$cluster)
table(df_spotify[o,4],kmeans.k3$cluster[o])
# data.frame(df_spotify$top.genre[o],kmeans.k6$cluster[o])

fviz_cluster(kmeans.k3, data = df_spotify[16:20],
             geom = "point",
             ellipse = TRUE,
             ellipse.type = "confidence",
             show.clust.cent = TRUE,
             main=paste("K-means K=",nrow(kmeans.k3$centers)),
             ggtheme = theme_minimal()
)

#Silhouette
plot(silhouette(kmeans.k3$cluster,spotdist)) 

################################################################################
## PAM clustering
fviz_nbclust(df_spotify[16:20], pam, method ="silhouette")+theme_minimal()
fviz_nbclust(df_spotify[16:20], pam, method ="wss", k.max=20)+theme_minimal()

pam.k6 <- pam(df_spotify[16:20],6)
## list of cluster assignments
o <- order(pam.k6$cluster)
table(df_spotify[o,4],pam.k6$cluster[o])

clusplot(pam.k6, labels = 6, col.p = pam.k6$clustering)
fviz_cluster(pam.k6, data = df_spotify[16:20],
             geom = "point",
             ellipse = TRUE,
             ellipse.type = "confidence",
             show.clust.cent = TRUE,
             main=paste("PAM (Partitioning Around Medoids) K=",nrow(kmeans.k3$centers)),
             ggtheme = theme_minimal()
)

# centros dos medoides
pam.k6$medoids

# Silhouette média
pam.k6$silinfo$avg.width
# Silhouette média por cada cluster
pam.k6$silinfo$clus.avg.widths

