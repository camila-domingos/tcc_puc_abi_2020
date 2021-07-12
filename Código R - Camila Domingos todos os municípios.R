##############################################
# Código Case Data Science Camila Domingos   # 
##############################################

# ----- Leitura da base de dados ----- #

dados=read.csv2("TO.csv")
attach(dados)
head(dados)




# --- Análise descritiva e exploratoria --- #

descritiva = apply(dados[3:10],2,summary)
descritiva

cov(dados)

# --- Análise de Classificação Cluster --- #

require(graphics)
require(utils)

rownames(dados) = dados[,2]
dados = dados[,4:11]
head(dados)
#Calcular a distancia entre as unidades experimenttais
dist(dados)

#Metodos hierarquicos
?hclust

#Argumentos
#d - uma matriz de distancia
# method - o metodo utilizado (compelta, simples, ward...)

dendograma=hclust(dist(dados), "complete") #metodo do vizinho mais proximo
plot(dendograma, main = "Dendograma de Tocantins")
rect.hclust(dend1, k = 3, border = "red")
dend1.1=hclust(dist(dados,method = "manhattan"),"complete")
plot(dend1.1)

par(mfrow=c(1,2))
plot(dend1)
plot(dend1.1)
par(mfrow=c(1,1))

dend2=as.dendrogram(dend1)
str(dend2)

par(mfrow =  c(2,2))
plot(dend2)
## "triangle" type and show inner nodes:
plot(dend2, nodePar = list(pch = c(1,NA), cex = 0.8, lab.cex = 0.8),
      type = "t", center = TRUE)
plot(dend2, edgePar = list(col = 1:2, lty = 2:3),
     dLeaf = .1, edge.root = TRUE)
plot(dend2, nodePar = list(pch = 2:1, cex = .4*2:1, col = 2:3),
     horiz = TRUE)
par(mfrow=c(1,1))


#Para visualizar a qual grupo cada unidade experimental pertence
cutree(dend1,k=3)


#Metodos nao hierarquicos
kmeans(dados, 4) # cluster nÃ£o hierarquico de 4 grupos 

cl <- kmeans(dados, 4)
plot(dados, col = cl$cluster)

sort(cl$cluster)

dad=cbind(dados$Educacao,dados$PIB)
cl2=kmeans(dad,3)
plot(dad,col=cl2$cluster)
points(cl2$centers, col = 1:3, pch = 8)







