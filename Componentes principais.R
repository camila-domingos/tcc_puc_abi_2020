
# --- Leitura da base de dados  --- #

dados = read.csv2("CP.csv",header = TRUE)
dados

# --- An?lise descritiva e explorat?ria --- #

resumo <- apply(dados,2,summary)
resumo
sddados<- apply(dados,2,sd)
sddados

rbind(round(Xbarra,3), round(sddados,3))

round(cov(dados),3)

round(cor(dados),3)

pairs(dados) 

boxplot(dados)

par(mfrow=c(2,5))
for(i in 21:23)
boxplot(dados[,i], main=names(dados)[i])
par(mfrow=c(1,1))



# --- Exerc?cio: Os dados tem distribui??o normal? --- #

# --- Isso interfere na an?lise de componentes principais? --- #

#Não interfere
library(mvShapiroTest)

mvShapiro.Test(as.matrix(dados[,-8]))
 
# --- An?lise de Componentes Principais --- #

prcomp(dados)

# A an?lise de componentes est? sendo feito para S ou R??????

eigen(cov(dados))

summary(prcomp(dados))
#tudo com  esta sendo feito com S

# --- An?lise da primeira componentes principal --- #
# --- Quais os maiores pesos? --- #
# --- Que nome voc? daria ? primeira componente principal? --- #

prcomp(dados)[[2]][,1]
# Desempenho em Corrida.
eigen(cov(dados))[[1]]

# --- Scree plot --- #
eigen(cov(dados))[[1]] # autovalores lambda  
plot(eigen(cov(dados))[[1]], type="b", pch=16, main="Scree plot das Componentes Principais", ylab="autovalor", xlab="ordem da componente")

# --- Explica??o da vari?ncia --- #
# proporção da variancia explicada por cada cp.  
round(eigen(cov(dados))[[1]] / sum(eigen(cov(dados))[[1]]), 3)

plot(prcomp(dados[,-8]))


#---- correla??o das vari?veis com 
heptatlon_pca = prcomp(dados[,-8],scale=T)
print(heptatlon_pca)
summary(heptatlon_pca)
names(heptatlon_pca) 
heptatlon_pca$sdev #
heptatlon_pca$rotation #como a rotação dos eixos
heptatlon_pca$center # Media
heptatlon_pca$scale # Desvio Padrao da variavel originais
heptatlon_pca$x # O valor das componentes principais de cada indivi­duo.


eigen(cor(dados[,-8])) #iguais

#---- correla??o das vari?veis com 

l.chapeu=eigen(cor(dados[,-8]))$value[1]
e.chapeu=eigen(cor(dados[,-8]))$vectors[,1]

cor.chapeu=e.chapeu*sqrt(l.chapeu)
cor.chapeu

z1=(dados[,1]-matrix(rep(Xbarra[1],25),ncol=1))/sddados[1]
z1
cor(heptatlon_pca$x[,1],z1)
  
biplot(heptatlon_pca, col = c("gray", "black"))
abline(h=0)
abline(v=0)

#-- Outra maneira de fazer ACP # Nao dá preferencia

hep = princomp(dados[,-8],cor=TRUE)
print(hep)
summary(hep)
names(hep)
dados
hep$loadings
hep$sdev
hep$scale   

sd.pop=function(x){
n=length(x)
v=sqrt((1/n)*(sum(x^2)-(sum(x))^2/n))
}

apply(dados[,-8],2,sd.pop)



##################################################
#- Analise Fatorial

# ----- Leia a base AF ----- #

# --- Matriz de dados --- #

matrizdados = read.csv2("H:\\2013.2\\Analise Multivariada\\Aula 06\\AF.csv")
attach(matrizdados)
matrizdados
names(matrizdados)

n <- nrow(matrizdados)
n
p <- ncol(matrizdados)
p

# --- Analise descritiva e exploratoria --- #

Xbarra <- apply(matrizdados,2,mean)
Xbarra

S <- cov(matrizdados)
S

matrizdados<-na.omit(matrizdados[,3:18])
matrizdados

Xbarra <- apply(matrizdados,2,mean)
Xbarra

S <- cov(matrizdados)
S

par(mfrow=c(4,4))
for(i in 1:16)
boxplot(matrizdados[,i], main=names(matrizdados)[i])
par(mfrow=c(1,1))

# --- Criterio: Scree plot --- #

par(mfrow = c(1,2))
plot(eigen(cor(matrizdados))[[1]], type="b", pch=16, main="Scree plot", ylab="autovalor", xlab="ordem da componente")
plot(eigen(cov(matrizdados))[[1]], type="b", pch=16, main="Scree plot", ylab="autovalor", xlab="ordem da componente")


# --- Estimacao via componentes principais --- #
# --- Criterio de Kaiser usado para análise fatorial--- #

sum(princomp(matrizdados, cor=TRUE)[[1]]>1)

#Por que maiores do que 1?

# --- Cargas fatoriais --- #

L = cbind(sqrt(eigen(cor(matrizdados))[[1]][1]) * eigen(cor(matrizdados))[[2]][,1],
sqrt(eigen(cor(matrizdados))[[1]][2]) * eigen(cor(matrizdados))[[2]][,2],
sqrt(eigen(cor(matrizdados))[[1]][3]) * eigen(cor(matrizdados))[[2]][,3])
L

L2 = L^2
xbarra <- apply(L2,1,mean) # por linha 
xbarra

prop.explicada <- apply(L2,2,sum)/16 #por coluna 
prop.explicada

sum(prop.explicada)

# --- Rotacao varimax --- #
varimax(cbind(sqrt(eigen(cor(matrizdados))[[1]][1]) * eigen(cor(matrizdados))[[2]][,1],
sqrt(eigen(cor(matrizdados))[[1]][2]) * eigen(cor(matrizdados))[[2]][,2],
sqrt(eigen(cor(matrizdados))[[1]][3]) * eigen(cor(matrizdados))[[2]][,3]))

L
#Verificando se a matriz de rotacao e ortogonal
A<-varimax(cbind(sqrt(eigen(cor(matrizdados))[[1]][1]) * eigen(cor(matrizdados))[[2]][,1],
sqrt(eigen(cor(matrizdados))[[1]][2]) * eigen(cor(matrizdados))[[2]][,2],
sqrt(eigen(cor(matrizdados))[[1]][3]) * eigen(cor(matrizdados))[[2]][,3]))$rotmat

round(A %*% t(A),2)


# factanal so estima por maxima verossimilhanca
analisefatorial <- factanal(matrizdados, factors=3, rotation="varimax", scores="regression")
analisefatorial

print(analisefatorial, digits=2, cutoff=.3, sort=TRUE)

load <- analisefatorial$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(matrizdados),cex=.7) # add variable names

#--- Cargas fatoriais ---#
cargas <- analisefatorial$loadings[]

#--- Comunalidades  ---#
comunalidades <- diag(cargas %*% t(cargas))

#--- Variancia especifica ---#
varespecifica <- analisefatorial$uniqueness

#--- Estimativa de R  ---#
Rchapeu <- cargas %*% t(cargas) + varespecifica

#--- Matriz de residuos  ---#
residuos <- cor(matrizdados) - Rchapeu


# --- Exercicio: Os dados tem distribuicao normal? --- #
# --- Posso estimar o modelo utilizando o metodo da maxima verossimilhanca?--- #

library(mvShapiroTest)

mvShapiro.Test(as.matrix(matrizdados))

# Distancias dj

par(mfrow=c(1,1))

d<-rep(0,n)

for (i in 1: n)
  d[i] <- as.matrix(matrizdados[i,] - Xbarra) %*% solve(S) %*% as.matrix(t(matrizdados[i,] - Xbarra))

qqplot(d, qchisq(ppoints(n), p), pch=16)
abline(0,1)

# --- Envelope para as distancias - executar codigo da funcao antes --- #

source("http://wiki.icmc.usp.br/images/2/23/Envelope.distancias.txt")
envelope.distancias(d, n, Xbarra, S)

#--------------
#Funcao que trabalha com outros metodos
#--------------

install.packages("psych")
install.packages("GPArotation")
library(psych)
library(GPArotation)

vero=fa(cor(matrizdados),3,rotate="varimax",fm="ml")
comp=fa(cor(matrizdados),3,rotate="varimax",fm="pa")















