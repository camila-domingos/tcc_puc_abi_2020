setwd("C:/Users/domyl/OneDrive/Desktop/PUC - TCC/Codigos para Dashboard")

##############################
# Leitura das bases de dados #
##############################

dados_AC = read.csv2("AC.csv");attach(dados_AC)
AC = mean(ÍndicedeGini2000)
#AC = mean(RendaperCapita)
AC
dados_AL = read.csv2("AL.csv");attach(dados_AL)
AL = mean(ÍndicedeGini2000)
#AL = mean(RendaperCapita)
AL
dados_AM = read.csv2("AM.csv");attach(dados_AM)
AM = mean(ÍndicedeGini2000)
#AM = mean(RendaperCapita)
AM
dados_AP = read.csv2("AP.csv");attach(dados_AP)
AP = mean(ÍndicedeGini2000)
#AP = mean(RendaperCapita)
AP
dados_BA = read.csv2("BA.csv");attach(dados_BA)
BA = mean(ÍndicedeGini2000)
#BA = mean(RendaperCapita)
BA
dados_CE = read.csv2("CE.csv");attach(dados_CE)
CE = mean(ÍndicedeGini2000)
#CE = mean(RendaperCapita)
CE
dados_ES = read.csv2("ES.csv");attach(dados_ES)
ES = mean(ÍndicedeGini2000)
#ES = mean(RendaperCapita)
ES
dados_GO = read.csv2("GO.csv");attach(dados_GO)
GO = mean(ÍndicedeGini2000)
#GO = mean(RendaperCapita)
GO
dados_MA = read.csv2("MA.csv");attach(dados_MA)
MA = mean(ÍndicedeGini2000)
#MA = mean(RendaperCapita)
MA
dados_MG = read.csv2("MG.csv");attach(dados_MG)
MG = mean(ÍndicedeGini2000)
#MG = mean(RendaperCapita)
MG
dados_MS = read.csv2("MS.csv");attach(dados_MS)
MS = mean(ÍndicedeGini2000)
#MS = mean(RendaperCapita)
MS
dados_MT = read.csv2("MT.csv");attach(dados_MT)
MT = mean(ÍndicedeGini2000)
#MT = mean(RendaperCapita)
MT
dados_PA = read.csv2("PA.csv");attach(dados_PA)
PA = mean(ÍndicedeGini2000)
#PA = mean(RendaperCapita)
PA
dados_PB = read.csv2("PB.csv");attach(dados_PB)
PB = mean(ÍndicedeGini2000)
#PB = mean(RendaperCapita)
PB
dados_PE = read.csv2("PE.csv");attach(dados_PE)
PE = mean(ÍndicedeGini2000)
#PE = mean(RendaperCapita)
PE
dados_PI = read.csv2("PI.csv");attach(dados_PI)
PI = mean(ÍndicedeGini2000)
#PI = mean(RendaperCapita)
PI
dados_PR = read.csv2("PR.csv");attach(dados_PR)
PR = mean(ÍndicedeGini2000)
#PR = mean(RendaperCapita)
PR
dados_RJ = read.csv2("RJ.csv");attach(dados_RJ)
RJ = mean(ÍndicedeGini2000)
#RJ = mean(RendaperCapita)
RJ
dados_RN = read.csv2("RN.csv");attach(dados_RN)
RN = mean(ÍndicedeGini2000)
#RN = mean(RendaperCapita)
RN
dados_RO = read.csv2("RO.csv");attach(dados_RO)
RO = mean(ÍndicedeGini2000)
#RO = mean(RendaperCapita)
RO
dados_RR = read.csv2("RR.csv");attach(dados_RR)
RR = mean(ÍndicedeGini2000)
#RR = mean(RendaperCapita)
RR
dados_RS = read.csv2("RS.csv");attach(dados_RS)
RS = mean(ÍndicedeGini2000)
#RS = mean(RendaperCapita)
RS
dados_SC = read.csv2("SC.csv");attach(dados_SC)
SC = mean(ÍndicedeGini2000)
#SC = mean(RendaperCapita)
SC
dados_SE = read.csv2("SE.csv");attach(dados_SE)
SE = mean(ÍndicedeGini2000)
#SE = mean(RendaperCapita)
SE
dados_SP = read.csv2("SP.csv");attach(dados_SP)
SP = mean(ÍndicedeGini2000)
#SP = mean(RendaperCapita)
SP
dados_TO = read.csv2("TO.csv");attach(dados_TO)
TO = mean(ÍndicedeGini2000)
#TO = mean(RendaperCapita)
TO

vetor = c(AC,AL,AM,AP,BA,CE,ES,GO,MA,MG,MS,MT,PA,PB,PE,PI,PR,RJ,RN,RO,RR,RS,SC,SE,SP,TO)

indice_de_gini = barplot(vetor, names = c("AC","AL","AM","AP","BA","CE","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"),
        ylim = c(0,0.7), col = "black", main = "Média do Índice de Gini 2000 por Estados Brasileiros",xlab = "Estados", ylab = "Índice de Gini 2000")

rotulo=round(vetor,2)
text(indice_de_gini, 0, rotulo, cex=1, pos=3, col ="white")


#renda_per_capita = barplot(vetor, names = c("AC","AL","AM","AP","BA","CE","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"),
#                 ylim = c(0,300), col = "black", main = "Renda per Capita dos Estados Brasileiros em 2000",xlab = "Estados", ylab = "Renda per Capita em Reais")

#rotulo=round(vetor,0)
#text(renda_per_capita, 0, rotulo, cex=1, pos=3, col ="white")



completa = read.csv2("completa.csv",sep= ",");attach(completa)
head(completa)

#####################
# Gráfico 1991x2000 #
#####################

em_1991 = c(148016,974618,746402,97968,4741884,2651402,705359,1183537,1788871,1804931,7346469,789179,834657,1823779,1347027,3040105,999248,3938022,6868130,1028767,436117,87369,4798497,2156808,593934,15971622,351091)
em_2000 = c(217635,1226122,1094325,182361,5992203,3434285,1008846,1573346,2511570,2288774,9262833,1028998,1185528,2540231,1648309,3804857,1267506,4995602,8155244,1330022,609817,129556,5710432,2832455,807250,20113058,500553)

plot(em_1991,ylim = c(90000,21000000), col = "red", xaxt = "n", xlab = "Estados", ylab = "População maior de 25 anos",type = "o", pch = 17)                     
axis(1, at=1:27,labels = c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"))
lines(em_2000, type = "b", lwd = 1, col = "green", pch = 19)


legend("topleft",                                   
       legend = c("Maiores de 25 anos em 1991", "Maiores de 25 anos em 2000"),
       col = c("red", "green"),
       pch = c(17,19),bty = "n", pt.cex = 1,cex = 1)


norte = c(0.63,0.62,0.70,0.67,0.71,0.68,0.66)
mean(norte)
sul = c(0.74,0.78,0.79)
mean(sul)
sudeste = c(0.78,0.76,0.72,0.73)
mean(sudeste)
centro_oeste = c(0.74,0.74,0.73,0.84)
mean(centro_oeste)
nordeste = c(0.63,0.63,0.60,0.59,0.63,0.59,0.62,0.64,0.58)
mean(nordeste)

#library(ggplot2)
gg.gauge <- function(pos,breaks=c(0,30,70,100)) {
   get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}
gg.gauge(3.72,breaks=c(0,35,70,100))
gg.gauge(4.97,breaks=c(0,35,70,100))
gg.gauge(3.25,breaks=c(0,35,70,100))
gg.gauge(2.69,breaks=c(0,35,70,100))
gg.gauge(5.74,breaks=c(0,35,70,100))
gg.gauge(6.18,breaks=c(0,35,70,100))
gg.gauge(3.29,breaks=c(0,35,70,100))
gg.gauge(5.53,breaks=c(0,35,70,100))
gg.gauge(4.68,breaks=c(0,35,70,100))
gg.gauge(4.88,breaks=c(0,35,70,100))
gg.gauge(6.21,breaks=c(0,35,70,100))
gg.gauge(5.03,breaks=c(0,35,70,100))
gg.gauge(3.66,breaks=c(0,35,70,100))
gg.gauge(3.84,breaks=c(0,35,70,100))
gg.gauge(7.21,breaks=c(0,35,70,100))
gg.gauge(6.14,breaks=c(0,35,70,100))
gg.gauge(5.72,breaks=c(0,35,70,100))
gg.gauge(5.65,breaks=c(0,35,70,100))
gg.gauge(7.43,breaks=c(0,35,70,100))
gg.gauge(6.41,breaks=c(0,35,70,100))
gg.gauge(3.30,breaks=c(0,35,70,100))
gg.gauge(2.60,breaks=c(0,35,70,100))
gg.gauge(7.20,breaks=c(0,35,70,100))
gg.gauge(5.38,breaks=c(0,35,70,100))
gg.gauge(5.13,breaks=c(0,35,70,100))
gg.gauge(10.72,breaks=c(0,35,70,100))
gg.gauge(4.53,breaks=c(0,35,70,100))




