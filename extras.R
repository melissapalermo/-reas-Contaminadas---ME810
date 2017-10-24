library(readxl) #biblioteca para ler o excel
library(knitr)
library(ggplot2)

data <- read_excel("C:/Users/ra093259/Desktop/BASEmudado.xlsx") #leitura dos dados
##data frame dos dados
dados<-data.frame(data)

##calculando o tamanho das observações por ano
n2011 = length(which(dados$ano==2011))
n2012 = length(which(dados$ano==2012))
n2013 = length(which(dados$ano==2013))
n2014 = length(which(dados$ano==2014))
n2015 = length(which(dados$ano==2015))
n2016 = length(which(dados$ano==2016))

## verificando quantas observações que fica na região central
central11 = length(which((dados$ano==2011) & (dados$Região.Central=="S")))
central12 = length(which((dados$ano==2012) & (dados$Região.Central=="S")))
central13 = length(which((dados$ano==2013) & (dados$Região.Central=="S")))
central14 = length(which((dados$ano==2014) & (dados$Região.Central=="S")))
central15 = length(which((dados$ano==2015) & (dados$Região.Central=="S")))
central16 = length(which((dados$ano==2016) & (dados$Região.Central=="S")))

## verificando quantas observações que não fica na região central
ncentral11 = n2011 - central11
ncentral12 = n2012 - central12
ncentral13 = n2013 - central13
ncentral14 = n2014 - central14
ncentral15 = n2015 - central15
ncentral16 = n2016 - central16

##vetores das observações
y1= c(n2011, n2012, n2013, n2014, n2015, n2016)
central=c(central11,central12,central13,central14,central15,central16)
ncentral =c(ncentral11,ncentral12,ncentral13,ncentral14,ncentral15,ncentral16)
x1=c(2011, 2012, 2013, 2014, 2015, 2016)
##plot(x1,central)

##data frame para cada um deles
dataa <-data.frame(x1,y1)
datac <-data.frame(x1,central)
datan <-data.frame(x1,ncentral)

##grafico mostrando a quantidade de regiões centrais, não central e observações por ano
ggplot(datac,aes(x=x1,y=central)) + geom_point(size=4)
tt <- ggplot(datan,aes(x=x1,y=ncentral)) + geom_point(size=2) + ylim(0,160) 
tt2 <- ggplot(dados,aes(as.factor(dados$ano))) + geom_bar()  + ylim(0,160)
grid.arrange(tt,tt2)

##calculando a proporção de região não central por quantidade ano
prop = ncentral/y1

table(as.factor(dados$classif))
table(as.factor(dados$fonte_cont))

##nova variavel, separando as classificações em 3 variaveis: contaminadas, descontaminadas e em processo de recuperação
dados$Status <- as.factor(dados$Status)

##verificando quantas observações estão contaminadas
contaminadas11 = length(which((dados$ano==2011) & (dados$Status=="1")))
contaminadas12 = length(which((dados$ano==2012) & (dados$Status=="1")))
contaminadas13 = length(which((dados$ano==2013) & (dados$Status=="1")))
contaminadas14 = length(which((dados$ano==2014) & (dados$Status=="1")))
contaminadas15 = length(which((dados$ano==2015) & (dados$Status=="1")))
contaminadas16 = length(which((dados$ano==2016) & (dados$Status=="1")))
contaminadas <- c(contaminadas11,contaminadas12,contaminadas13,contaminadas14,contaminadas15,contaminadas16)

datacontam <-data.frame(x1,contaminadas)
ggplot(datacontam,aes(x=x1,y=contaminadas)) + geom_point(size=4)

##verificando quantas observações não estão contaminadas
descontaminadas11 = length(which((dados$ano==2011) & (dados$Status=="0")))
descontaminadas12 = length(which((dados$ano==2012) & (dados$Status=="0")))
descontaminadas13 = length(which((dados$ano==2013) & (dados$Status=="0")))
descontaminadas14 = length(which((dados$ano==2014) & (dados$Status=="0")))
descontaminadas15 = length(which((dados$ano==2015) & (dados$Status=="0")))
descontaminadas16 = length(which((dados$ano==2016) & (dados$Status=="0")))
descontaminadas <- c(descontaminadas11,descontaminadas12,descontaminadas13,descontaminadas14,descontaminadas15,descontaminadas16)

datadescontam <-data.frame(x1,descontaminadas)
ggplot(datadescontam,aes(x=x1,y=descontaminadas)) + geom_point(size=4)

##verificando a proporção de contaminadas e não contaminadas em relação ao total por ano
prop2 <- contaminadas/y1
prop3 <- descontaminadas/y1

##verificando quantas observações estão em processo para descontaminação
emprocesso11 = length(which((dados$ano==2011) & (dados$Status=="2")))
emprocesso12 = length(which((dados$ano==2012) & (dados$Status=="2")))
emprocesso13 = length(which((dados$ano==2013) & (dados$Status=="2")))
emprocesso14 = length(which((dados$ano==2014) & (dados$Status=="2")))
emprocesso15 = length(which((dados$ano==2015) & (dados$Status=="2")))
emprocesso16 = length(which((dados$ano==2016) & (dados$Status=="2")))
emprocesso <- c(emprocesso11,emprocesso12,emprocesso13,emprocesso14,emprocesso15,emprocesso16)

dataproc <-data.frame(x1,emprocesso)
ggplot(dataproc,aes(x=x1,y=emprocesso)) + geom_point(size=4)

prop4 <- emprocesso/y1
