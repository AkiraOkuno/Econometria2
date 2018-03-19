library(urca)
library(vars)

library(tseries)

#carregar series do CONSUMO e do PIB

consumo <- read.csv("consumo-ipea.csv", stringsAsFactors = F)
names(consumo) <- c("data", "consumo")
consumo <- ts(consumo$consumo, start = c(1991, 1), frequency = 4)
plot(consumo)

pib <- read.csv2("pib.csv", stringsAsFactors = F)
pib <- pib[-length(pib[,1]),]
pib[,2] <- gsub("\\.", "", pib[,2])
pib[,2] <- as.numeric(gsub(",", ".", pib[,2]))
pib <- ts(pib[,2], start = c(1990, 1), frequency = 12)
pib <- aggregate(pib, nfrequency = 4)
plot(pib)

df <- ts.intersect(consumo, pib)
matplot(df, type = "l")

#TESTE DE JOHANSEN
summary(ca.jo(df))

#r = rank da matriz de coeficientes
#Vemos que ao p-valor de 5%, rejeitamos a nula de nao-cointegracao (r=0)
#também rejeitamos a nula de r<=1
#a melhor estimativa para r = 2 (precisamos de uma combinacao linear)

#utilziamos como peso o autovetor do maior autovalor
s = consumo - 0.5535966*pib
plot(s)

############################################

moeda <- read.csv2("papel moeda.csv", stringsAsFactors = F)
moeda <- ts(as.numeric(gsub(",", "", moeda[,2][-454])), start = c(1980, 1), frequency = 12)
moeda <- aggregate(moeda, nfrequency = 4)

juros <- read.csv2("selic.csv", stringsAsFactors = F)
juros <- ts(as.numeric(gsub(",", "\\.", juros[,2][-377])), start = c(1986, 7), frequency = 12)
juros <- aggregate(juros, nfrequency = 4)

ipca <- read.csv("ipca nominal.csv", sep = "\t", stringsAsFactors = F)
ipca <- ts(unlist(ipca), start = c(1979, 12), frequency = 12)
ipca <- aggregate(ipca, nfrequency = 4)

#sistema singular
df <- ts.intersect(juros, pib, ipca, moeda)

summary(ca.jo(df))

#############################################

ipca <- read.csv("ipca nominal.csv", sep = "\t")
ipca <- ts(unlist(ipca), start = c(1979, 12), frequency = 12)

preco_usa <- read.csv2("preco_usa.csv", stringsAsFactors = F)
preco_usa <- ts(as.numeric(preco_usa[,2][-440]), start = c(1980, 1), frequency = 12)

cambio <- read.csv2("cambio.csv", stringsAsFactors = F)
cambio <- ts(as.numeric(cambio[,2][-779]), start = c(1953, 1), frequency = 12)
cambio <- window(cambio, start = c(1995,1), frequency = 12)

df <- ts.intersect(ipca, preco_usa, cambio)
summary(ca.jo(df))
#Vemos que ao p-valor de 5%, nao rejeitamos a nula de nao-cointegracao (r=0)

########################################################################

#engle granger methodology

#ensure all series are I(1)
#Consumo pib

adf.test(consumo)
adf.test(pib)

#run regression
pib = ts(pib, start = c(1991, 1), end = c(2014,3), frequency = 4)

reg <- lm(pib ~ consumo)

#extract residuals

resid_longrun = reg$residuals

#check if residuals are stationary

adf.test(resid_longrun)
#ok, pval<0.05  

#nao coloquei a relacao 2 pq a matriz era singular :(

#caso 3

pib = ts(pib, start = c(1995, 1), end = c(2014,3), frequency = 4)
cambio = ts(cambio, start = c(1995, 1), end = c(2014,3), frequency = 4)
preco_usa = ts(preco_usa, start = c(1991, 1), end = c(2014,3), frequency = 4)

reg2 <- lm(pib~ cambio, preco_usa)
resid <- reg$residuals
adf.test(resid)
#pval<0.05, ok!

#caso3
ipca = ts(ipca, start = c(1986, 3), end = c(2017,3), frequency = 4)
juros = ts(juros, start = c(1986, 3), end = c(2017,3), frequency = 4)
moeda = ts(moeda, start = c(1986, 3), end = c(2017,3), frequency = 4)

reg3 <- lm(juros~ipca,moeda)
resid3 <- reg3$residuals
adf.test(resid3)
#p-val=0.29
#nao ha cointegração
