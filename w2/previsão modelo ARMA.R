install.packages("fArma")
library(fArma)

data <- read.table(file = "serie.txt", header=T)

#set seed
set.seed(10)

#número de observações
n=137

#definindo o modelo
modelo <- list(ar=c(0.5),ma=c(-0.3,0.7)) #ARMA(1,2)

#constante
c=5

#media incondicional
media = 5/(1-sum(modelo$ar))

#variável w
y = media + arima.sim(modelo,n,sd=0.5)
dim(y)
plot(y)

#FAC
acf(y)

#FACP
pacf(y)

# serie do masini
dados <- as.ts(data$value)

resultado <- arima(dados, order=c(1,0,2))

aic_resultado <- AIC(resultado)
bic_resultado <- BIC(resultado)
hq_resultado <- HQ(resultado)

#criar uma vetor que contém os criterios de informação AIC para cada par
#(i,j) de ordens AR,MA

info <- c()
for (i in 0:8){
  for (j in 0:8){
    mod <- arima(dados,  order=c(i,0,j))
    aic <-AIC(mod)
    info = c(info, aic)
  }
}

#transformar esse vetor em uma matriz cujas linhas são as ordens AR
#e as colunas são as ordens MA
#Obs: as ordens começam em 0

info_matrix <- matrix(info, nrow = 9, ncol = 9, byrow = TRUE)
info_matrix

#encontrar o menor valor do criterio AIC e encontrar os respectivos indices

m <- which(info_matrix == min(info_matrix), arr.ind = TRUE)
m

ar = m[1,1]-1
ma = m[1,2]-1

ar #=2
ma #=1

#criar uma vetor que contém os criterios de informação BIC para cada par
#(i,j) de ordens AR,MA

info2 <- c()
for (i in 0:8){
  for (j in 0:8){
    mod2 <- arima(dados,  order=c(i,0,j))
    bic <-BIC(mod)
    info2 = c(info2, bic)
  }
}

#transformar esse vetor em uma matriz cujas linhas são as ordens AR
#e as colunas são as ordens MA
#Obs: as ordens começam em 0

info_matrix2 <- matrix(info2, nrow = 9, ncol = 9, byrow = TRUE)
info_matrix2

#encontrar o menor valor do criterio AIC e encontrar os respectivos indices

m2 <- which(info_matrix2 == min(info_matrix2), arr.ind = TRUE)
m2

ar2 = m[1,1]-1
ma2 = m[1,2]-1

ar2
ma2

dados

arma <- armaFit( ~ arma(2,1), dados, method = "ML")
res = residuals(arma)
jarqueberaTest(res)
