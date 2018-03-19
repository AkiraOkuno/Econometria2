library(zoo)
library(forecast)
library(ggplot2)
library(ggfortify)
library(mFilter)
library(fpp)
library(xts)

#carregar serie de PIB
data = read.csv2("PIB.csv", header = T)
PIB = ts(data$PIB, start = c(1996,1), frequency = 4)
rm(data)


#roda o HP
filtrohp <- function(y, lambda){
  id <- diag(length(y))
  d <- diff(id, d=2)
  tendhp <- solve(id+lambda*crossprod(d),y)
  as.ts(tendhp)
}

#filtro HP
PIB1 = filtrohp(PIB, 1600)
PIB1 = ts(PIB1, start=c(1996,1),frequency=4)
autoplot(PIB) + autolayer(PIB1)

