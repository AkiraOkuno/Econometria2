library(ggplot2)
library(ggfortify)
library(zoo)
library(grid)
library(gridExtra)

data <- read.table("cambio.txt", header = TRUE, sep = "\t", dec = ",")

#limpar os dados
#função ts cria objetos de series temporais
class(data)
cambio_raw <- ts(data$cambio, start = c(1985,1,2), frequency = 365.25)

#extrair um pedaço dos dados (só queremos a partir de 2012)
cambio_non <- window(cambio_raw, start = c(2012))
P1 <- autoplot(cambio_non)
P1 + ggtitle("Câmbio Nominal Diário") + xlab("Year") + ylab("R$/US$")

#remover os NAs
cambio_clean <- cambio_non[!is.na(cambio_non)]

#variação diária
cambio <- diff(log(cambio_clean))

#prob de subida e descida do cambio
up <- mean(cambio>=0)
down <- mean(cambio<0)
up
down
up+down

#lag plot
lag.plot(cambio, lag = 12, diag = F) 

#acf
rho <- is.numeric(acf(cambio, plot = F)$acf)
