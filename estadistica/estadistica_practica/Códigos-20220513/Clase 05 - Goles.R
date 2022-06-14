##########################################################
#                   ESTAD?STICA                          #
#                Profs. DEL ROSSO - NUSKE                #
#	            MAESTR?A EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIER?A                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTAD?STICA
## Docentes: Rodrigo Del Rosso - Ezequiel Nuske

rm(list = ls())

## SETEAR RUTA DE TRABAJO
path = "D:/OneDrive - Facultad de Cs Econ?micas - UBA/Docencia/Posgrado/Austral/Estad?stica/2022/Presencial/Pr?cticas/C?digos/Clase 5 - Intervalos de Confianza/"
setwd(path)

## CARGAR PAQUETES
suppressPackageStartupMessages({
  library(dplyr)
  library(rriskDistributions)
  library(fitdistrplus)
  library(lessR)
})

## CARGAR LOS DATOS
## https://www.kaggle.com/datasets/martj42/international-football-results-from-1872-to-2017

file.show("results.csv")

datos <- read.csv(file.choose(),header = T)
head(datos)
names(datos)

## TOMAR DATOS DE ARGENTINA y BRASIL DE LOCAL
Argentina <- filter(datos,home_team == "Argentina")
Brasil <- filter(datos, home_team == "Brazil")

## HISTOGRAMA
Histogram(x = home_score,
          data = Argentina,
          xlab = "# Goles Local", 
          ylab ="Cantidad", 
          main = "Argentina")
Argentina %>% ggplot(mapping=aes(x=home_score)) + geom_bar()

Histogram(x = away_score,
          data = Argentina,
          xlab = "# Goles Visitante", 
          ylab ="Cantidad", 
          main = "Argentina")

Histogram(x = home_score,
          data = Brasil,
          xlab = "# Goles Local", 
          ylab ="Cantidad", 
          main = "Brasil")

Histogram(x = away_score,
          data = Brasil,
          xlab = "# Goles Visitante", 
          ylab ="Cantidad", 
          main = "Brasil")

Histogram(x = home_score,data = datos)
Histogram(x = away_score,data = datos)

## ESTIMACI?N DEL PAR?METRO LAMBDA EN POISSON
#Fitriskpluss
rriskMMEdist(Argentina$home_score, "pois")
rriskMMEdist(Argentina$away_score, "pois")

## OTRA ALTERNATIVA DE AJUSTE PARAM?TRICO
fit <- fitdist(Argentina$home_score,
               distr =  "pois",
               method = "mle")
summary(fit)

x11()
plot(fit)

fit2 <- fitdist(datos$home_score,
               distr =  "pois",
               method = "mle")
summary(fit2)
x11()
plot(fit2)

fit3 <- fitdist(Brasil$home_score,
               distr =  "pois",
               method = "mle")
summary(fit3)

x11()
plot(fit3)
