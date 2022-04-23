##########################################################
#                   ESTADÍSTICA                          #
#	            MAESTRÍA EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIERÍA                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTADÍSTICA
## Docentes: Rodrigo Del Rosso

rm(list = ls())

################################
####### SETEO DE CARPETA #######
################################

getwd()

path = "...."

setwd(path)

dir()

# Basado en https://www.princeton.edu/~otorres/sessions/
# http://www.princeton.edu/~otorres/sessions/s2r.pdf

# Leemos el archivo

download.file(url = "http://dss.princeton.edu/training/students.xls",
              destfile = paste0(path,"/students.xls"),
              method = "auto",
              quiet = FALSE,
              mode = "wb",
              cacheOK = TRUE)
dir()

## Importar el dataset como csv
datos <- read.csv2("students.csv",
                   header = T)

## Explorando los datos

summary(datos) # Suministra estadística descriptiva básica

edit(datos) # Abre el editor de datos

str(datos) # Suministra la estructura de los datos

names(datos) # Lista las variables

head(datos) # Primeras 6 filas

head(datos, n = 10) # Primeras 10 filas

head(datos, n= -10) # Últimas 10 filas

tail(datos) # Últimas 6 filas

tail(datos, n = 10) # Últimas 10 filas

tail(datos, n = -10) # Últimas 10 filas

datos[1:10, ] # Primeras 10 filas

is.data.frame(datos)
is.vector(datos)

datos[1:10,"First.Name"] # Primeras 10 filas de las 3 primeras variables

# Información Faltante (Missing)

rowSums(is.na(datos)) # Cantidad de Missing por Filas

colSums(is.na(datos)) # Cantidad de Missing por Columnas

# Ejemplo de Conversión a Datos Faltantes

datos[datos$Age == 30,"Age"] <- NA

# Listar filas que tienen valores faltantes
datoscompletos = datos[complete.cases(datos),]
datosconna = datos[!complete.cases(datos),]

# Crear un nuevo dataset sin datos faltantes
datos1 <- na.omit(datos)

# Reemplazar un valor
datos1[datos1$SAT == 1787,"SAT"] <- 1800

datos1[datos1$Country == "Bulgaria","Country"] <- "US"

# Renombrar Variables

# Interactivamente
fix(datos)

# Comando básico "names"
names(datos)[3] <- "First"

# Mediante paquete reshape
install.packages("reshape")
library(reshape)

names(datos)

rename()

datos <- rename(datos, c("Last" = Last.Name))

datos <- rename(datos, c("First" = First.Name))

datos <- rename(datos, c("Status" = Student.Status))

datos <- rename(datos, c("Score" = Average.score..grade.))

datos <- rename(datos, c("Height" = Height..in.))

datos <- rename(datos, c("Read" = Newspaper.readership..times.wk.))

datos = datos[,-length(datos)]

names(datos)

datos$Last
# Last
# attach(datos)
# detach(datos)

# Crear secuencia de números id
datos$id <- seq(dim(datos)[1])

# Crear una variable con el número total de observaciones
datos$total <- dim(datos)[1]

# Recoding variables

library(car)
datos$Age.rec <- recode(datos$Age, "18:19='18to19';20:29='20to29';30:39='30to39'")
datos$Age.rec <- as.factor(datos$Age.rec)

# Estadística Descriptiva mediante paquete "pastecs"-
library(pastecs)

stat.desc(datos)

tabla = stat.desc(datos[,c("Age","SAT","Score","Height", "Read")])
write.csv2(x = tabla,file = "tabla descriptiva.csv")

stat.desc(datos[,c("Age","SAT","Score")], 
          basic=TRUE, 
          desc=TRUE, 
          norm=TRUE, 
          p=0.95)

stat.desc(datos[10:14], 
          basic = TRUE, 
          desc = TRUE, 
          norm = TRUE, 
          p = 0.95)

# Estadística Descriptiva

# mean(datos, na.rm = T)

mean(datos$SAT)

with(datos, mean(SAT))

median(datos$SAT)

var(datos$SAT)

sd(datos$SAT)

max(datos$SAT)

min(datos$SAT)

range(datos$SAT)

quantile(datos$SAT)

quantile(datos$SAT, c(.3,.6,.9))

fivenum(datos$SAT) 

length(datos$SAT)

length(datos)

datos$SAT[which.max(datos$SAT)]

datos$SAT[which.min(datos$SAT)]

# Modo por Frecuencias

table(datos$Country)

max(table(datos$Country))

names(sort(-table(datos$Country)))[1]

# Estadística Descriptiva mediante grupos usando tapply

mean <- tapply(datos$SAT,datos$Gender, mean)

sd <- tapply(datos$SAT,datos$Gender, sd)

median <- tapply(datos$SAT,datos$Gender, median)

max <- tapply(datos$SAT,datos$Gender, max)

cbind(mean, median, sd, max)

round(cbind(mean, median, sd, max),digits=1)

t1 <- round(cbind(mean, median, sd, max),digits=1)

t1

# Estadística Descriptiva por grupos mediante aggregate

aggregate(datos[c("Age","SAT")],by=list(sex=datos$Gender), mean, na.rm=TRUE)

aggregate(datos[c("Age","SAT")],datos["Gender"], mean, na.rm=TRUE)

aggregate(datos,by=list(sex=datos$Gender), mean, na.rm=TRUE)

aggregate(datos,
          by=list(sex = datos$Gender, 
                        major = datos$Major, 
                        status = datos$Status), 
                        mean,
                        na.rm=TRUE)

aggregate(datos$SAT,
          by=list(sex=datos$Gender, 
                  major=datos$Major, 
                  status=datos$Status), 
                  mean,
                  na.rm=TRUE)

aggregate(datos[c("SAT")],
          by=list(sex=datos$Gender, 
                  major=datos$Major, 
                  status=datos$Status),
                  mean, 
                  na.rm=TRUE)

# Histogramas

library(car)

head(Prestige)

hist(Prestige$income)

hist(Prestige$income, col="green")

with(Prestige, hist(income))

with(Prestige, hist(income, breaks="FD", col="green"))
box()

hist(Prestige$income, breaks="FD")

# Histogramas Condicionales

par(mfrow=c(1, 2))
hist(datos$SAT[datos$Gender=="Female"], breaks="FD", main="Female", xlab="SAT",col="green")
hist(datos$SAT[datos$Gender=="Male"], breaks="FD", main="Male", xlab="SAT", col="green")

# {} indican un comando compuesto que 
# permite varios comandos con el comando 'with'

par(mfrow=c(1, 1))

with(Prestige, {
  hist(income, breaks="FD", freq=FALSE, col="green")
  lines(density(income), lwd = 2)
  lines(density(income, adjust = 0.5),lwd = 1)
  rug(income)
})

# Histogramas Superpuesto

hist(datos$SAT, 
     breaks="FD", 
     col="green")

hist(datos$SAT[datos$Gender=="Male"], 
     breaks="FD", 
     col="gray", 
     add=TRUE)

legend("topright", c("Female","Male"), fill=c("green","gray"))

# Chequear
satgender <- table(datos$SAT,datos$Gender)
satgender

###########################################
## Simulaciones con Números "Aleatorios" ##
###########################################

## Función Sample ##

1:6

set.seed(123)
sample(1:6, 1)

sample(1:6, 10, replace = T)

# Roll One Dice = Rodar un Dado
Roll1Dice <- function(n){
  return(sample(1:6, n, rep = T))
}

set.seed(100000)
Roll1Dice(10)

# Flip One Coin = Lanzar una Moneda
# Head = Cara
# Tail = Cruz

n = 10
sample(c("Heads", "Tails"), n, rep = T)

Flip1Coin <- function(n){
  return(sample(c("Heads", "Tails"), n, rep = T))
}

n = 100
Flip1Coin(n)

C = Flip1Coin(100000)

sum(C == "Heads")
sum(C == "Tails")
sum(C != "Heads")

table(C)

prop.table(table(C))

# Ejemplo de Ciclo For

for(i in 1:100){
  print(i)
}

# Experimento de Tirar 2 Dados
roll1 = NULL
roll2 = NULL
for(i in 1:100){
  roll1[i] = Roll1Dice(1)
  roll2[i] = Roll1Dice(1)
}

sum(roll1 == roll2)  ## Suma la cantidad de veces que los valores son iguales

## Histograma de la Suma de la tirada de dos Dados
hist((roll1 + roll2), density = 100, breaks = 1:12, prob = T)

## Barplot de la Suma de la tirada de dos Dados
barplot(table(roll1 + roll2), main = "2 Dice Sum, 100 Rolls")

## Igual experimento pero se repite 10000 veces 
roll1 = NULL
roll2 = NULL
for (i in 1:10000) {
  roll1[i] = Roll1Dice(1)
  roll2[i] = Roll1Dice(1)
}

## Barplot para las 10000 tiradas
barplot(table(roll1 + roll2), density = 100, main = "2 Dice Sum, 10000 Rolls")

## Otro ejemplo
random.numbers = sample(x = 1:10, size = 5, replace = TRUE)
sum(random.numbers)

## Tirada de un dado con reposición
sample(1:6, size = 1, replace = TRUE)

## Tirada de dos dados en forma individual con reposición
sample(1:6, size = 1, replace = TRUE) + sample(1:6, size = 1, replace = TRUE)

## Tirada de dos dados en forma conjunta
Dice.roll <- sample(1:6, size = 2, replace = TRUE)
Dice.roll

sum(Dice.roll)

# Otra forma
sum(sample(1:6, size = 2, replace = TRUE))

# Crear otra función = Tirada de dos dados
two.Dice <- function(){
  Dice <- sample(1:6, size = 2, replace = TRUE)
  return(sum(Dice))
}

two.Dice()

# Lo replicó 20 veces
replicate(n = 20000, expr = two.Dice())

## Función simil para tamaño "n.Dice"
Dice.sum <- function(n.Dice){
  Dice <- sample(1:6, size = n.Dice, replace = TRUE)
  return(sum(Dice))
}

# lo replicamos 50 veces con 3 tiradas de un dado
replicate(50, Dice.sum(3))

# ahora generalizamos la cantidad de lados del dado a "n.sides"
# y la cantidad de tiradas del lado "n.Dice"
my.Dice.sum <- function(n.Dice, n.sides){
  Dice <- sample(1:n.sides, size = n.Dice, replace = TRUE)
  return(sum(Dice))
}

# lo replicamos 100 veces
replicate(100, my.Dice.sum(5,4))


## Aproximación al concepto de Probabilidad

sims <- replicate(100, two.Dice())
table(sims)
table(sims)/length(sims)

plot(table(sims), 
     xlab = 'Sum', 
     ylab = 'Frequency', 
     main = '100 Rolls of 2 Fair Dice')


plot(table(sims)/length(sims), 
     xlab = 'Sum', 
     ylab = 'Relative Frequency', 
     main = '100 Rolls of 2 Fair Dice')

# 1000 Simulaciones
more.sims <- replicate(1000, two.Dice())
table(more.sims)/length(more.sims)

plot(table(more.sims)/length(more.sims), 
     xlab = 'Sum', ylab = 'Relative Frequency', main = '1000 Rolls of 2 Fair Dice')

# 100000 Simulaciones
even.more.sims <- replicate(100000, two.Dice())
table(even.more.sims)/length(even.more.sims)

plot(table(even.more.sims)/length(even.more.sims), 
     xlab = 'Sum', 
     ylab = 'Relative Frequency', 
     main = '100000 Rolls of 2 Fair Dice')

# Este problema fue inicialmente planteado por el famoso jugador del siglo XVII 
# Antoine Gombaud, más conocido como el Caballero de Méré. 
# Fermat y Pascal discutieron su solución en su correspondencia

# Que es más probable: 
# (A) obtener al menos un 6 al lanzar un solo dado 
#  de seis lados justo 4 veces o 

# (B) obtener al menos un par de seis cuando se lanzan dos dados 
# de seis lados justos 24 veces.

experimentA <- function(){
  rolls <- sample(1:6, size = 4, replace = TRUE)
  condition <- sum(rolls == 6) > 0
  return(condition)
}

experimentB <- function(){
  first.Dice <- sample(1:6, size = 24, replace = TRUE)
  second.Dice <- sample(1:6, size = 24, replace = TRUE)
  condition <- sum((first.Dice == second.Dice) & (first.Dice == 6)) > 0
  return(condition)
}

set.seed(123)

tirada1 = NULL
tirada2 = NULL
for(i in 1:100){
  simsA <- replicate(i, experimentA())
  tirada1[i] = sum(simsA)/length(simsA)
  
  simsB <- replicate(i, experimentB())
  tirada2[i] = sum(simsB)/length(simsB)
}

plot(tirada1, type = "l", main = "Experimento A")
lines(tirada2, type = "l", col = "red")

###############################
####### OBJETOS CREADOS #######
###############################

ls()   # fíjese cuáles son los objetos que ha creado.     

objects() ## otra forma de listar todos los objetos creados

rm("nombre objeto") ## para eliminar un objeto creado

rm("nombre objeto 1","nombre objeto 2")

rm(list = ls(all = TRUE)) ## elimina todos los objetos creados
