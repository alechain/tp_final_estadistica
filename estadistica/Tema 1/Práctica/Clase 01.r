##########################################################
#                   ESTAD?STICA                          #
#	            MAESTR?A EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIER?A                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTAD?STICA
## Docentes: Rodrigo Del Rosso

################################
####### SETEO DE CARPETA #######
################################

getwd()

dir()

setwd("....")

getwd() ## verifico si me modific? la ruta

##########################
####### SUCESIONES #######
##########################

1:5              

-100:0

# La funci?n "seq" produce secuencias equi-espaciadas, con paso que se puede determinar. Por default usa paso 1.

seq(1,5)

seq(1,5,0.5)

# Con la funci?n rep replica un valor dado la cantidad de veces que se le indique: rep(valor, longitud)

rep(NA,5)

?seq

seq(5,1)

seq(to = 5, from = 1)

##########################
####### ASIGNACI?N #######
##########################

## Operador de asignaci?n "<-": nos permite asignar un nombre/valor/formato a un objeto

x <- 1

x
print(x)

x = 1
1 = x

print(x)

assign("x", 1)

print(x)

## Se distinguen may?sculas de min?sculas. Por ejemplo, si ahora le pedimos que nos muestre X

X

ls()
objects()
rm("x")

## R es CASE SENSITIVE 

prompt

########################
####### VECTORES #######
########################

x <- seq(1,5)
y <- seq(1,10,2)

x * y    # Multiplica componente a componente

x %*% y   # Realiza el producto escalar entre vectores.

# Ejemplo de vector l?gico

x <- c(T,F,F,T) # un vector l?gico contiene True o False en cada componente.
x

# Ejemplo de caracteres 

x <- "Hoy es Viernes" # vector de caracteres de longitud 1
y <- c("Martin","Mauro","Rodrigo") # vector de caracteres de longitud 3

# Algunas funciones sobre vectores

length(y)    # devuelve la dimension del vector

rev(y)		# invierte el orden de los elementos del vector

y = c(6,9,2,7,1)
sort(y)        # ordena  los elementos en orden ascendente

########################
####### MATRICES #######
########################

# A partir de un vector dado generar una Matriz # 

x <- 1:12

A <- matrix(x,nrow=3,ncol=4)

B <- matrix(x,nrow=3,byrow=T)

# Con las funciones cbind rbind #

col1 <- c(1,2,3)
col2 <- c(0,1,1)
A <- cbind(col1,col2)

B <- rbind(col1,col2)

# Operaciones algebraicas.

# Si se quiere multiplicar en forma matricial se debe usar %*%:

A <- matrix(1:12,ncol=4)

B <- matrix(seq(1,24,3),ncol=2)

A %*% B

M <- matrix(c(2,3,1,2,4,5,2,4,3),ncol=3)
t(M)     

det(M)

solve(M)

# Para obtener elementos de una matriz #

A

A[1,3]  # devuelve el elemento de la fila 1 y columna 3

A[1,]   # devuelve la primera fila de la matriz

A[,4]   # devuelve la cuarta columna de la matriz.

A[7]    # devuelve el septimo elemento de la matriz recorri?ndola por columnas.

# Operaciones por filas o columnas #

A <- matrix(1:20,ncol=5)
A

apply(A,2,sum)  # Sumamos las columnas de la matriz A

apply(A,1,sum)  # Sumamos las filas de la matriz A

apply(A,1,sd)

# Nombres de las filas y columnas de una matriz #

notas <- c(10,9,10,9,8,10)
notas <- matrix(notas,3,2,byrow=T)
nombres <- c("Rodrigo","Martin","Mauro")
rownames(notas) <- nombres
colnames(notas) <- c("Parcial", "Final")
notas

dimnames(notas)

notas["Rodrigo",]

notas[,"Final"]

data.class(notas)   ## preguntamos que tipo de objeto es "notas"

############################
####### DATAS FRAMES #######
############################

# Ejemplos de construcci?n de un data frame #

#Ej 1 

x1 <- c("Guido","Micaela","Vanesa")
x2 <- c(10,9,8)
x3 <- c(9,9,7)
x4 <- c(TRUE,FALSE,TRUE)
Notas <- data.frame(x1,x2,x3,x4)
colnames(Notas) <- c("Nombre","Primer_Parcial","Segundo_Parcial","Tutor?a")
Notas

str(Notas)
head(Notas, n = 2)
tail(Notas)

# Acceso a los elementos de un data frame #

Notas[,1] 

Notas$Primer_Parcial

Notas$Nombre

Notas$Primer_Parcial


Notas %>% summarise(Primer_parcial_media=mean(Primer_Parcial), Segundo_parcial_media=mean(Segundo_Parcial),)
Notas

apply(Notas[,2:3],2, mean)

apply(Notas[,2:4],2,mean)

######################
####### PLOTEO #######
######################

#Leemos el archivo

Obras <- read.csv2("Obras.csv")
Obras
plot(Obras$Materiales)
?plot
plot(Obras$Materiales,
     col = "blue",
     pch = 20) # Cambiamos el formato de los puntos

ggplot(mapping=aes(x='Materiales', data=Obras)) %>% 

# Gr?fico de dispersi?n

x <- Obras$Materiales
y <- Obras$Mano.de.obra   ######ojo aca que el script decia "obra con O mayuscula"
plot(x,y) 

# Editamos el gr?fico

plot(x,
     y,
     col = "green",
     pch = 19,
     xlab = "Materiales",
     ylab = "Mano de Obra",
     xlim = c(360,420),
     ylim = c(350,500))

lines(x,y) # Si queremos unir con lineas, sin cerrar el gr?fico usamos lines:

# Con lty podemos elegir el estilo de la l?nea, y con col el color de la misma

plot(x,
     y,
     col="green",
     pch=19,
     xlab="Materiales",
     ylab="Mano de Obra",
     xlim=c(360,420),
     ylim=c(350,500))

lines(x,y,
      lty=2,
      col="green")

title("Materiales vs Mano de Obra") # Ponemos un t?tulo al gr?fico

#######################################
####### ESTAD?STICA DESCRIPTIVA #######
#######################################

DatosEPH <- read.csv2("DatosEPH2016.csv", 
                      header = T)     # Leemos el archivo de datos

Aglomerado.frec <- table(DatosEPH$AGLOMERADO)
pie(Aglomerado.frec) 

# Gr?fico editado

pie(Aglomerado.frec,
    labels = c("CABA","GBA"),
    col = c("red","blue"),
    main = "Gr?fico circular para la variable Aglomerado")

# Para graficar un gr?fico de barras

barplot(Aglomerado.frec)

# Gr?fico editado

barplot(Aglomerado.frec,
        col = c("red","blue"),
        main = "Gr?fico de barras para la variable Aglomerado")

barplot(Aglomerado.frec,
        col = c("red","blue"),
        main = "Gr?fico de barras para la variable Aglomerado",
        axis.lty = 1)

## MEDIDAS QUE RESUMEN INFORMACI?N ##

Ingresos <- DatosEPH$ITF

mean(Ingresos, na.rm = T)

# Si hubiera valores faltantes dar?a NA, una forma de calcular la media descartando los valores faltantes es con na.rm=T

median(Ingresos,na.rm = T)

min(Ingresos,na.rm = T)

max(Ingresos,na.rm = T)

quantile(Ingresos,na.rm = T)

# lo mismo calcula fivenum
fivenum(Ingresos,na.rm = T)

# si queremos otros percentiles

quantile(Ingresos,na.rm = T, probs = c(0.10,0.30,0.90,0.95))

summary(Ingresos)

range(Ingresos)

diff(range(Ingresos))

#Otra forma

max(Ingresos)-min(Ingresos)

var(Ingresos)

sd(Ingresos)  #desvio estandar

IQR(Ingresos) #rango intercuartil

mad(Ingresos) #desvio absoluto medio

100*sd(Ingresos)/mean(Ingresos)

#Si queremos solo dos cifras decimales

round(100*sd(Ingresos)/mean(Ingresos),2)

# Gr?ficos

hist(Ingresos)

histo.ingresos <- hist(Ingresos)
names(histo.ingresos)
histo.ingresos$breaks
histo.ingresos$mids
histo.ingresos$counts 

Ingresos
IngresosCABA <- DatosEPH$ITF[DatosEPH$AGLOMERADO == "CABA"]
hist(IngresosCABA,breaks=seq(0,48000,4000),col="blue",ylim=c(0,60), main="Histograma de Ingresos CABA")
hist(IngresosGBA,breaks=seq(0,48000,4000),col="green",ylim=c(0,60), main="Histograma de Ingresos GBA")
IngresosGBA <- DatosEPH$ITF[DatosEPH$AGLOMERADO == "GBA"]

# Para poder comparar ambos histogramas trabajamos para ambas variables con los mismos intervalos de clase # y la misma escala para las frecuencias.

par(mfrow = c(1,2))# es para dividir la pantalla gr?fica en 1 fila y dos columnas para poder visualizar mejor.

# Longitud de cada intervalo 4000 (elegimos esta longitud de acuerdo a los datos observados)

hist(IngresosCABA,breaks=seq(0,48000,4000),freq=F,col="blue",ylim=c(0,0.0001), main="Histograma de Ingresos CABA")
hist(IngresosGBA,breaks=seq(0,48000,4000),labels=T,freq=F,col="green",ylim=c(0,0.0001), main="Histograma de Ingresos GBA")

#Boxplot
par(mfrow=c(1,1))
boxplot(Ingresos,col="green",ylab="Ingresos")

#Horizontal

x11()   ### para que no me pise los gr?ficos y tenerlos abiertos

boxplot(Ingresos,col = "green",xlab = "Ingresos",horizontal=T)

# Vamos a generar otra variable, Cant = n?mero de integrantes del hogar

cant <- DatosEPH$ITF/DatosEPH$IPCF

DatosEPH2 <- cbind(DatosEPH,cant)

data.class(DatosEPH2)

# Guardamos la nueva base

write.csv2(DatosEPH2,"DatosEPH2016_v2.csv")

## En ingl?s es sin "2" 
## En espa?ol es con "2"

###############################
####### OBJETOS CREADOS #######
###############################

ls()   # f?jese cu?les son los objetos que ha creado.     

objects() ## otra forma de listar todos los objetos creados

rm("nombre objeto") ## para eliminar un objeto creado

rm("nombre objeto 1","nombre objeto 2")

rm(list = ls(all = TRUE)) ## elimina todos los objetos creados
