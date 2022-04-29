# clase 1 de R
# vectores en R
rm(list = ls()) 
#  '#'  para comentarios
#  '<-' para asignacion

v1 <- c(1,2,3,4)
v1
# lo mismo pero con notacion  ":"
v1  = c(1:4)
v1

# 
v2 <- c(55:151)
v2

# notacion []
v2[c(4:7,15)]
v2

# con seq()
v3 <- seq(from=1, to=2, by=.2)
v3

# con rep()
v4 <- rep(4, times=3) 
v4

# con string
v5 <- c('argentina', 'chile', 'brasil') 
v5

# no notacion []
v5[2:3]
v5[3]

# con valores NA
v6 <- c('argentina', NA, 'brasil') 
v6

# con signo '-'
v7 <- v5
v7
v8 <- v5[-1]
v8
v9 <- v5[-2]
v9

# vectores logicos
v10 <- c(TRUE, FALSE, FALSE, TRUE)
v10
v10[1:4]

# consulto la class, attributes y el tamano
length(v10)

# para saber que tipo de objeto es
class(v1)
class(v9)
class(v10)
###
# clase de ciclos
# https://www.r-bloggers.com/2021/09/r-for-loop/
# vector of numbers
num = c(2, 3, 12, 14, 5, 19, 23, 64)

# variable to store the count of even numbers
count = 0

# for loop to count even numbers
for (i in num) {
  # check if i is even
  if (i %% 2 == 0) {
    count = count + 1
  }
}

print(count)
###  
# if else
# https://www.r-bloggers.com/2021/09/r-if-else/
  
x <- 12

# check if x is positive or negative number
if (x > 0) {
  print("x is a positive number")
} else {
  print("x is a negative number")
}  
###
# fubciones
# https://www.r-bloggers.com/2021/09/r-ifelse-function/








