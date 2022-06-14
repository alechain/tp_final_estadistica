#Ejercicio Simulacion
x = ifelse(runif(100000)>0.5,1,-1)
table(x)
x0 = cumsum(x)
x0
plot(seq_along(x0),x0,col="indianred",type = "l",lwd=1,frame.plot = F)
abline(h=0,col="gray",lty="dashed")
