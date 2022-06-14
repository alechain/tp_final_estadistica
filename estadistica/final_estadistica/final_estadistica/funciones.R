#Media conocida, Varianza conocida, Población infinita

infinit_norm<-function(mu=0,sigma=1,n=10,esti=10, menor_igual=TRUE) {
  
  sd<- (sigma/sqrt(n))
  z<- (esti-mu)/sd
  proba<-pnorm(z,mean=0,sd=1)
  if(menor_igual==TRUE) 
     {return(proba)} 
  else 
     {1-proba}
}

#Media conocida, Varianza conocida, Población finita

finit_norm<-function(mu=0,sigma=1,n=10,N=10,esti=10, menor_igual=TRUE) {
  
  sd<- (sigma/sqrt(n))
  correc<- sqrt(((N-n)/(N-1)))
  z<- (esti-mu)/(sd*correc)
  proba<-pnorm(z,mean=0,sd=1)
  if(menor_igual==TRUE) 
  {return(proba)} 
  else 
  {1-proba}
}

#Media conocida, Varianza descononocida, Población infinita

infinit_t<-function(mu=0,sigma=1,n=10,esti=10, menor_igual=TRUE) {
  
  sd<- (sigma/sqrt(n))
  z<- (esti-mu)/(sd)
  proba<-pt(z,mean=0,sd=1,df = n-1)
  if(menor_igual==TRUE) 
  {return(proba)} 
  else 
  {1-proba}
}

#Media conocida, Varianza descononocida, Población finita

finit_t<-function(mu=0,sigma=1,n=10,N=10,esti=10, menor_igual=TRUE) {
  
  sd<- (sigma/sqrt(n))
  correc<- sqrt(((N-n)/(N-1)))
  z<- (esti-mu)/(sd*correc)
  proba<-pt(z,mean=0,sd=1,df=n-1)
  if(menor_igual==TRUE) 
  {return(proba)} 
  else 
  {1-proba}
}


# Varianza Poblacional poblacion infinita

varian<- function(sigmacuadrado=1,n=10,scuadrado=10, menor_igual=TRUE) {
  

  ch<- ((n-1)*scuadrado)/sigmacuadrado
  proba<-pchisqt(ch,df=n-1)
  if(menor_igual==TRUE) 
  {return(proba)} 
  else 
  {1-proba}
}

###################################### IC

#Media conocida, Varianza conocida, Población infinita y finita

ic_normal<-function(sigma=1,n=10,esti=10, alpha=0.05, finita=FALSE, N=10) {
  
  sd<- (sigma/sqrt(n))
  z<- qnorm(p=(1-(alpha/2)))
  correc<- sqrt(((N-n)/(N-1)))
  ii<- esti-z*sd
  is<- esti+z*sd
  
  iif<- esti-(z*sd*correc)
  isf<- esti+(z*sd*correc)
  
  if(finita==FALSE) 
  {return(paste0('Intervalo Inferior: ',ii,' Intervalo Superior: ',is))} 
  else 
  {return(paste0('Intervalo Inferior: ',iif,' Intervalo Superior: ',isf))}
}


#Media conocida, Varianza descononocida, Población infinita y finita

ic_t<-function(sigma=1,n=10,esti=10, alpha=0.05, finita=FALSE, N=10) {
  
  sd<- (sigma/sqrt(n))
  z<- qt(p=(1-(alpha/2)),df = n-1)
  correc<- sqrt(((N-n)/(N-1)))
  ii<- esti-z*sd
  is<- esti+z*sd
  
  iif<- esti-(z*sd*correc)
  isf<- esti+(z*sd*correc)
  
  if(finita==FALSE) 
  {return(paste0('Intervalo Inferior: ',ii,'Intervalo Superior: ',is))} 
  else 
  {return(paste0('Intervalo Inferior: ',iif,' Intervalo Superior: ',isf))}
}



# Varianza Poblacional

ic_varianza<-function(n=10,varianza_muestral=10, alpha=0.05) {
  
  numerador<- (n-1)*varianza_muestral
  b<- qchisq(p=(1-(alpha/2)),df = n-1)
  a<- qchisq(p=(alpha/2),df = n-1)

  ii<- numerador/b
  is<- numerador/a
  
  
  return(paste0('Intervalo Inferior: ',ii,' Intervalo Superior: ',is)) 

}

#Muestras chicas (Tchebycheff)

ic_chicas<-function(sigma=1,n=10,esti=10, alpha=0.05, finita=FALSE, N=10) {
  
  sd<- (sigma/sqrt(n))
  k<- sqrt((1/alpha))
  correc<- sqrt(((N-n)/(N-1)))
  ii<- esti-k*sd
  is<- esti+k*sd
  
  iif<- esti-(k*sd*correc)
  isf<- esti+(k*sd*correc)
  
  if(finita==FALSE) 
  {return(paste0('Intervalo Inferior: ',ii,' Intervalo Superior: ',is))} 
  else 
  {return(paste0('Intervalo Inferior: ',iif,' Intervalo Superior: ',isf))}
}

#Para p

ic_p<-function(sigma=1,n=10,p=0.5, alpha=0.05, finita=FALSE, N=10) {
  
  prim<- ((p*(1-p))/n)
  z<- qnorm(p=(1-(alpha/2)))
  second<- ((N-n)/(N-1))
  
  tot<- sqrt(prim)
  tot_f<- sqrt(prim*second)
  
  
  ii<- p-z*tot
  is<- p+z*tot
  
  iif<- p-(z*tot_f)
  isf<- p+(z*tot_f)
  
  if(finita==FALSE) 
  {return(paste0('Intervalo Inferior: ',ii,' Intervalo Superior: ',is))} 
  else 
  {return(paste0('Intervalo Inferior: ',iif,' Intervalo Superior: ',isf))}
}




########3 Tamaño de la muestra

# Para media conocida, varianza conocida y poblacion normal
size_muestra<-function(sigma=1, alpha=0.05,N=10,n=5, ls=2,li=1, tamaño=TRUE, finito=FALSE, e_dado=10) {
  
 # e_esti<- (ls-li)/2
  e<- qnorm(p=(1-(alpha/2))) * (sigma/sqrt(n))
  
  n<- ((qnorm(p=(1-(alpha/2))) * sigma ) / e_dado)^2
  
  correc<- sqrt(((N-n)/(N-1)))
  
  e_finito<- e*correc
  
  n_finito<- ((qnorm(p=(1-(alpha/2))))^2 * (sigma)^2 * N) /( (e_dado)^2 * (N-1) + (qnorm(p=(1-(alpha/2))))^2 *(sigma)^2)

  
  if(tamaño==TRUE & finito==FALSE) 
  {return(paste0('El size n es igual a : ',n))} 
  else if (tamaño==TRUE & finito==TRUE) 
  {return(paste0('El size n es igual a : ',n_finito))}
  else if (tamaño==FALSE & finito ==FALSE) {
   {return(paste0('El error e es igual a : ',e))}
  }
  else {
    {return(paste0('El error e es igual a : ',e_finito))}
  }
}
  
# Para media conocida, varianza desconocida y poblacion normal
size_muestra_t<-function(sigma=1, alpha=0.05,N=10,n=5, ls=2,li=1, tamaño=TRUE, finito=FALSE, e_dado=10) {
  
  # e_esti<- (ls-li)/2
  e<- qt(p=(1-(alpha/2)), df = n-1) * (sigma/sqrt(n))
  
  n<- ((qt(p=(1-(alpha/2)),df = n-1) * sigma ) / e_dado)^2
  
  correc<- sqrt(((N-n)/(N-1)))
  
  e_finito<- e*correc
  
  n_finito<- ((qnorm(p=(1-(alpha/2))))^2 * (sigma)^2 * N) /( (e_dado)^2 * (N-1) + (qnorm(p=(1-(alpha/2))))^2 *(sigma)^2)
  
  
  if(tamaño==TRUE & finito==FALSE) 
  {return(paste0('El size n es igual a : ',n))} 
  else if (tamaño==TRUE & finito==TRUE) 
  {return(paste0('El size n es igual a : ',n_finito))}
  else if (tamaño==FALSE & finito ==FALSE) {
    {return(paste0('El error e es igual a : ',e))}
  }
  else {
    {return(paste0('El error e es igual a : ',e_finito))}
  }
}

############## Dif de medias

# dif de medias varianzas conocidas

z_dif_medias<-function(x1=10,x2=10, mu1_m2=0,sigma_sq_1=1,sigma_sq_2=1,n1=10,n2=10) {
  
  numerador<- (x1-x2)-mu1_m2
  denominador<- sqrt( (sigma_sq_1/n1 ) + (sigma_sq_2/n2)  )
  z<- numerador/denominador

  return(paste0('El estadistico Z es: ', z)) 

}


# Dif de medias con Varianzas DESCONOCIDAS pero iguales

t_dif_medias_var_iguales<-function(x1=10,x2=10, mu1_m2=0,sigma_sq_1=1,sigma_sq_2=1,n1=10,n2=10) {
  
  numerador<- (x1-x2)-mu1_m2
  sa<- ( (n1-1)*sigma_sq_1  + (n2-1) * sigma_sq_2   ) / (n1+n2-2)

  denominador<- sqrt( sa* ((1/n1)+(1/n2)) )
  
  t<- numerador/denominador
  
  return(paste0('El estadistico t para var desconocidas pero iguales es: ', t, ' con', (n1+n2-2), ' grados de libertad')) 
  
}

# Dif de medias con Varianzas DESCONOCIDAS pero distintas (test de welch)

t_dif_medias_var_distintas<-function(x1=10,x2=10, mu1_m2=0,sigma_sq_1=1,sigma_sq_2=1,n1=10,n2=10) {
  
  numerador<- (x1-x2)-mu1_m2
  
  v_num<- ((sigma_sq_1/n1) + (sigma_sq_2/n2))^2
  v_denom_1<- ((sigma_sq_1/n1)^2)/(n1+1)
  v_denom_2<- ((sigma_sq_2/n2)^2)/(n2+1)
  v_denom<- v_denom_1+v_denom_2
  
  v<- (v_num/v_denom) - 2
  
  denom<- sqrt((sigma_sq_1/n1)+(sigma_sq_2/n2))
  
  t<- numerador/denominador
  
  return(paste0('El estadistico t para var desconocidas pero iguales es: ', t, ' con', v, ' grados de libertad')) 
  
}



#Cocientes Varianzas 

cociente_varianzas<-function( cociente_var=1,sigma_sq_1=1,sigma_sq_2=1,n1=10,n2=10) {
  

  chi<- (sigma_sq_1/sigma_sq_2)*(cociente_var)
  f1<- n1-1
  f2<- n2-1
  
 
  
  
  return(paste0('El estadistico chi es: ', chi, ' Con ',f1,' grados de libertad ', f2, ' grados de libertad')) 
  
}

# Dif medias proporciones
dif_media_proporciones<-function(p1=10,p2=10, p1_p2=0,n1=10,n2=10) {
  
  numerador<- (p1-p2)-p1_p2
  
  p_hat<- ((n1*p1+n2*p2)/(n1+n2))
  q_hat<- (1-p_hat)
  
  denominador<- sqrt(p_hat*q_hat*((1/n1)+(1/n2)))
  
  z<- numerador/denominador
  return(paste0('El estadistico Z de dif de proporciones es: ', z)) 
  
}



############## Prueba de hipotesis

#Normal

z_normal<-function(sigma=1,n=10,esti=10, mu=10,finita=FALSE, N=12) {
  
  sd<- (sigma/sqrt(n))
  z<- (esti-mu)/sd
  correc<- sqrt(((N-n)/(N-1)))
  z_finito<- (esti-mu)/(sd*correc)
  
  
  if(finita==FALSE) 
  {return(paste0('El estadistico Z es: ', z))} 
  else 
  {return(paste0('El estadistico Z es: ', z_finito))}
}


#Media conocida, Varianza descononocida, Población infinita y finita

t_student<-function(sigma=1,n=10,esti=10, mu=10,finita=FALSE, N=12) {
  
  sd<- (sigma/sqrt(n))
  z<- (esti-mu)/sd
  correc<- sqrt(((N-n)/(N-1)))
  z_finito<- (esti-mu)/(sd*correc)
  
  
  if(finita==FALSE) 
  {return(paste0('El estadistico t es: ', z, ' Con ',(n-1),' grados de libertad'))} 
  else 
  {return(paste0('El estadistico t es: ', z_finito, ' Con ',(n-1),' grados de libertad'))}
}



# Varianza Poblacional

chi_varianza<-function(sigma_cuadrado=1,n=10,varianza_muestral=10) {
  
  numerador<- (n-1)*varianza_muestral
  denominador<- sigma_cuadrado
  chi<- numerador/denominador
  
  
  
  
  return(paste0('El estadistico chi es: ', chi, ' Con ',(n-1),' grados de libertad')) 
  
}


z_proporcion<-function(n=10,p_esti=0.10, p=10,finita=FALSE, N=12) {
  
  sd<- sqrt((p*(1-p))/n)
  
  z<- (p_esti-p)/sd
  correc<- ((N-n)/(N-1))
  
  sd_correc<- sqrt(((p*(1-p))/n)*correc)
  
  z_finito<- (p_esti-p)/sd_correc
  
  
  if(finita==FALSE) 
  {return(paste0('El estadistico Z de propor es: ', z))} 
  else 
  {return(paste0('El estadistico Z de propor es: ', z_finito))}
}