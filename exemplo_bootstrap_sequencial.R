R<- 10000 # numero de replicas de Monte Carlo 
B<- 500 # numero de reamostras Bootstrap 
n<- 15 # tamanho amostral 
theta<- 5 # valor do parametro 
### inicializa¸c~ao de vetores para guardas as r´eplicas 
vtheta<- rep(NA, R) 
vep<-vtheta 
vep_boot<-vtheta 
for(i in 1:R) # loop de MC 
{ 
  x <- rexp(n,theta) # amostra 
  media <- mean(x) # media da amostra 
  vtheta[i] <- 1/media # estimador de MV de theta 
  vep[i] <-1/(sqrt(n)*media) # estimadtiva do erro-padrao do estimador de theta 
  ### Aqui come¸ca a determina¸c~ao do erro-padr~ao bootstrap 
  vtheta_boot <- rep(NA,B) # vetor para guardas r´eplicas bootstrap 
  for(j in 1:B) 
  { 
    x_boot<- sample(x,replace=T) # reamostra bootstrap 
    vtheta_boot[j]<-1/mean(x_boot) # r´eplica boostrap do estimador de theta 
    } 
  vep_boot[i]<-sd(vtheta_boot) # erro-padr~ao bootstrap 
  ### Aqui termina a determina¸c~ao do erro-padr~ao bootstrap 
  } 
mean(vtheta) # valor esperado do estimador de MV de theta 
sd(vtheta) # erro-padrao verdadeiro (estimativa de MC) 
mean(vep) # valor esperado do estimador do erro-padrao 
mean(vep_boot) # valor esperado do estimador bootstrap do erro-padrao