library(foreach)
library(doParallel)
registerDoParallel(8)

comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

start <- Sys.time()

R<- 10000 # numero de replicas de Monte Carlo 
B<- 500 # numero de reamostras Bootstrap 
n<- 15 # tamanho amostral 
theta<- 5 # valor do parametro


#Simulação de Monte Carlo
result <- foreach(i=1:R, .combine = comb, .multicombine = TRUE,
                  .init = list(list(), list(), list())) %dopar%
{ 
  x <- rexp(n,theta) # amostra 
  media <- mean(x) # media da amostra 
  vtheta <- 1/media # estimador de MV de theta 
  vep <- 1/(sqrt(n)*media) # estimativa do erro-padrao do estimador de theta
  vtheta_boot<-rep(NA,B) # vetor para guardas réplicas bootstrap 
  for(j in 1:B) 
  { 
    x_boot <- sample(x,replace=T) # reamostra bootstrap 
    vtheta_boot[j] <- 1/mean(x_boot) # réplica boostrap do estimador de theta 
  } 
  list(sd(vtheta_boot), vtheta, vep) # erro-padrão bootstrap #
}


vep_boot <- unlist(result[[1]])
vtheta <- unlist(result[[2]])
vep <- unlist(result[[3]])


mean(vtheta) # valor esperado do estimador de MV de theta 
sd(vtheta) # erro-padrao verdadeiro (estimativa de MC) 
mean(vep) # valor esperado do estimador do erro-padrao 
mean(vep_boot) # valor esperado do estimador bootstrap do erro-padrao 
print(c("tempo de execucao paralelo: ", Sys.time() - start))