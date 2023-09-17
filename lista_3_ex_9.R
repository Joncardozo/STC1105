# Crie uma função em R que recebe um inteiro k e 
# retorna k ocorrência de uma variável aleatória 
# com distribuição normal padrão geradas pelo Método Polar.

normal_polar <- function(k){
  amostra <- c();
  for (i in 1:k){
    
    repeat {
      U1 <- runif(1);
      U2 <- runif(1);
      X <- 2*U1 - 1;
      Y <- 2*U2 - 1;
      W <- X^2 + Y^2;
      
      if (W <= 1)
      {
        prox <- sqrt(-2*log(W)/W)*X;
        amostra <- c(amostra, prox);
        break
      }
    }
  }
  amostra
}