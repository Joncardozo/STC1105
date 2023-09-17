# Crie uma função em R que recebe um inteiro k e 
# retorna k ocorrência de uma variável aleatória 
# com distribuição normal padrão geradas pelo 
# Método de Box-Muller.

normal_BW <- function(k){
  amostra <- c();
  for (i in 1:k){
    realizacao <- sqrt(-2*log(runif(1)))*cos(2*pi*runif(1));
    amostra <- c(amostra, realizacao);
  }
  amostra
}