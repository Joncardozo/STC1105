# Crie uma função em R que recebe como argumentos de entrada os seguintes 
# dados: x0, k, M, a e c. Essa função deve retornar k ocorrências da 
# distribuição uniforme geradas por meio do gerador linear congruencial, 
# em que x0 é a semente, M é o médulo, a é o multiplicador e c é o 
# deslocamento.Crie uma função em R que recebe como argumentos de entrada 
# os seguintes dados: x0, k, M, a e c. Essa função deve retornar k 
# ocorrências da distribuição uniforme geradas por meio do gerador linear 
# congruencial, em que x0 é a semente, M é o módulo, a é o multiplicador 
# e c é o deslocamento.

runif_lin_congr <- function(x0, k, M, a, c) {
  sequencia <- c(x0)
  for (i in 2:(k+1)) {
    prox <- (a*sequencia[i - 1] + c) %% M;
    sequencia <- append(sequencia, prox);
  }
  sequencia <- sequencia[-1] / M;
  sequencia
}