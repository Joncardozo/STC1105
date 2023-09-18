# Crie uma função em R que recebe como argumentos de entrada uma semente (x0) 
# e um número inteiro k e retorna k ocorrências da distribuição uniforme por 
# meio do método Midsquare

runif_midsquare <- function (x0, k) {
  sequencia <- c(x0);
  for (i in 2:(k + 1)) {
    prox <- trunc((sequencia[i - 1] ^ 2 %% 10 ^ 6 / (10 ^ 2)));
    sequencia <- append(sequencia, prox);
  }
  sequencia <- sequencia[-1] / 10000;
  sequencia
}