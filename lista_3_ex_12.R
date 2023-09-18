# Crie uma função em R que recebe um inteiro k e retorna k ocorrência
# da variável aleatória X que possui função distribuição 
# F(x) = 1 − exp(−αx^β), 0 < x < ∞. A distribuição de X é denominada 
# de Weibull. Use o método da Inversão (você pode usar a função runif 
# para gerar ocorrências da uniforme).

rweibull_inv <- function (k, alpha, beta) {
  sequencia <- c();
  for (i in 1:k) {
    u <- runif(1);
    ocorrencia <- (log(u) / alpha) ^ (1 / beta);
    sequencia <- append(sequencia, ocorrencia);
  }
  sequencia
}