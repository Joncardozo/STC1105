# lista 2 - exercicio 2
library(dplyr)

# funcao para gerar intervalo de conficanca
gera_intervalo_confianca <- function(alpha, theta_hat, n, amostra){
  limite_inferior <- theta_hat - qnorm(1 - alpha/2)*theta_hat/sqrt(sum(amostra));
  limite_superior <- theta_hat + qnorm(1 - alpha/2)*theta_hat/sqrt(sum(amostra));
  c(limite_inferior, limite_superior)
}

# dados do exercicio
n <- 30;
theta <- 2;
alpha <- 0.1;
monte_carlo_itr <- 1000;

# simulacao de monte-carlo
simul <- 0;
for (i in 1:monte_carlo_itr){
  amostra <- rpois(n, theta);
  theta_hat <- mean(amostra);
  intervalo <- gera_intervalo_confianca(alpha, theta_hat, n, amostra);
  simul <- simul + between(theta, intervalo[1], intervalo[2]) / monte_carlo_itr;
}

# resultado da simulacao: 
# razao em que o parametro real foi capturado pelo intervado de confianca
resultado <- simul;
cat("O parametro foi capturado pelo intervalo em ", resultado*100, "% das das simulacoes\n")
