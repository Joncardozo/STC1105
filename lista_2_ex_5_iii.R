# lista 2 - exercicio 5 iii
library(dplyr)

# funcao para gerar intervalo de conficanca
gera_intervalo_confianca <- function(alpha, theta_hat, n){
  limite_inferior <- theta_hat - qnorm(1 - alpha/2)*sqrt(theta_hat*(1-theta_hat)/n);
  limite_superior <- theta_hat + qnorm(1 - alpha/2)*sqrt(theta_hat*(1-theta_hat)/n);
  c(limite_inferior, limite_superior)
}

# dados do exercicio
alpha <- 0.05;
monte_carlo_itr <- 1000;
simul <- 0;

# simulacao de monte-carlo
for (i in 1:monte_carlo_itr){
  theta <- runif(1);
  n <- sample(30:100, 1);
  amostra <- rbinom(1, n, theta);
  theta_hat <- amostra / n;
  intervalo <- gera_intervalo_confianca(alpha, theta_hat, n);
  simul <- simul + between(theta, intervalo[1], intervalo[2]) / monte_carlo_itr;
}

# resultado da simulacao: 
# razao em que o parametro real foi capturado pelo intervado de confianca
resultado <- simul;
cat("A hipótese nula foi rejeitada em  ", (1-resultado)*100, "% das das simulacoes\n")
