# Considere um gerador linear congruencial. 
# Sejam M = 31, a = 3, c = 0 e x0 = 1. 
# Gere x1, x2, . . . , x30. 
# Faça o gráfico de xi+1 (eixo vertical) × xi (eixo horizontal). 
# O que você conclui? Qual é o período?

gera_numero_aleatorio <- function(M, a, c, x0){
  numero <- (a*x0 + c) %% M
}

M <- 31;
a <- 3;
c <- 0;
x0 <- 1;

n <- 30;
sequencia_aleatoria <- c(x0);

for (i in 2:(n+1)){
  proximo <- gera_numero_aleatorio(M, a, c, sequencia_aleatoria[i-1]);
  sequencia_aleatoria <- c(sequencia_aleatoria, proximo);
}

xi <- sequencia_aleatoria[1:length(sequencia_aleatoria)-1];
xi_prox <- sequencia_aleatoria[2:length(sequencia_aleatoria)];

plot(xi_prox, xi)

