
# Considere um gerador linear congruencial.
# Sejam M = 5, a = 3, c = 0 e x0 = 1. 
# Gere x1, x2, . . . , x6. 
# O que voce conclui? Qual e o periodo?

gera_numero_aleatorio <- function(M, a, c, x0){
  numero <- (a*x0 + c) %% M
}

M <- 5;
a <- 3;
c <- 0;
x0 <- 1;

n <- 6;
sequencia_aleatoria <- c(x0);

for (i in 2:(n+1)){
  proximo <- gera_numero_aleatorio(M, a, c, sequencia_aleatoria[i-1]);
  sequencia_aleatoria <- c(sequencia_aleatoria, proximo);
}

sequencia_aleatoria <- sequencia_aleatoria[2:length(sequencia_aleatoria)]

sequencia_aleatoria