gera_numero_aleatorio <- function(M, a, b, c, x0){
  numero <- (a*x0^2 + b*x0 + c) %% M
}

M <- 128;
a <- 1;
b <- 2
c <- 3;
x0 <- 10;

n <- 3;
sequencia_aleatoria <- c(x0);

for (i in 2:(n+1)){
  proximo <- gera_numero_aleatorio(M, a, b, c, sequencia_aleatoria[i-1]);
  sequencia_aleatoria <- c(sequencia_aleatoria, proximo);
}

sequencia_aleatoria <- sequencia_aleatoria[2:length(sequencia_aleatoria)]

sequencia_aleatoria

