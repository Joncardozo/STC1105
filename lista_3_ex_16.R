# Construa uma função para o problema do exercício anterior, em que 
# o argumento de entrada deve ser o número de termos a serem 
# considerados na soma. Analise a aproximação ao número e considerando 
# diferentes números de elementos na soma.

e_x_potencia <- function(x, n) {
  soma <- 0;
  
  for (i in 1:n) {
    potencia <- x ^ i;
    
    i_fatorial <- 1;
    for (j in 2:i){
      i_fatorial <- i_fatorial * j;
    }
    
    termo <- potencia / i_fatorial;
    soma <- soma + termo;
  }
  soma
}


e_x <- c();
ns <- seq(10, 100, 10);
for (i in ns) {
  e_x <- c(e_x, e_x_potencia(10, i));
}

e_x_real <- rep(exp(10), 10);
diferenca <- abs(e_x - e_x_real);

comparacao <- data.frame("numero_termos"=ns, "soma"=e_x, 
           "valor_exato"=e_x_real, "diferença"=diferenca)


print(comparacao)