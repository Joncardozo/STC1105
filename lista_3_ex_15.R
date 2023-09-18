# Escreva uma algoritmo, usando o comando for, para calcular a soma dos 
# 10 primeiros termos de e^x


x <- 10;      # exemplo x = 10
soma <- 0;

for (i in 1:10) {
  potencia <- x ^ i;
  
  i_fatorial <- 1;
  for (j in 2:i){
    i_fatorial <- i_fatorial * j;
  }
  
  termo <- potencia / i_fatorial;
  soma <- soma + termo;
}

print(soma)