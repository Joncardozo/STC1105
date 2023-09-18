# Usando um laço, escreva um algoritmo que calcula o fatorial de um inteiro n.

n <- 10;     # exemplo n = 10
res <- 1;

for (i in 2:n){
  res <- res * i;
}

print(res)