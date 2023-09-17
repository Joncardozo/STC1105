#Considere um gerador Midsquare. 
#Para x0 = 3792 gere uma sequencia de 4 numeros pseudo-aleatorios. 
#O que voce conclui?

x0 <- 3792;
x1 <- trunc((x0 ^ 2 %% 10 ^ 6) / (10 ^ 2));
seq_aleatoria <- c(x1);

for (i in 2:4){
  x_next <- trunc((seq_aleatoria[i - 1] ^ 2 %% 10 ^ 6 / (10 ^ 2)));
  seq_aleatoria <- c(seq_aleatoria, x_next);
}

seq_aleatoria