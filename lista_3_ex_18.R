# Considere o problema: uma moeda honesta é jogada 3 vezes. Considere X 
# o número de caras obtidas. Determine numericamente, via simulação de 
# Monte Carlo, a probabilidade de X ser igual a 2.

N <- 10000;
x_eq_2 <- 0;

for (i in 1:10000) {
  amostra <- rbinom(1, 3, 0.5);
  if (amostra == 2) {
    x_eq_2 <- x_eq_2 + 1;
  }
}

prob <- x_eq_2 / N;
print(prob)
