1. Um dos pilares da ciência é reprodutibilidade dos experimentos pelos pesquisadores, a fim de obter os mesmos resultados seguindo um conjunto de procedimentos. Como uma amostra verdadeiramente aleatória não pode ser reproduzida, a não ser que seja por acaso, a amostra pseudo-aleatória é preferível em experimentos por permitir que eles sejam reproduzíveis e que os resultados sejam verificáveis. Em contraposição, aplicações como criptografia requerem algoritmos dos quais não é possível reproduzir a realização após o experimento.
2. O período T é um número inteiro, o menor inteiro T, do qual $$u_{i+T}=u_i, \forall i$$ ou seja, é a quantidade de realizações necessárias para que a sequência pseudo-aleatória gerada se repete.
3. 
 ```R
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
```

Conclui-se que para esta semente o período é 1, ou seja, esta semente é incapaz de gerar qualquer número aleatório, repetindo o mesmo número em cada iteração.
4. 
```R 

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
```
Este script retorna: ```R [1] 3 4 2 1 3 4 ``` 
Aparentemente, o período T desta sequência é 4.

5. 
```R 
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
```
Saída do script: 
![[plot_lista_3_ex_5.png]]

Não foi observado uma sequência de números que satisfaz $u_{i+T}=u_i, \forall i$. Portanto, não é possível estabeler o período da sequência observada. Nota-se, porém, que um padrão se repete em $T = 3$, com uma inclinação linear característica desse tipo de gerador. Ainda que o período não possa ser estabelecido, conclui-se que o restante da sequência é determinada a partir dos primeiros 3 números da sequência.
6. 
```R 
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
```

8. 
```R 
# Crie uma função em R que recebe um inteiro k e 
# retorna k ocorrência de uma variável aleatória 
# com distribuição normal padrão geradas pelo 
# Método de Box-Muller.

normal_BW <- function(k){
  amostra <- c();
  for (i in 1:k){
    realizacao <- sqrt(-2*log(runif(1)))*cos(2*pi*runif(1));
    amostra <- c(amostra, realizacao);
  }
  amostra
}
```

9. 
```R 
# Crie uma função em R que recebe um inteiro k e 
# retorna k ocorrência de uma variável aleatória 
# com distribuição normal padrão geradas pelo Método Polar.

normal_polar <- function(k){
  amostra <- c();
  for (i in 1:k){
    
    repeat {
      U1 <- runif(1);
      U2 <- runif(1);
      X <- 2*U1 - 1;
      Y <- 2*U2 - 1;
      W <- X^2 + Y^2;
      
      if (W <= 1)
      {
        prox <- sqrt(-2*log(W)/W)*X;
        amostra <- c(amostra, prox);
        break
      }
    }
  }
  amostra
}
```

10. 
```R
# Crie uma função em R que recebe como argumentos de entrada uma semente (x0) 
# e um número inteiro k e retorna k ocorrências da distribuição uniforme por 
# meio do método Midsquare

runif_midsquare <- function (x0, k) {
  sequencia <- c(x0);
  for (i in 2:(k + 1)) {
    prox <- trunc((sequencia[i - 1] ^ 2 %% 10 ^ 6 / (10 ^ 2)));
    sequencia <- append(sequencia, prox);
  }
  sequencia <- sequencia / 10000;
  sequencia
}
```

11. 
```R
# Crie uma função em R que recebe como argumentos de entrada os seguintes 
# dados: x0, k, M, a e c. Essa função deve retornar k ocorrências da 
# distribuição uniforme geradas por meio do gerador linear congruencial, 
# em que x0 é a semente, M é o médulo, a é o multiplicador e c é o 
# deslocamento.Crie uma função em R que recebe como argumentos de entrada 
# os seguintes dados: x0, k, M, a e c. Essa função deve retornar k 
# ocorrências da distribuição uniforme geradas por meio do gerador linear 
# congruencial, em que x0 é a semente, M é o módulo, a é o multiplicador 
# e c é o deslocamento.

runif_lin_congr <- function(x0, k, M, a, c) {
  sequencia <- c(x0)
  for (i in 2:(k+1)) {
    prox <- (a*sequencia[i - 1] + c) %% M;
    sequencia <- append(sequencia, prox);
  }
  sequencia <- sequencia[-1] / M;
  sequencia
}
```

12. 
```R
# Crie uma função em R que recebe um inteiro k e retorna k ocorrência
# da variável aleatória X que possui função distribuição 
# F(x) = 1 − exp(−αx^β), 0 < x < ∞. A distribuição de X é denominada 
# de Weibull. Use o método da Inversão (você pode usar a função runif 
# para gerar ocorrências da uniforme).

rweibull_inv <- function (k, alpha, beta) {
  sequencia <- c();
  for (i in 1:k) {
    u <- runif(1);
    ocorrencia <- (log(u) / alpha) ^ (1 / beta);
    sequencia <- append(sequencia, ocorrencia);
  }
  sequencia
}
```

Função densidade de probabilidade inversa de Weibull:
$$F^{-1}(u)=\sqrt[\beta]{\dfrac{\log{u}}{\alpha}}$$
13. 
```R
# Escrever um algoritmo para calcular e imprimir as 10 primeiras 
# potências de 3.

for (i in 1:10){
  print(3 ^ i)
}
```

14. 
```R
# Usando um laço, escreva um algoritmo que calcula o fatorial de um inteiro n.

n <- 10;     # exemplo n = 10
res <- 1;

for (i in 2:n){
  res <- res * i;
}

print(res)
```

15. 
```R
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
```

16. 
```R
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
```

Saída do script:

```R
   numero_termos     soma valor_exato   diferença
1             10 12836.31    22026.47 9190.160680
2             20 21985.48    22026.47   40.983769
3             30 22020.46    22026.47    6.001759
4             40 22020.47    22026.47    6.000000
5             50 22020.47    22026.47    6.000000
6             60 22020.47    22026.47    6.000000
7             70 22020.47    22026.47    6.000000
8             80 22020.47    22026.47    6.000000
9             90 22020.47    22026.47    6.000000
10           100 22020.47    22026.47    6.000000
```

Nota-se que a precisão aumenta rapidamente com o número de termos da expansão. Porém a convergência estagna não sendo mais possível aproximar mais inserindo mais termos devido ao overflow do cálculo do fatorial a partir de 100 termos.

17. 
```R
# Suponha que um dado é lançado e deseja-se estimar a probabilidade da face observada
# ser 3. Considere os dois seguintes programas que tentam dar uma solução numérica à 
# este problema:

# programa 1
cont<-1 
for(i in 1: 100) { 
	face<-sample(6,1) 
	if (3==face) 
		cont<-cont+1 
} 
print(cont/100)

# programa 2
cont<-1 
for(i in 1: 100) { 
	face<-round(6*runif(1)) 
	if (3==face) 
		cont<-cont+1 
} 
print(cont/100)
```

O programa 2 é problemático pois a função round arrendonda para o valor para o inteiro mais próximo. Como a função runif gera uma realização aleatória da distribuição uniforme, o termo ``` 6*runif(1)``` gera valores aleatórios entre 0 e 6. Alguns destes valores podem estar compreendidos entre 0 e 0.5, que arredondados para 0 apresentam uma realização fora do espaço amostral de um lançamento de um dado de 6 faces. 

O programa 2, através da função ```sample(6,1)``` apresenta uma implementação correta para o problema apresentado.

18. 
```R
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

```

Resultado obtido: 
```R
[1] 0.3769
```

Valor exato: $$\binom{3}{2} 0.5^2 0.5 = 0.375$$
