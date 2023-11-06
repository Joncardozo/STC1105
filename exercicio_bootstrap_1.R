# Exercício 1 métodos bootstrap

n <- 10000
theta <- 5
B <- 500
sample_size <- 15

theta_boot_sim <- rep(NA, n)
theta_hat <- rep(NA, n)
se_approx <- rep(NA, n)
se_boot <- rep(NA, n)
for (i in 1:n){
  amostra <- rexp(sample_size, theta)
  se_approx[i] <- 1/(sqrt(sample_size)*mean(amostra))
  theta_hat[i] <- 1/mean(amostra)
  theta_boot <- rep(NA, B)
  for (j in 1:B){
    boot_sample <- sample(amostra, length(amostra),replace = T)
    theta_boot[j] <- 1/mean(boot_sample)
  }
  se_boot[i] <- sd(theta_boot)
  theta_boot_sim[i] <- mean(theta_boot)
}

print(c("estimador MV para theta: ", mean(theta_hat)))
print(c("estimador MV para SE: ", sd(theta_hat)))
print(c("SE aproximado: ", mean(se_approx)))
print(c("estimador bootstrap para SE: ", mean(se_boot)))
