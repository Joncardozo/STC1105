# Exercício seção 5.2 métodos bootstrap

theta <- 50
B <- 10000
sample_size <- 15

theta_boot_sim <- rep(NA, n)
theta_hat <- rep(NA, n)
phi_hat <- rep(NA, n)
phi_hat_boot <- rep(NA, n)
bias <- rep(NA, n)

amostra <- rpois(sample_size, theta)
theta_hat <- mean(amostra)
phi_hat <- exp(-theta_hat)
theta_boot <- rep(NA, B)
bias_boot <- -theta_hat
for (j in 1:B){
  boot_sample <- sample(amostra, sample_size,replace = TRUE)
  theta_boot[j] <- mean(boot_sample)
  bias_boot <- bias_boot + mean(theta_boot[j])/B
}

theta_boot_sim <- mean(theta_boot)
phi_hat_boot <- exp(-theta_boot_sim)

print(c("theta: ", theta))
print(c("estimador MV para theta: ", mean(theta_hat)))
print(c("estimador bootstrap para theta: ", theta_boot_sim))
print(c("vies do estimador: ", bias_boot))
print(c("estimador corrigido: ", theta_hat + bias_boot))
