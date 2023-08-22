n <- 30;
theta <- 2;
monte_carlo_itr <- 50;
theta_hat <- c();

for (i in 1:monte_carlo_itr){
  amostra <- rpois(n, theta);
  theta_hat <- c(theta_hat, mean(amostra));
}

mean(theta_hat);