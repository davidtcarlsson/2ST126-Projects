counter <- 1
for (n in c(10, 50, 100)) {
  for (lambda in c(0.1, 1, 10)) {
    xi <- replicate(10000, rexp(n, lambda), simplify = FALSE)
    test <- lapply(xi, KI_test, n, lambda)
    mean(unlist(test))
  }
}

KI_test <- function(xi, n, lambda) {
  lambda_hat <- n / sum(xi)
  sd <- 1 / (sqrt(n) / lambda)
  return((lambda_hat - qnorm(0.05/2, 0, 1) * sd < lambda) | (lambda_hat + qnorm(0.05/2, 0, 1) * sd > lambda)) 
}

xi <- replicate(10000, rexp(200, 10), simplify = FALSE)
test <- lapply(xi, KI_test, n=200, lambda=10)
mean(unlist(test))
