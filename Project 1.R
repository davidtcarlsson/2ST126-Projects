# Sample estimates for the exp dist
exp_dist <- function(xi, lambda, test_statistic) {
  alpha <- 0.05
  n <- length(xi)
  lambda_hat <- n / sum(xi)
  sd <- lambda_hat / sqrt(n)
  test_statistic(lambda_hat, lambda, sd, alpha)
}

# Sample estimates for the binom dist
bin_dist <- function(xi, p, test_statistic) {
  alpha <- 0.05
  n <- length(xi)
  p_hat <- sum(xi) / n
  sd <- sqrt(p_hat * (1 - p_hat) / n) 
  test_statistic(p_hat, p, sd, alpha)
}

# Check if estimate fulfill the wald test
wald_test <- function(theta_hat, theta, sd, alpha) {
  abs(theta_hat - theta) / sd < qnorm(1 - alpha / 2)
}

# Check if estimate fulfill the score test
score_test <- function(theta, sd, alpha) {
  
}

# Generate 10000 samples and run the a given test on each of them
# returns the proportion of tests where the confidence interval "covered"
# the true parameter value
run_exp_tests <- function(test_statistic) {
  print("Exponential distribution")
  # Loop trough a number of different lambdas
  for (lambda in c(0.1, 1, 10)) {
    # Loop through a number of different sample sizes
    for (n in c(10, 50, 100)) {
      # For a given lambda and sample size generate 10000 samples
      xi <- replicate(10000, rexp(n, lambda), simplify = FALSE)
      # Test each sample and return the proportion that fulfilled the test
      results <- mean(unlist(lapply(X = xi, FUN = exp_dist, lambda, test_statistic)))
      # Print out lambda, sample size and the proportion
      print(paste("n =", n, "lambda =", lambda, "prop = ", results))
    }
    print("-----------------------------------------------")
  }
}

run_binom_tests <- function(test_statistic) {
  print("Binomial distribution")
  # Loop trough a number of different p's
  for (p in c(0.1, 0.3, 0.5)) {
    # Loop through a number of different sample sizes
    for (n in c(10, 50, 100)) {
      # For a given p and sample size generate 10000 samples
      xi <- replicate(10000, rbinom(n = n, size = 1, prob = p), simplify = FALSE)
      # Test each sample and return the proportion that fulfilled the test
      results <- mean(unlist(lapply(X = xi, FUN = bin_dist, p, test_statistic)))
      # Print out lambda, sample size and the proportion
      print(paste("n =", n, "p =", p, "prop =", results))
    }
    print("-----------------------------------------------")
  }
}

run_exp_tests(wald_test)
#run_exp_tests(score_test)

run_binom_tests(wald_test)
# run_binom_tests(score_test)



