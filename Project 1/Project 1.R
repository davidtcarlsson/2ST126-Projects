# The significance level we will use for both tests
alpha <- 0.05

# Calculates the MLE estimates the exponential distribution
mle_exp <- function(xi) {
  n <- length(xi)
  lambda_hat <- n / sum(xi)
  sd <- lambda_hat / sqrt(n)
  return(c(lambda_hat, sd))
}

# Calculates the MLE estimates the binomial distribution
mle_binom <- function(xi) {
  n <- length(xi)
  p_hat <- sum(xi) / n
  sd <- sqrt(p_hat * (1 - p_hat) / n) 
  return(c(p_hat, sd))
}

# Checks if KI covers the population parameter for our two tests
wald_test <- function(xi, theta, mle_estimates) {
  # Extract the parameters from the vector
  theta_hat <- mle_estimates(xi)[1]
  sd <- mle_estimates(xi)[2]
  (abs(theta_hat - theta) / sd) < qnorm(1 - alpha / 2)
}

# Runs the score test for the exponential distribution
score_test_exp <- function(xi, lambda0, mle_estimates) {
  n <- length(xi)
  xbar <- mean(xi)
  sqrt(n) * abs(1 - lambda0 * xbar) < qnorm(1 - alpha / 2)
}

# Runs the score test for the binomial distribution
score_test_binom <- function(xi, p0, mle_estimates) {
  n <- length(xi)
  xbar  <- mle_estimates(xi)[1]
  (abs(xbar - p0) / sqrt((p0 * (1 - p0)) / n)) < qnorm(1 - alpha / 2)
}

# Generate 10000 samples and run the a given test on each of them
# returns the proportion of tests where the confidence interval "covered"
# the true parameter value
run_exp_tests <- function() {
  print("Tests on the exponential distribution")
  # Loop trough a number of different lambdas
  for (lambda in c(0.1, 1, 10)) {
    # Loop through a number of different sample sizes
    for (n in c(10, 50, 100)) {
      # For a given lambda and sample size generate 10000 samples
      xi <- replicate(10000, rexp(n, lambda), simplify = FALSE)
      # Test each sample and return the proportion that fulfilled the test
      results_score <- mean(unlist(lapply(X = xi, FUN = score_test_exp, lambda, mle_exp)))
      results_wald <- mean(unlist(lapply(X = xi, FUN = wald_test, lambda, mle_exp)))
      # Print out lambda, sample size and the proportion
      print(paste("Score", "n =", n, "lambda =", lambda, "prop = ", results_score))
      print(paste("Wald", "n =", n, "lambda =", lambda, "prop = ", results_wald))
    }
    print("-----------------------------------------------")
  }
}

run_binom_tests <- function() {
  print("Tests on the binomial distribution")
  # Loop trough a number of different p's
  for (p in c(0.1, 0.3, 0.5)) {
    # Loop through a number of different sample sizes
    for (n in c(10, 50, 100)) {
      # For a given p and sample size generate 10000 samples
      xi <- replicate(10000, rbinom(n = n, size = 1, prob = p), simplify = FALSE)
      # Test each sample and return the proportion that fulfilled the test
      results_score <- mean(unlist(lapply(X = xi, FUN = score_test_binom, p, mle_binom)))
      results_wald <- mean(unlist(lapply(X = xi, FUN = wald_test, p, mle_binom)))
      # Print out lambda, sample size and the proportion
      print(paste("Score","n =", n, "p =", p, "prop =", results_score))
      print(paste("Wald","n =", n, "p =", p, "prop =", results_wald))
    }
    print("-----------------------------------------------")
  }
}

# Run the tests on both distributions
run_exp_tests()
run_binom_tests()
