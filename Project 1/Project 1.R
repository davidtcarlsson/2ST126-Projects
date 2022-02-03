# The significance level we will use for both tests
alpha <- 0.05

# Calculates the MLE estimates for both distributions
mle_exp <- function(xi) {
  n <- length(xi)
  lambda_hat <- n / sum(xi)
  sd <- lambda_hat / sqrt(n)
  return(c(lambda_hat, sd))
}

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
  abs(theta_hat - theta) / sd < qnorm(1 - alpha / 2)
}

score_test <- function(xi, theta, sd, alpha) {
  # Not defined yet
  mean(xi)
}

# Generate 10000 samples and run the a given test on each of them
# returns the proportion of tests where the confidence interval "covered"
# the true parameter value
run_exp_tests <- function(test_statistic) {
  counter <- 1
  # Loop through both of our tests
  for (test_statistic in c(wald_test, score_test)) {
    # Print out the description at the start
    print(c("Wald test on exponential distribution", 
            "Score test on exponential distribution")[counter])
    # Loop trough a number of different lambdas
    for (lambda in c(0.1, 1, 10)) {
      # Loop through a number of different sample sizes
      for (n in c(10, 50, 100)) {
        # For a given lambda and sample size generate 10000 samples
        xi <- replicate(10000, rexp(n, lambda), simplify = FALSE)
        # Test each sample and return the proportion that fulfilled the test
        results <- mean(unlist(lapply(X = xi, FUN = test_statistic, lambda, mle_exp)))
        # Print out lambda, sample size and the proportion
        print(paste("n =", n, "lambda =", lambda, "prop = ", results))
      }
      print("-----------------------------------------------")
    }
  }
}

run_binom_tests <- function() {
  counter <- 1
  # Loop through both of our tests
  for (test_statistic in c(wald_test, score_test)) {
    # Print out the description at the start
    print(c("Wald test on binomial distribution", 
            "Score test on binomial distribution")[counter])
    counter <- counter + 1
    # Loop trough a number of different p's
    for (p in c(0.1, 0.3, 0.5)) {
      # Loop through a number of different sample sizes
      for (n in c(10, 50, 100)) {
        # For a given p and sample size generate 10000 samples
        xi <- replicate(10000, rbinom(n = n, size = 1, prob = p), simplify = FALSE)
        # Test each sample and return the proportion that fulfilled the test
        results <- mean(unlist(lapply(X = xi, FUN = test_statistic, p, mle_binom)))
        # Print out lambda, sample size and the proportion
        print(paste("n =", n, "p =", p, "prop =", results))
      }
      print("-----------------------------------------------")
    }
  }
}

# Run the tests on both distributions
run_exp_tests()
run_binom_tests()
