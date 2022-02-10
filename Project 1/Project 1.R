# Set a seed so that we are able to reproduce the results
set.seed(2022)

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
  # Create a matrix that we will add our results to
  exp_output <- matrix(nrow = 9, ncol = 5)
  colnames(exp_output) <- c("Stickprovsstorlek", "p", "Score-testet", "Wald-testet", "Absolut differens")
  counter <- 1
  # Loop trough a number of different lambdas
  for (lambda in c(0.1, 1, 10)) {
    # Loop through a number of different sample sizes
    for (n in c(10, 50, 100)) {
      # For a given lambda and sample size generate 10000 samples
      xi <- replicate(10000, rexp(n, lambda), simplify = FALSE)
      # Test each sample and return the proportion that fulfilled the test
      results_score <- mean(unlist(lapply(X = xi, FUN = score_test_exp, lambda, mle_exp)))
      results_wald <- mean(unlist(lapply(X = xi, FUN = wald_test, lambda, mle_exp)))
      # Add the results to the table
      exp_output[counter, ] <- c(n, lambda, results_score, results_wald, abs(results_score-results_wald))
      counter <- counter + 1
    }
  }
  return(exp_output)
}

run_binom_tests <- function() {
  # Create a matrix that we will add our results to
  binom_output <- matrix(nrow = 9, ncol = 5)
  colnames(binom_output) <- c("Stickprovsstorlek", "Lambda", "Score-testet", "Wald-testet", "Absolut differens")
  counter <- 1
  # Loop trough a number of different p's
  for (p in c(0.1, 0.3, 0.5)) {
    # Loop through a number of different sample sizes
    for (n in c(10, 50, 100)) {
      # For a given p and sample size generate 10000 samples
      xi <- replicate(10000, rbinom(n = n, size = 1, prob = p), simplify = FALSE)
      # Test each sample and return the proportion that fulfilled the test
      results_score <- mean(unlist(lapply(X = xi, FUN = score_test_binom, p, mle_binom)))
      results_wald <- mean(unlist(lapply(X = xi, FUN = wald_test, p, mle_binom)))
      # Add the results to the table
      binom_output[counter, ] <- c(n, p, results_score, results_wald, abs(results_score-results_wald))
      counter <- counter + 1
    }
  }
  return(binom_output)
}

# Run the tests on both distributions and generate two output tables
exp_results <- run_exp_tests()
binom_results <- run_binom_tests()

# Comments
# Score and wald test yield the same results on the exponential distribution
# For the binomial distribution the score and wald test converges when n->inf.
# For small sample sizes the wald test are further from the significance level 
# than the score test.
