# Library that I use to apply the "pipe" operator. Only difference it makes
# is that the test functions look a lot "cleaner"
library(magrittr)

# Set a seed so that we are able to reproduce the results
set.seed(2022)

# The significance level we will use for both tests
alpha <- 0.05

# Number of simulated datasets 
iterations <- 10000

# Generate 10000 samples and run the a given test on each of them
# returns the proportion of tests where the confidence interval "covered"
# the true parameter value
run_exp_tests <- function(alpha, iterations) {
  # Create a matrix that we will add our results to
  output <- matrix(nrow = 9, ncol = 4)
  colnames(output) <- c("n", "p", "Score-testet", "Wald-testet")
  # Counter to keep track on which row to add the new data in the output matrix
  counter <- 1
  # Loop trough a number of different lambdas
  for (lambda in c(0.1, 1, 10)) {
    # Loop through a number of different sample sizes
    for (n in c(10, 50, 100)) {
      # For a given lambda and sample size generate 10000 samples
      xi <- replicate(iterations, rexp(n, lambda), simplify = FALSE)
      # Run a function that applies the score test
      result_score <- lapply(xi, FUN = function(xi) {
        n <- length(xi)
        xbar <- mean(xi)
        (sqrt(n) * abs(1 - lambda * xbar)) < qnorm(1 - alpha / 2)
      }) %>% unlist %>% mean
      # Run a function that applies the wald test
      result_wald <- lapply(xi, FUN = function(xi) {
        lambda_hat <- n / sum(xi)
        sd <- lambda_hat / sqrt(n)
        (abs(lambda_hat - lambda) / sd) < qnorm(1 - alpha / 2)
      }) %>% unlist %>% mean
      # Add the results to the table
      output[counter, ] <- c(n, lambda, result_score, result_wald)
      counter <- counter + 1
    }
  }
  return(output)
}

run_binom_tests <- function(alpha, iterations) {
  # Create a matrix that we will add our results to
  output <- matrix(nrow = 9, ncol = 4)
  colnames(output) <- c("n", "Lambda", "Score-testet", "Wald-testet")
  # Counter to keep track on which row to add the new data in the output matrix
  counter <- 1
  # Loop trough a number of different p's
  for (p in c(0.1, 0.3, 0.5)) {
    # Loop through a number of different sample sizes
    for (n in c(10, 50, 100)) {
      # For a given p and sample size generate 10000 samples
      xi <- replicate(iterations, rbinom(n = 1, size = n, prob = p), simplify = FALSE)
      # Run a function that applies the score test
      result_score <- lapply(xi, FUN = function(xi) { 
        xbar <- xi / n 
        (abs(xbar - p) / sqrt((p * (1 - p)) / n)) < qnorm(1 - alpha / 2)
      }) %>% unlist %>% mean
      # Run a function that applies the wald test
      result_wald <- lapply(xi, FUN = function(xi) {
        xbar <- xi / n
        sd <- sqrt(xbar * (1 - xbar) / n) 
        (abs(xbar - p) / sd) < qnorm(1 - alpha / 2)
      }) %>% unlist %>% mean
      # Add the results to the table
      output[counter, ] <- c(n, p, result_score, result_wald)
      counter <- counter + 1
    }
  }
  return(output)
}

# Run the tests on both distributions and generate two output tables
exp_results <- run_exp_tests(alpha, iterations)
binom_results <- run_binom_tests(alpha, iterations)

# Comments
# Score and wald test yield the same results on the exponential distribution
# For the binomial distribution the score and wald test converges when n->inf.
# For small sample sizes the wald test are further from the significance level 
# than the score test.
