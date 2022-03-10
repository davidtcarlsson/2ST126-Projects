# Library that I use to apply the "pipe" operator. Only difference it makes
# is that the test functions look a lot "cleaner"
library(magrittr)

# Set a seed so that we are able to reproduce the results
set.seed(2022)

# The significance level we will use for both tests
alpha <- 0.05

# Subtask 1 ####

# Subtask 2 ####

iterations <- 1000
x <- c(2, 4, 0, 3, 5, 3, 1, 3, 4, 3)
n <- length(x)

## Non-parametric bootstrap ####
# Generate 1000 bootstrap samples
boot <- replicate(iterations, sample(x, n, replace = TRUE), simplify = FALSE)

# Comparing distributions with our sample
x %>% table
boot %>% unlist %>% table 

# Plot the distribution for the mean values from our bootstrap samples

# Histogram parameters is defined as
breakpoints <- seq(1, 5, by = 0.2)
ylimits <- c(0, 200)
xlimits <- c(1, 5)

# Which gives us the plot
par(mfrow = c(2, 1))
vapply(boot, FUN = mean, FUN.VALUE = double(1)) %>% 
  hist(x = ., 
       main = "Distribution for the mean (boot)", 
       col = "lightblue", 
       breaks = breakpoints,
       ylim = ylimits,
       xlim = xlimits,
       xlab = "Mean")
  
# Comparing means in our sample with the bootstrap samples
mean(x)
boot %>% unlist %>% mean

# Calculate the confidence interval on the bootstrap samples and our sample
t.test(x, conf.level = alpha)
t.test(unlist(boot), conf.level = alpha) # KI is a lot narrower for the bootstrap samples

## Parametric bootstrap ####

# We estimate theta with the sample mean
theta_hat <- mean(x)

# Generate a sample drawn from a Poisson distribution with our estimated
# theta as the parameter
sim <- replicate(iterations, rpois(n, lambda_hat), simplify = FALSE)

# Distribution for the means from simulations
vapply(sim, FUN = mean, FUN.VALUE = double(1)) %>% 
  hist(x = ., 
       main = "Distribution for the mean (sim)", 
       col = "lightgreen", 
       breaks = breakpoints,
       ylim = ylimits,
       xlim = xlimits,
       xlab = "Mean")

# Calculate the confidence interval on the simulated samples and our sample
t.test(x, conf.level = alpha)
t.test(unlist(sim), conf.level = alpha)
