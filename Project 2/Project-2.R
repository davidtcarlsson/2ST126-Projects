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

# We estimate theta with the sample mean
theta_hat <- mean(x)

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

# The mean for each sample is calculated by
means_boot <- vapply(boot, FUN = mean, FUN.VALUE = double(1))

# Which gives us the distribution
hist_boot <- function() {
  hist(x = means_boot, 
       main = "Fördelningen av medelvärden (boot)", 
       col = "lightblue", 
       breaks = breakpoints,
       ylim = ylimits,
       xlim = xlimits,
       xlab = "Medelvärde",
       ylab = "Antal") 
  abline(v = mean(means_boot), col = "Red", lwd = 3)
  abline(v = pinterval_lower_boot, col = "Blue", lwd = 3)
  abline(v = pinterval_upper_boot, col = "Blue", lwd = 3)
  abline(v = binterval_lower_boot, col = "Purple", lwd = 3)
  abline(v = binterval_upper_boot, col = "Purple", lwd = 3)
}

# Lets sort the means so that we can pick out the percentile values
means_boot_sorted <- sort(means_boot)

# The percentile interval gives us
m <- (alpha / 2) * iterations
pinterval_lower_boot <- means_boot_sorted[m]
pinterval_upper_boot <- means_boot_sorted[iterations + 1 - m]

# The basic method gives us
binterval_lower_boot <- 2 * theta_hat - means_boot_sorted[iterations + 1 - m]
binterval_upper_boot <- 2 * theta_hat - means_boot_sorted[m]

# Comparing means in our sample with the bootstrap samples
mean(x)
boot %>% unlist %>% mean

## Parametric bootstrap ####

# Generate a sample drawn from a Poisson distribution with our estimated
# theta as the parameter
sim <- replicate(iterations, rpois(n, theta_hat), simplify = FALSE)

# The mean for each sample is calculated by
means_sim <- vapply(sim, FUN = mean, FUN.VALUE = double(1))

# Distribution for the means from simulations
hist_sim <- function() {
  hist(x = means_sim, 
       main = "Fördelningen av medelvärden (sim)", 
       col = "lightgreen", 
       breaks = breakpoints,
       ylim = ylimits,
       xlim = xlimits,
       xlab = "Medelvärde",
       ylab = "Antal")
  abline(v = mean(means_sim), col = "Red", lwd = 3)
  abline(v = pinterval_lower_sim, col = "Blue", lwd = 3)
  abline(v = pinterval_upper_sim, col = "Blue", lwd = 3)
  abline(v = binterval_lower_sim, col = "Purple", lwd = 3)
  abline(v = binterval_upper_sim, col = "Purple", lwd = 3)
}

# Lets sort the means so that we can pick out the percentile values
means_sim_sorted <- sort(means_sim)

# The percentile interval gives us
m <- (alpha / 2) * iterations
pinterval_lower_sim <- means_sim_sorted[m]
pinterval_upper_sim <- means_sim_sorted[iterations + 1 - m]

print("Percentile method") 
c(pinterval_lower_sim, "theta", pinterval_upper_sim)

# The basic method gives us
binterval_lower_sim <- 2 * theta_hat - means_sim_sorted[iterations + 1 - m]
binterval_upper_sim <- 2 * theta_hat - means_sim_sorted[m]

# Plots ####
par(mfrow = c(2, 1))
hist_boot()
hist_sim()
