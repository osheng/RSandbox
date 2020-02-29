# Problems from Evans and Rosenthal Chapter 6
source("helper.R")
# Question 6.3.19
# Suppose a measurement on a population can be assumed to follow the N(μ,σ^2) distribution,
# (μ,σ^2) ∈ R×(0,∞) is unknown and the size of the population is very large.
# A very conservative upper bound on σ is given by 5.
# A researcher wants to determine a 0.95-confidence interval for μ that is no longer than 1.
# Determine asample size that will guarantee this. (Hint: Start with a large sample approximation.)
#
# TODO:
# This solution runs a linear search for n. Write up a sort of binary search.
# Plot the runtime of the two solutions with respect (1) s and (2) g.
# Since mu and sigma are unknown, we compute the length with the t-distribution.
question6_3_19 = function(s, g) {
  # compute the length of a gamma confidence interval for n samples
  width = function(g, n) {
    return(qt((1 + g) / 2, n - 1) * s / sqrt(n))
  }
  #initialize while loop
  n = 2
  while (2 * width(g, n) > 1) {
    n = n + 1
  }
  sprintf("n must be at least %s, which gives CI of length %s", n, 2 * width(g, n))
}
question6_3_19(5, 0.95)


# Question 6.3.21: This question require package Rlab for rbern.
# Generate 10^3 samples of size n=5 from the Bernoulli(0.5) distribution.
# Foreach of these samples, calculate (6.3.5) with γ=0.95 and record the proportion of intervals
# that contain the true value. What do you notice? Repeat this simulation with n=20.
# What do you notice?

# Compute the confidence intervale for a bernoulli
compute_bernCI = function(sample_data, g) {
  mu = mean(sample_data)
  fisher_info = length(sample_data) * (1 / (mu * (1 - mu)))
  width = qnorm((1 + g) / 2) * sqrt(1 / fisher_info)
  return(c(mu - width, mu + width))
}

question6_3_21 = function(num_samples, sample_size, bparam, g) {
  intervals = replicate(num_samples, compute_bernCI(rbern(sample_size, bparam), g))
  sprintf("%s%% of the confidence intervals contained the true parameter",
          pct_containing(intervals,bparam))
}
question6_3_21(10 ^ 3, 5, 0.5, 0.95)
question6_3_21(10 ^ 3, 10, 0.5, 0.95)
question6_3_21(10 ^ 3, 20, 0.5, 0.95)
question6_3_21(10 ^ 3, 50, 0.5, 0.95)

# Question 6.3.22
# Generate 10^4 samples of size n=5 from the N(0,1) distribution.
# For each ofthese samples, calculate the interval ( ̄x−s/√5, ̄x+s/√5),
# where s is the sample standard deviation, and compute the proportion of times this interval contains μ.
# Repeat this simulation with n=10 and 100 and compare your results.
compute_zI = function(sample_data) {
  mu = mean(sample_data)
  n = length(sample_data)
  s = (sum(sample_data ^ 2) - n * mu ^ 2) / (n - 1)
  width = s / sqrt(n)
  return(c(mu - width, mu + width))
}

question6_3_22 = function(num_samples, sample_size, mu) {
  intervals = replicate(num_samples, compute_zI(rnorm(sample_size, mu , 1)))
  sprintf("%s%% of the  intervals contained the true parameter",
          pct_containing(intervals,mu))
}
question6_3_22(10 ^ 4, 5, 0)
question6_3_22(10 ^ 4, 10, 0)
question6_3_22(10 ^ 4, 100, 0)
