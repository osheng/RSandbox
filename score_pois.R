# Write a function that generates 30 random samples from
# Poisson(λ = 5) dist and calculates the score function for λ = 5
# Run this function 100K times and calculate the mean and variance
# of the output (mean should be ≈ 0 and var ≈ 30/5 = 6)
# Define score function of Poisson(lambda_0) with n data points and evaluated at lambda
score_pois = function(lambda, n, lambda_0) {
  data = rpois(lambda_0, n)
  return(sum(data) / lambda - n)
}

# Estimate mean and variance of score_pois
score_data = replicate(10 ^ 6, score_pois(5, 30, 5))
mean(score_data) # should about 0 for score_pois(5, 30, 5)
var(score_data) # should about 30/5=6 for score_pois(5, 30, 5)