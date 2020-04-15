# This module implements Least Squared Approximation as explained in
# Friedberg, Insel, and Spence's Linear Algebra, section 6.3

LSA = function(x, y, n = 1) {
  # Returns a vector of coefficients for least squared polynomial approximation
  # Note the first value is the y-intercept, the last is the x^n coefficient
  # @param x
  # @param y
  
  l = length(x)
  A = matrix(1, nrow = l, ncol = 1)
  for (i in seq(1, n)) {
    temp = matrix(x ^ i, nrow = l, ncol = 1)
    A = cbind(temp, A)
  }
  return(c(solve(t(A) %*% A) %*% t(A) %*% y))
}

poly = function(x, y, n) {
  # Returns vector of y-values
  px = seq(min(x), max(x), 0.1)
  py = integer(length(px))
  c = LSA(x, y, n)
  for (i in seq(0, n)) {
    py = py + c[n + 1 - i] * px ^ (i)
  }
  return(py)
}
# TODO: find a way to add more colours
# TODO: add a legend for the colours
# TODO: display the polymial expressions
# TODO: figure out how to do this in ggplot
colors = c("red", "green", "blue", "mediumorchid1")
LSAplot = function(x,
                   y,
                   n = 1,
                   col = colors,
                   all = FALSE) {
  # Plots the least squares polynomial degree n approximation
  px = seq(min(x), max(x), 0.1)
  plot(x, y)
  for (j in seq(1, n)) {
    if (all | j == n)
      lines(px, poly(x, y, j), col = col[j %% length(colors)])
  }
}

# TODO: I have a theoretical problem
# If I have the data x=c(1,2) and y=c(1,2), then any human could tell me that the degree n polynomial of "best fit"
# is y=x for all n>=1. But the method won't be able to figure that out because it will require solving a singular matrix.
# Is there a better way to compute the best fit polynomial of degree n?