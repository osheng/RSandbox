# Problems from Evans and Rosenthal Chapter 6


# Since mu and sigma are unknown, we compute the length with the t-distribution.
question6_3_19=function(s,g){
  # compute the length of a gamma confidence interval for n samples 
  width=function(g, n){return(qt((1+g)/2,n-1)*s/sqrt(n))}
  #initialize while loop
  n=2
  while (2*width(g, n)>1){n=n+1}
  sprintf("n must be at least %s, which gives CI of length %s", n, 2*width(g,n))
}
question6_3_19(5,0.95)

