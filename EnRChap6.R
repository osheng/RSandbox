# Problems from Evans and Rosenthal Chapter 6

# Question 6.3.19
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


# Question 6.3.21: This question require package Rlab for rbern.
compute_bernCI=function(sample_data,g){
  mu = mean(sample_data)
  fisher_info=length(sample_data)*(1/(mu*(1-mu)))
  width = qnorm((1+g)/2)*sqrt(1/fisher_info)
  return(c(mu-width,mu+width))
}
 
question6_3_21=function(num_samples, sample_size, bparam, g){
  
  replicate(num_samples, rbern(sample_size,bparam))
  
}
question6_3_21(10^3, 5, 0.5, 0.95)