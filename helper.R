# Return what percent of all intervals contain mu
# @param mu 
# @param intervals 2xn array of intervals
pct_containing=function(intervals, mu){
  num_hits = ncol(intervals[, intervals[1,] <= mu &
                              intervals[2,] >= mu])
  return(num_hits/ncol(intervals))
}