### Integration of survival function

# bxf is a function that returns the density estimate of the 
## survival function at each time.
# Known distributions for b,f need to be multiplied together beforehand
# b is average number of secondaries per primary at time t after infection
# f is function for probability of survival until time t after infection
# max is the maximum length to measure
cont_surv_func <- function(bxf, max) {
  r0 <- integrate(bxf, 0, max, subdivisions = 10000)$value
  return(list(est = r0))
}

# For discrete distributions.
# b is average number of secondaries per primary at time t
# f is function for probability of survival until time t
disc_surv_func <- function(b, f) {
  r0 <- sum(b * f)
  return(list(est = r0))
}

# Variance: resampling?
# Figure out sampling distributions?
# Would probably need to be created for each separate instance
# 
# 
# # Average number of infections is bell-shaped with peak at 5.
# # Infection time survival is exponential distribution.
# bxf_test <- function(t) {return(dexp(t / 2) * dnorm(t, 5, 3) * 10)}
# cont_surv_func(bxf_test, 100)
# 
# # Discretized test
# # Number of people infected at every time is randomly normally distributed
# # Infection time survival is discretized exponential distribution.
# b_dtest <- rnorm(11, 0.6, 0.1)
# f_dtest <- dexp(0:10)
# disc_surv_func(b_dtest, f_dtest)
