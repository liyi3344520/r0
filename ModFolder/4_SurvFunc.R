### Directly estimated survival function
# Variance?

# start_pop is the starting compartment sizes
# inf_dens should be the infectivity of an individual at the range of 
## possible infectivity times. 
discrete_dsf <- function(start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0),
                             inf_dens) {
  sus_prop <- start_pop["S"] / sum(start_pop)
  r0 <- sus_prop * sum(inf_dens)
  return(list(est = r0))
}

cont_dsf <- function(start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0),
                     inf_dens) {
  sus_prop <- start_pop["S"] / sum(start_pop)
  r0 <- sus_prop * integrate(inf_dens, 0, Inf)$value
  return(list(est = r0))
}

# # Continuous test
# # Infectiousness is exponentially distributed.
# id_test <- function(t) {return(dexp(t / 1.5))}
# cont_dsf(c("S" = 9900, "I" = 100), id_test)
# 
# # Discretized test
# # Infectiousness is beta distributed
# id_test <- rbeta(11, 0.2, 1)
# discrete_dsf(c("S" = 9900, "I" = 100), id_test)