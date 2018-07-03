### Estimation of R0 through modeling it as a branching process.
# Variance?

# Assume data are taken periodically
# inf_data are the prevalence counts at each equidistant time step.
# init_pop is the starting compartment sizes
# p is the vector of probabilities that someone at time t infects someone at
## time t + j. Maximum length of k. [Generation time distribution]
# times can be defined if for some reason the prob dist is not defined from 0 
## or if there are skips in time steps
likelihood_branching <- function(start_pop = c("S" = 99950, "I" = 50, "R" = 0), 
                                 sus_data, inf_data, p,
                                 times = c(0:(length(p) - 1))) {
  prob <- matrix(c(times, p), nrow = 2, byrow = TRUE)
  incidence_dat <- -diff(c(start_pop["S"], sus_data))
  num <- sum(incidence_dat, na.rm = TRUE)
  denom_vec <- rep(NA, length(incidence_dat))
  for (ii in 1:length(denom_vec)) {
    pnij_vec <- rep(NA, ncol(prob))
    for (jj in 1:ncol(prob)) {
      if (ii - prob[1, jj] > 0) {
        pij <- prob[2, jj]
        pnij <- pij * incidence_dat[ii - prob[1, jj]]
        pnij_vec[jj] <- pnij
      }
    }
    denom_vec[ii] <- sum(pnij_vec, na.rm = TRUE)
  }
  denom <- sum(denom_vec, na.rm = TRUE)
  r0 <- num/denom
  r0_sd <- r0 ^ 2 * 1 / sum(incidence_dat) 
  return(list(est = r0, sd = r0_sd))
}

# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# p_test <- pgamma(1:14, 3, 1) / sum(pnorm(1:14, 3, 1))
# likelihood_branching(dat[, 3], p_test, c("S" = 99950, "I" = 50, "R" = 0))