### MLE estimation of secondary infections
# Variance through Fisher information 

# Assumes Poisson likelihood for # of secondary infections 
## caused by one primary infection
# Must be during an exponential growth period
# Assumes periodic incidence with equal interval time steps
# w is the GTD probabilities; should have one entry for each interval
# times if for some reason the intervals are not defined from 0 
## or if there are skips
# Returns R0 estimate and standard deviation

mle_secondary <- function(start_pop = c("S" = 99950, "I" = 50, "R" = 0), 
                          sus_data, inf_data, w,
                          times = c(0:(length(w) - 1))) {
  prob <- matrix(c(times, w), nrow = 2, byrow = TRUE)
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
  denom <- sum(denom_vec)
  r0 <- num/denom
  r0_sd <- r0 ^ 2 * 1 / sum(incidence_dat) 
  return(list(est = r0, sd = r0_sd))
}

# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# w_test <- pgamma(1:14, 3, 1) / sum(pnorm(1:14, 3, 1))
# mle_secondary(dat[1:100, 3], w_test, c("S" = 99950, "I" = 50, "R" = 0))