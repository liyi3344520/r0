# Time dependent reproduction number

# w is the GTD probabilities; should have one entry for each interval
# Assume we know starting comp sizes, have data for every evenly spaced time step.
# Assumes everyone has equal prob of getting infected at time i from case at time j.
# Assumes transmission takes at least one time step.
td_rep <- function(start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0),
                   sus_data, inf_data, w) {
  
  # Find incidence data
  incidence_dat <- -diff(c(start_pop["S"], sus_data))
  incidence_dat <- ifelse(incidence_dat <= 0, 0, incidence_dat)
  max_days <- length(w)
  days <- c(1:length(w))
  
  # Find R0
  # Sum over all pi0.
  # Assume GTD is lower bounded by 0.
  pij <- rep(NA, length(days))
  
  # Every time of onset
  for(ii in (2:(max_days + 1))) {
    if (round(incidence_dat[ii]) == 0) {
      next
    }
    plj <- rep(NA, round(incidence_dat[ii]))
    
    # Every person at that time
    for(ll in (1:round(incidence_dat[ii]))){
      nom <- round(incidence_dat[ii]) * w[ii] 
      gtd <- rep(0, max_days)
      gtd[1:ii] <- w[ii:1]
      denom <- 0
      for (kk in 1:(ii-1)) {
        denom <- denom + incidence_dat[ii - kk] * gtd[kk]
      }
      plj[ll] <- nom / denom
    }
    plj <- sum(plj, na.rm = TRUE)
    pij[ii] <- plj
  }
  r0 <- sum(pij, na.rm = TRUE) / max(1, incidence_dat[1])
  
  return(list(est = r0))
}

# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# w_test <- c(rep(0, 5), pgamma(1:14, 3, 1)) / sum(pnorm(1:14, 3, 1))
# td_rep(dat[, 3], c("S" = 99950, "E" = 0, "I" = 50, "R" = 0),
#        w_test)