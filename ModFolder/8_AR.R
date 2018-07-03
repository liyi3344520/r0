### Attack rate

# Attack rate is the proportion of people eventually infected
# start_pop is the starting compartment sizes. 
# sus_prop is the initial proportion of susceptible
R0_attack_rate <- function(AR, start_pop) {
  sus_prop <- start_pop["S"] / sum(start_pop)
  r0 <- -log((1 - AR) / sus_prop) / (AR - (1 - sus_prop))
  dh <- (log((1 - AR) / sus_prop) - (AR + sus_prop - 1)/(AR - 1)) /
    (AR + sus_prop - 1) ^ 2
  r0_sd <- sqrt(dh ^ 2 * (AR * (1 - AR) / sum(start_pop)))
  return(list(est = r0, sd = r0_sd))
}

# Does this even have a model variance? 

# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# sus_prop_test <- 99950 / 100000
# ar_test <- (99950 - 0 - dat[nrow(dat), 2]) / sum(dat[nrow(dat), -1])
# R0_attack_rate(ar_test, sus_prop_test)