### Direct parameter estimation 

# Find R from an equation involving the serial interval and mean latent period
## Serial interval = latent + infection period
## Assumes exponential rate of growth and known serial interval/mean latent period
## v is the mean latent period + mean infectious period
## l is the mean latent period
## Only works if data starts from single first case
exp_dpe <- function(cases, prop_suscept, t, v, l) {
  lambda = log(cases - 1) / t
  f = l/v
  R = 1 + lambda * v + f * (1 - f) * (lambda * v) ^ 2
  r0 = R / prop_suscept
  
  # The variance estimation makes me want to cry
  
  return(list(est = r0))
}

# This other direct parameter estimation thing
## k is number of contacts per unit of time
## b is the probability of transmission between a contact of I and S
## D = mean duration of infectiousness
plug_chug <- function(k, b, D) {return(list(est = k * b * D))}

# l_test <- 7
# v_test <- 14
# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# dat[,2:4] <- dat[,2:4] / 50
# exp_dpe(dat[100,"I"], dat[100,"S"] / 2000, 100, v_test, l_test)

# plug_chug(2, 0.1, 7)
