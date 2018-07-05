# Find the exponential growth rate, then use MGF on the GTD
# Variance by resampling from sampling dist of hat(r)

# Assume we know starting comp sizes, have data for every evenly spaced time step.
# w is the GTD probabilities; should have one entry for each interval
# discrete_steps if for some reason the intervals are not defined from 0 
## or if there are skips
# k is number of times to resample for std. dev. estimates
# Returns R0 estimate and standard deviation
exp_growth <- function(start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0), 
                       sus_data, inf_data, w, discrete_steps = c(0:(length(w) - 1)),
                       k = 100) {
  
  # Find incidence data
  incidence_dat <- -diff(c(start_pop["S"], sus_data))
  incidence_dat <- ifelse(incidence_dat <= 0, 0, incidence_dat)
  days <- c(1:length(incidence_dat))
  
  # Fit Poisson regression
  model1 <- glm(incidence_dat ~ days - 1, family = "poisson")
    
  # Find expected value
  r <- coef(summary(model1))[1, 1]
  r_se <- coef(summary(model1))[1, 2]

  # MGF
  Mt <- sum(w * exp(-r * (discrete_steps)))
  r0 <- 1 / Mt
  
  dh <- -1 / (Mt ^ 2) * sum(-discrete_steps * w * exp(-discrete_steps * r))
  
  
  r0_sd <- dh * r_se
  
  return(list(est = r0, sd = r0_sd))
}

# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# p_test <- pgamma(1:14, 3, 1) / sum(pnorm(1:14, 3, 1))
# exp_growth(dat[, 3], c("S" = 99950, "E" = 0, "I" = 50, "R" = 0),
#            p_test)