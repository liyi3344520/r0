### Two estimation methods

# Initial growth rate and duration of infection
## Variance through ???

init_growth <- function(start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0), dur, 
                        sus_data, inf_data, k = 100){
  # Find incidence data
  incidence_dat <- -diff(c(start_pop["S"], sus_data))
  incidence_dat <- ifelse(incidence_dat <= 0, 0, incidence_dat)
  

  # Divide incidence at each time point by starting incidence
  days <- c(1:length(incidence_dat))
  
  # Fit Poisson regression to find exp growth rate
  model1 <- glm(incidence_dat ~ days - 1, family = "poisson")
  r <- coef(summary(model1))[1, 1]
  r_se <- coef(summary(model1))[1, 2]

  # Find doubling time
  doub_time <- log(2) / log(1 + r)
  # Find r0
  r0 <- 1 + dur * log(2) / doub_time
  
  dh <- 1 + dur / (1 + r)
  # Calculate variance
  r0_sd <- dh ^ 2 * r_se
  return(list(est = r0, sd = r0_sd))
}

# Proportion of susceptibles after epidemic ends
final_size <- function(mu_inf){
  r0 <- (1 - mu_inf) ^ (-1) * log(mu_inf ^ (-1))
  dh <- (mu_inf + mu_inf * log(1 / mu_inf) - 1) / 
    ((1 - mu_inf) ^ 2 * mu_inf)
  r0_sd <- sqrt(dh ^ 2 * (mu_inf * (1 - mu_inf) / sum(start_pop)))
  return(list(est = r0, sd = r0_sd))
}

#Test
# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# init_growth(14, dat[, 3], 100, c("S" = 99950, "E" = 0, "I" = 50, "R" = 0))
# final_size(0.95)