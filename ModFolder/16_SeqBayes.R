# Sequential Bayes Method

# For now, we assume that the prior is Gamma
# Likelihood is known Poisson
# Posterior is then gamma (conjugate priors)
# Need an assumption for gam parameters (avg duration of infectious period)
# Assume data at every time step of equal interval
# Returns R0 estimate and standard deviation
# inv_inf is the assumed inverse of mean infectivity period
seq_bayes_gamma <- function(start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0), 
                            sus_data, inf_data, inv_inf,
                            gamma_prior = c("alpha" = 1, "beta" = 1)) {
  
  # Find incidence data
  incidence_dat <- -diff(c(start_pop["S"], sus_data))
  incidence_dat <- ifelse(incidence_dat <= 0.1, 0.1, incidence_dat)
  alpha_par <- gamma_prior["alpha"]
  beta_par <- gamma_prior["beta"]
  for(ii in 2:length(incidence_dat)) {
    alpha_par <- log(incidence_dat[ii] / incidence_dat[ii - 1]) / inv_inf + 1 + alpha_par
    beta_par <- beta_par + 1
  }
  r0 <- alpha_par / beta_par
  r0_sd <- sqrt(alpha_par / beta_par ^ 2)
  return(list(est = r0, sd = r0_sd))
}

# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# seq_bayes_gamma(dat[, 3], c("S" = 99950, "E" = 0, "I" = 50, "R" = 0), inv_inf = 1/7)