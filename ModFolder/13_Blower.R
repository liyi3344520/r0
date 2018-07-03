### Constant term of characteristic equation

# inf_data is the infeacted prevalence
# Starting parameters for the model: 
## beta are transmission rates for E and I compartment
## theta is rate of cured people who become susceptible again
## alpha is inverse latent period
## mu are the death rates. 0 is for S and R, e for E, i for I
## c is cure rate of infected
# start_pop are starting compartment sizes
seir_blower <- function(start_parm = c("beta_e" = 0.000000006, "beta_i" = 0.000000006,
                                                 "theta" = 0, "alpha" = 0.1,
                                                 "mu_0" = 0.000000005, "mu_e" = 0.000000005, 
                                                 "mu_i" = 0.000000005, "c" = 0),
                     start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0), inf_data, days) {
  library("EpiModel")
  
  SEIR <- function(t, t0, parms) {
    with(as.list(c(t0, parms)), {    
      
      #uses B, sigma, gamma
      ## Differential Equations
      dS <- -((beta_e + beta_i)* I + mu_0)*S + theta * c * I
      dE <- - (alpha + mu_e) * E + beta_e * S * I
      dI <- beta_i * S * I + alpha * E - (mu_i + c) * I
      dR <- c * (1 - theta) * I - mu_0 * R
      
      ## Output
      list(c(dS, dE, dI, dR))
    })
    
  }
  
  init <- init.dcm(S = start_pop["S"], E = start_pop["E"], 
                   I = start_pop["I"], R = start_pop["R"])
  cont <- control.dcm(nsteps = max(days) + 1, dt = 1, new.mod = SEIR)
  
  obj_func <- function(data, estimate, days) {
    return(sum((data - estimate[days])^2))
  }
  
  comp <- function(param, initdcm, contdcm, data) {
    paramdcm <- param.dcm(beta_e = param["beta_e"], beta_i = param["beta_i"],
                          theta = param["theta"], alpha = param["alpha"],
                          mu_0 = param["mu_0"], mu_e = param["mu_e"], 
                          mu_i = param["mu_i"], c = param["c"])
    model1 <- dcm(paramdcm, initdcm, contdcm)
    inc_est <- model1$epi$I[,1][-1]
    return(obj_func(data, inc_est))
  }
  
  ### Ndeps makes things solveable.
  new_par <- optim(start_parm, comp, initdcm = init, 
                  contdcm = cont, data = inf_data, hessian = TRUE,
                  control = list(maxit = 200, ndeps = rep(100000 ^ -1, 8)))
  
  r0 <- start_pop["S"] * ((new_par$par["beta_e"] + new_par$par["beta_i"]) * new_par$par["alpha"] + 
                            new_par$par["beta_i"] * new_par$par["mu_e"]) / 
    ((new_par$par["alpha"] + new_par$par["mu_e"]) * (new_par$par["c"] + new_par$par["mu_i"]))
  
  
  deriv_be <- new_par$par["alpha"] * start_pop["S"] / 
    ((new_par$par["alpha"] * new_par$par["mu_e"]) * 
       (new_par$par["c"] * new_par$par["mu_i"]))
  
  deriv_bi <- start_pop["S"] / ((new_par$par["c"]) * (new_par$par["mu_i"]))
  
  deriv_alpha <- new_par$par["beta_e"] * start_pop["S"] * new_par$par["mu_e"] / 
    ((new_par$par["alpha"] + new_par$par["mu_e"]) ^ 2 * 
       (new_par$par["c"] * new_par$par["mu_i"]))
  
  deriv_mue <- - new_par$par["alpha"] * start_pop["S"] * new_par$par["mu_e"] / 
    ((new_par$par["alpha"] + new_par$par["mu_e"]) ^ 2 * 
       (new_par$par["c"] * new_par$par["mu_i"]))
  
  deriv_mui <- -start_pop["S"] * ((new_par$par["beta_e"] + new_par$par["beta_i"]) * new_par$par["alpha"] + 
                                    new_par$par["beta_i"] * new_par$par["mu_e"]) /
    ((new_par$par["alpha"] + new_par$par["mu_e"]) * 
       (new_par$par["c"] * new_par$par["mu_i"]) ^ 2)
  
  deriv_mu0 <- 0
  
  deriv_theta <- 0
  
  deriv_c <- -start_pop["S"] * ((new_par$par["beta_e"] + new_par$par["beta_i"]) * new_par$par["alpha"] + 
                                  new_par$par["beta_i"] * new_par$par["mu_e"]) /
    ((new_par$par["alpha"] + new_par$par["mu_e"]) * 
       (new_par$par["c"] * new_par$par["mu_i"]) ^ 2)
  
  dh <- c(deriv_be, deriv_bi, deriv_theta, deriv_alpha, deriv_mu0, deriv_mue, deriv_mui, deriv_c)
  
  r0sd <- tryCatch(sqrt(t(dh) %*% solve(new_par$hessian) %*% dh), error = function(e) NA)
  
  return(list(est = r0, sd = r0sd))
}

# dat <- read.csv("https://raw.githubusercontent.com/atzechang/data/master/Baseline/Baseline1_99950_10_1/Baseline1Norm.csv")[,-1]
# seir_blower(inf_data = dat[, 3], days = dat[, 1])