# Done for the SEIR model.

# Infected data should be a vector of counts that start on day 1. 
# Assume we know starting comp sizes, have data for every evenly spaced time step.
# start_parm should be a named vector
# k is the number of samples to do when calculating standard deviation.
# Returns R0 estimate and standard deviation
seir_ng <- function(start_parm = c("beta" = 0.06, "gamma" = 0.05, "alpha" = 0.1),
                    start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0), sus_data, inf_data, 
                    k = 100) {
  library(EpiModel)
  # SEIR Model
  SEIR <- function(t, t0, parms) {
    with(as.list(c(t0, parms)), {    
      
      num <- S + E + I + R
      
      #uses B, sigma, gamma
      ## Differential Equations
      dS <- -beta*S*(I)/num
      dE <- beta*I*S/num - (alpha)*E
      dI <- alpha*E - (gamma)*I
      dR <- gamma*I
      
      ## Output
      list(c(dS, dE, dI, dR))
    })
  }
  
  # Parameters for EpiModel
  init <- init.dcm(S = start_pop["S"], E = start_pop["E"], 
                   I = start_pop["I"], R = start_pop["R"])
  cont <- control.dcm(nsteps = length(inf_data) + 1, dt = 1, new.mod = SEIR)
  # Take prevalence data, convert to incidence for the time step. 
  incidence_dat <- -diff(c(start_pop["S"], sus_data))
  
  
  # Weighted SSE bjective function to minimize
  obj_func <- function(data, estimate) {
    return(sum((data - estimate)^2))
  }
  # Compare model to data, return weighted SSE
  comp <- function(param, initdcm, contdcm, data) {
    paramdcm <- param.dcm(beta = param["beta"], gamma = param["gamma"], alpha = param["alpha"])
    model1 <- dcm(paramdcm, initdcm, contdcm)
    inc_est <- -diff(model1$epi$S[,1])
    return(obj_func(data, inc_est))
  }
  
  
  # Parameter estimation with OLS.
  new_par <- constrOptim(start_parm, comp, grad = NULL,
                         ui = rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)),
                         ci = c(0, 0, 0), initdcm = init, contdcm = cont,
                         data = incidence_dat, control = list(maxit = 25))
  
  # Find r0 using FV(-1)
  F_mat <- matrix(c(0, new_par$par["beta"], 0, 0),
                  nrow = 2, byrow = TRUE)
  V_mat <- matrix(c(new_par$par["alpha"], 0, -new_par$par["alpha"], new_par$par["gamma"]),
                  nrow = 2, byrow = TRUE)
  inv_Vmat <- solve(V_mat)
  eigenvals <- eigen(F_mat %*% inv_Vmat)$values
  r0 <- max(abs(eigenvals))
  
  # Variance estimation of parameters using sensitivity analysis
  
  SEIR_ODE <- function(X0, Y0 , Z0, A0, par1, par2, par3,
                       t0, T, step = .1, inner_fxn = SEIR_re_inner,
                       do_plot = FALSE){
    
    ## Initialize params for integration
    params <- c(par1, par2, par3)
    state <- c(X = X0, Y = Y0, Z = Z0, A = A0)
    times <- seq(t0, T, by = step)
    
    ## use ODE to integrate and get the simulation
    ode_results <- deSolve::ode(state, times, func = inner_fxn, parms = params,
                                maxsteps = 50000, method = "rk4")
    ## Plot results
    if(do_plot){
      plot(1, 1, type = "n", xlim = range(ode_results[, 1]), ylim = range(ode_results[, 2:3]),
           xlab = "Time", ylab = "# of Individuals",
           main = paste("SIR curve \n par1 =", par1, "; par2=", par2))
      lines(ode_results[, 1], ode_results[, 2], col = "blue", lwd = 2)
      lines(ode_results[, 1], ode_results[, 3], col = "red", lwd = 2)
      legend("topright", c("# Susceptible", "# Infected"), col = c(4, 2), lwd = 2)
    }
    return(ode_results)
  }
  
  #' Making the ODE function for deSolve reparametrizing
  #'
  #' @param t a vector of times.  Should be strictly monotonically increasing.
  #' @param state the initial conditions, a vector.
  #' @param params the parameters, here r0 and gamma
  #' @return the function used in ode
  SEIR_re_inner <- function(t, state, params){
    N <- sum(state)
    with(as.list(c(state, params)), {
      dX <- - params[1] * Y * X / N
      dA <- params[1] * Y * X / N - params[3] * A
      dZ <- params[2]* Y
      dY <- -dX - dZ - dA
      list(c(dX, dY, dZ, dA))
    })
  }
  
  sensitivity_analysis_SEIR <- function(new_par, T, t,  X0, 
                                        Y0, Z0, A0, inner_fxn = SEIR_re_inner, delta = .001,
                                        do_plot = FALSE){
    par1 <- new_par$par["beta"] 
    par2 <- new_par$par["gamma"]
    par3 <- new_par$par["alpha"]
    out <- SEIR_ODE(X0, Y0, Z0, A0, par1 = par1, par2 = par2, par3 = par3, 
                    t0 = 0, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
    out_par1 <- SEIR_ODE(X0, Y0, Z0, A0, par1 = par1 + delta, par2 = par2, par3 = par3,
                         t0 = 0, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
    out_par2 <- SEIR_ODE(X0, Y0, Z0, A0, par1 = par1, par2 = par2 + delta, par3 = par3,
                         t0 = 0, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
    out_par3 <- SEIR_ODE(X0, Y0, Z0, A0, par1 = par1, par2 = par2, par3 = par3 + delta,
                         t0 = 0, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
    Ypar1 <- (out_par1[, 4] - out[, 4]) / delta
    Ypar2 <- (out_par2[, 4] - out[, 4]) / delta
    Ypar3 <- (out_par3[, 4] - out[, 4]) / delta
    out <- data.frame(out)
    out$Ypar1 <- Ypar1
    out$Ypar2 <- Ypar2
    out$Ypar3 <- Ypar3
    out <- out[out$time %in% t,]
    chi <- as.matrix(out[, c("Ypar1", "Ypar2", "Ypar3")])
    sigma2 <- 1 / (length(inf_data) - length(new_par$par)) * new_par$value
    new_var <- sigma2 * solve(t(chi) %*% chi)
    return(new_var)
  }
  par_var <- sensitivity_analysis_SEIR(new_par, length(inf_data), 1:length(inf_data), sum(start_pop) - inf_data[1], 
                                       inf_data[1], 0, 0)
  
  # Get samples of parameters 
  library(MASS)
  mv_samples <- mvrnorm(k, new_par$par, par_var)
  beta_samples <- mv_samples[, 1]
  gamma_samples <- mv_samples[, 2]
  alpha_samples <- mv_samples[, 3]
  
  # Recalculate R0 from samples, get standard deviation
  r0_samples <- rep(NA, k)
  for(ii in (1:k)) {
    F_mat_new <- matrix(c(0, beta_samples[ii], 0, 0),
                    nrow = 2, byrow = TRUE)
    V_mat_new <- matrix(c(alpha_samples[ii], 0, -alpha_samples[ii], gamma_samples[ii]),
                    nrow = 2, byrow = TRUE)
    inv_Vmat_new <- solve(V_mat_new)
    eigenvals_new <- eigen(F_mat_new %*% inv_Vmat_new)$values
    r0_samples[ii] <- max(abs(eigenvals_new))
  }

  
  return(list(est = r0, sd = sd(r0_samples)))
  
}

# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# seir_ng(dat[, 3], c("beta" = 0.06, "gamma" = 0.05, "alpha" = 0.14),
#         start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0))