### Other compartment models with Gauss Newton OLS?

# inf_data = infected counts
# start_param = initialization parameters
# start_pop = starting compartment sizes
# t = time steps associated with the infected data
# b is the birth rate per 1 person in each time step
# Returns R0 estimate and standard deviation
comp_gls <- function(inf_data, N, times, start_parm = c("beta" = 0.06, "epsilon" = 0.05, "delta" = 0.1, "b" = 0.00000001, "gamma" = 0.1), 
                     start_pop = c("M" = 0, "E" = 0, "I" = 50, "R" = 0), k = 100, ndeps = 0.0000000000003) {
  # Solve for the model parameters
  # Compartment sizes should be scaled to be proportion of population
  library(EpiModel)
  library(numDeriv)
  # MSEIR Model
  meir <- function(t, t0, parms) {
    with(as.list(c(t0, parms)), {    
      
      #uses B, sigma, gamma
      ## Differential Equations
      dm <- birth*(e + ii + rr) - delta * m
      de <- beta * ii * (1 - m - e - ii - rr) - (epsilon + birth) * e
      dii <- epsilon * e - (gamma + birth) * ii
      drr <- gamma * ii - birth * rr
      
      ## Output
      list(c(dm, de, dii, drr))
    })
  }
  
  init <- init.dcm(m = start_pop["M"] / N, e = start_pop["E"] / N, 
                   ii = start_pop["I"] / N, rr = start_pop["R"] / N)
  cont <- control.dcm(nsteps = max(times) + 1, dt = 1, new.mod = meir)
  
  # Standardize
  inf_data <- inf_data / N
  
  # SSE objective function to minimize
  obj_func <- function(data, estimate, times) {
    return(sum((data - estimate[times])^2))
  }
  
  # Compare model to data, return weighted SSE
  comp <- function(param, initdcm, contdcm, weights, data, times) {
    paramdcm <- param.dcm(beta = param["beta"], epsilon = param["epsilon"], delta = param["delta"],
                          birth = param["b"], gamma = param["gamma"])
    model1 <- dcm(paramdcm, initdcm, contdcm)
    inc_est <- model1$epi$ii[,1][-1]
    return(obj_func(data, inc_est, times))
  }
  
  new_mod <- constrOptim(start_parm, f = comp, grad = NULL,
                         ui = rbind(c(1, 0, 0, 0, 0), c(0, 1, 0, 0, 0), c(0, 0, 1, 0, 0), 
                                    c(0, 0, 0, 1, 0), c(0, 0, 0, 0, 1)), ci = c(0, 0, 0, 0, 0),
                         initdcm = init, contdcm = cont, data = inf_data, times = times,
                         control = list(maxit = 40, ndeps = rep(ndeps, length(start_parm))))
#   new_mod <- optim(par = start_parm, fn = comp, hessian = TRUE,
#                          initdcm = init, contdcm = cont, data = inf_data, times = times,
#                          control = list(maxit = 200, ndeps = rep(ndeps, length(start_parm))))
#   
  beta_est <- new_mod$par["beta"]
  epsilon_est <- new_mod$par["epsilon"]
  gamma_est <- new_mod$par["gamma"]
  b_est <- new_mod$par["b"]
  delta_est <- new_mod$par["delta"]
  r0 <- beta_est * epsilon_est / 
    ((gamma_est + b_est) * (epsilon_est + b_est))
  dh <- c(epsilon_est ^ 2 / ((beta_est + epsilon_est)^2 * (gamma_est + b_est)),
          beta_est ^ 2 / ((beta_est + epsilon_est)^2 * (gamma_est + b_est)), 
          0,
          -(beta_est * epsilon_est)/((beta_est + epsilon_est) * (gamma_est + b_est) ^ 2),
          -(beta_est * epsilon_est)/((beta_est + epsilon_est) * (gamma_est + b_est) ^ 2))
  r0_hessian <- optimHess(new_mod$par, comp,
                          initdcm = init, contdcm = cont, data = inf_data, times = times,
                          control = list(maxit = 50, ndeps = rep(ndeps, length(start_parm))))
  r0_sd <- try(sqrt(abs(t(dh) %*% solve(r0_hessian) %*% dh)))
  if(class(r0_sd) == "try-error") {return(list(est = r0, sd = NA))}
  return(list(est = r0, sd = r0_sd))
  
#   meir_ODE <- function(X0, A0 , Z0, Y0, par1, par2, par3, par4, par5,
#                        t0, T, step = .1, inner_fxn = meir_re_inner,
#                        do_plot = FALSE){
#     
#     ## Initialize params for integration
#     params <- c(par1, par2, par3, par4, par5)
#     state <- c(X = X0, A = A0, Z = Z0, Y = Y0)
#     names(state) <- c("X", "A", "Z", "Y")
#     times <- seq(t0, T, by = step)
#     print(state)
#     ## use ODE to integrate and get the simulation
#     ode_results <- deSolve::ode(state, times, func = inner_fxn, parms = params,
#                                 maxsteps = 50000, method = "rk4")
#     ## Plot results
#     if(do_plot){
#       plot(1, 1, type = "n", xlim = range(ode_results[, 1]), ylim = range(ode_results[, 2:3]),
#            xlab = "Time", ylab = "# of Individuals",
#            main = paste("SIR curve \n par1 =", par1, "; par2=", par2))
#       lines(ode_results[, 1], ode_results[, 2], col = "blue", lwd = 2)
#       lines(ode_results[, 1], ode_results[, 3], col = "red", lwd = 2)
#       legend("topright", c("# Susceptible", "# Infected"), col = c(4, 2), lwd = 2)
#     }
#     return(ode_results)
#   }
#   
#   #' Making the ODE function for deSolve reparametrizing
#   #'
#   #' @param t a vector of times.  Should be strictly monotonically increasing.
#   #' @param state the initial conditions, a vector.
#   #' @param params the parameters, here r0 and gamma
#   #' @return the function used in ode
#   meir_re_inner <- function(t, state, params){
#     with(as.list(c(state, params)), {
#       dX <- params[4] * (A + Z + Y) - params[3] * X
#       dA <- params[1] * Z * (1 - X - A - Z - Y) - (params[2] + params[4]) * A
#       dZ <- params[2] * A - (params[5] + params[4]) * Z
#       dY <- params[5] * Z - params[4] * Y
#       list(c(dX, dY, dZ, dA))
#     })
#   }
#   
#   sensitivity_analysis_meir <- function(new_par, T, time,  X0, 
#                                         A0, Z0, Y0, inner_fxn = meir_re_inner, delta = .001,
#                                         do_plot = FALSE){
#     par1 <- new_par$par["beta"] 
#     par2 <- new_par$par["epsilon"]
#     par3 <- new_par$par["delta"]
#     par4 <- new_par$par["b"]
#     par5 <- new_par$par["gamma"]
#     out <- meir_ODE(X0, A0, Z0, Y0, par1 = par1, par2 = par2, par3 = par3, par4 = par4, par5 = par5, 
#                     t0 = 0, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
#     out_par1 <- meir_ODE(X0, A0, Z0, Y0, par1 = par1 + delta, par2 = par2, par3 = par3, par4 = par4, par5 = par5, 
#                          t0 = 0, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
#     out_par2 <- meir_ODE(X0, A0, Z0, Y0, par1 = par1, par2 = par2 + delta, par3 = par3, par4 = par4, par5 = par5, 
#                          t0 = 0, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
#     out_par3 <- meir_ODE(X0, A0, Z0, Y0, par1 = par1, par2 = par2, par3 = par3 + delta, par4 = par4, par5 = par5, 
#                          t0 = 0, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
#     out_par4 <- meir_ODE(X0, A0, Z0, Y0, par1 = par1, par2 = par2, par3 = par3, par4 = par4 + delta, par5 = par5, 
#                          t0 = 0, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
#     out_par5 <- meir_ODE(X0, A0, Z0, Y0, par1 = par1, par2 = par2, par3 = par3, par4 = par4, par5 = par5 + delta, 
#                          t0 = 0, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
#     Ypar1 <- (out_par1[, 4] - out[, 4]) / delta
#     Ypar2 <- (out_par2[, 4] - out[, 4]) / delta
#     Ypar3 <- (out_par3[, 4] - out[, 4]) / delta
#     Ypar4 <- (out_par4[, 4] - out[, 4]) / delta
#     Ypar5 <- (out_par5[, 4] - out[, 4]) / delta
#     out <- data.frame(out)
#     out$Ypar1 <- Ypar1
#     out$Ypar2 <- Ypar2
#     out$Ypar3 <- Ypar3
#     out$Ypar4 <- Ypar4
#     out$Ypar5 <- Ypar5
#     out <- out[out$time %in% time,]
#     chi <- as.matrix(out[, c("Ypar1", "Ypar2", "Ypar3", "Ypar4", "Ypar5")])
#     sigma2 <- 1 / (nrow(df) - length(new_par$par)) * new_par$value
#     new_var <- sigma2 * solve(t(chi) %*% chi)
#     return(new_var)
#   }
#   
#   par_var <- sensitivity_analysis_meir(new_mod, max(times), times, start_pop["M"] / N, 
#                                        start_pop["E"] / N, start_pop["I"] / N, start_pop["R"] / N)
#   
#   # Find variance of R0 from sampling MV normal 
#   library(MASS)
#   mv_samples <- mvrnorm(k, new_mod$par, par_var)
#   r0_samples <- mv_samples[,1] * mv_samples[,2] / 
#     ((mv_samples[,5] + mv_samples[,4]) * (mv_samples[,2] + mv_samples[,4]))
#   
#   return(list(R0 = mean(r0_samples), std_err = sd(r0_samples)))
}



# tests <- comp_gls(inf_testdat, N = 100000, times = 1:365)

# Find variance of parameter estimates with inverse Hessian