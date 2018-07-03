### SEIR Model with GLS

# Need to source files
#setwd("~/R0/Models")

# Infected data should be a vector of counts that start on day 1. 
# rho is a parameter for the weights for the GLS algorithm
# Assume we know starting comp sizes, have data for every evenly spaced time step.
# start_parm should be a named vector
# k is the number of samples to do when calculating standard deviation.
# stop_par is the stopping threshhold for the algorithm
# Returns R0 estimate and standard deviation
seir_gls <- function(start_parm = c("beta" = 0.06, "gamma" = 0.05, "alpha" = 0.1),
                     start_pop = c("S" = 99950, "E" = 0, "I" = 50, "R" = 0), sus_data, inf_data,
                     rho = 1, stop_par = 0.0001, k = 100, ndeps = 0.0000001) {
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
  obj_func <- function(weights, data, estimate) {
    return(sum(weights * (data - estimate)^2))
  }
  
  # Compare model to data, return weighted SSE
  comp <- function(param, initdcm, contdcm, weights, data) {
    paramdcm <- param.dcm(beta = param["beta"], gamma = param["gamma"], alpha = param["alpha"])
    model1 <- dcm(paramdcm, initdcm, contdcm)
    inc_est <- -diff(model1$epi$S[,1])
    return(obj_func(weights, data, inc_est))
  }
  
  
  # Step 1: Parameter estimation with OLS.
  w <- rep(1, length(incidence_dat))
#   new_par <- constrOptim(start_parm, comp, grad = NULL,
#                          ui = rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)),
#                          ci = c(0, 0, 0), initdcm = init, contdcm = cont,
#                          weights = w, data = incidence_dat, 
#                          control = list(maxit = 100, ndeps = rep(ndeps, length(start_parm))))
    new_par <- optim(start_parm, comp, initdcm = init, contdcm = cont,
                           weights = w, data = incidence_dat, 
                           control = list(maxit = 50, ndeps = rep(ndeps, length(start_parm))))
  
  # Repeat 2-3 until change in estimates between iterations is small
  delta <-  Inf
  while(delta > stop_par) {
    old_par <- new_par$par
    incidence_est <- diff(dcm(param.dcm(beta = old_par["beta"], 
                                        gamma = old_par["gamma"], 
                                        alpha = old_par["alpha"]), 
                         init, 
                         cont)$epi$I[,1])
    # Step 2: Calculate weights for next step of GLS
    w <- 1/(incidence_est^(2*rho))
    
    # Step 3: Find the least-squares minimizing set of parameters
#     new_par <- constrOptim(old_par, comp, grad = NULL,
#                            ui = rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)),
#                            ci = c(0, 0, 0), initdcm = init, contdcm = cont,
#                            weights = w, data = incidence_dat, control = list(maxit = 50))
        new_par <- optim(old_par, comp, initdcm = init, contdcm = cont,
                               weights = w, data = incidence_dat, control = list(maxit = 50))
    delta <- abs(old_par["beta"] - new_par$par["beta"]) + abs(old_par["gamma"] - new_par$par["gamma"])
  }
  
  #Variance estimation of parameters using sensitivity analysis
  
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
    sigma2 <- 1 / (length(t) - length(new_par$par)) * new_par$value
    new_var <- tryCatch(sigma2 * solve(t(chi) %*% chi), error = function(e) NA)
    return(new_var)
  }
  par_var <- try(sensitivity_analysis_SEIR(new_par, length(inf_data), 1:length(inf_data), sum(start_pop) - inf_data[1], 
                                       inf_data[1], 0, 0))
  
  if (class(par_var) == "try-error") {return(list(est = new_par$par["beta"] / new_par$par["gamma"], sd = NA))}
  # Find variance of R0 from sampling MV normal 
  library(MASS)
  mv_samples <- mvrnorm(k, new_par$par, par_var)
  r0_samples <- mv_samples[, 1] / mv_samples[, 2]
  return(list(est = mean(r0_samples), sd = sd(r0_samples)))
  
}


# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# seir_gls(dat[, 3], 1, c("beta" = 0.07, "gamma" = 0.04, "alpha" = 0.14),
#          c("S" = 99950, "E" = 0, "I" = 50, "R" = 0))
