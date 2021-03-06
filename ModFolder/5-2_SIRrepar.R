#Load package
library("EpiModel")
library(deSolve)

#Remember to change desired directory 

#####Arguments:
#init.param is a vector of the initial guess of the beta and gamma parameters. It needs to be a vector that has elements
###named "r0" and "gamma". All other elements will be ignored. 
#init.pop is a vector of the initial size of each compartment. It needs to be a vector that has elements named "S", "I", and 
###"R". All other elements will be ignored. 
#days is a vector of the day numbers of the data points. This allows for data without regular intervals between points.
###Must be a vector of numbers. 
#inf.data is the number of people in I for each day in the data. Must be a vector of numbers, same length as days.
#sus.data is the number of people in S for each day in the data. Must be a vector of numbers, same length as days.
#ndeps is for optim calculating the hessian

SIRrepar <- function(init.param, init.pop, days, inf.data, sus.data, rem.data,
                     ndeps = 0.00001, parscale_o = c(0.01, 0.01)) {
  #Get initial populations of each compartment S, I, R.
  init.sus <- init.pop["S"]
  init.inf <- init.pop["I"]
  init.rem <- init.pop["R"]
  
  #Get total number of people in population
  #Assume population stays constant except for deaths by disease
  N <-sum(init.pop)
  
  #Function for EpiModel for solving for first iteration with the linearization adjustment.
  
  #Regular SIR
  SIR <- function(t, t0, parms) {
    with(as.list(c(t0, parms)), {    
      num <- S + I + R
      ## Differential Equations
      dS <- -R0*gamma*S*I/num
      dI <- R0*gamma*S*I/num - gamma*I 
      dR <- gamma*I
      ## Output
      list(c(dS, dI, dR), 
           num = num,
           si.flow = R0*gamma*S*I/num,
           pr = I+R)
    })
  }
  
  
  #Find the SSE between the real data and the simulation from the model. 
  opt <- function(inf.model,inf.data, sus.model, sus.data, rem.model, rem.data, days) {
    err<- sum(mean((inf.model[days] - inf.data)^2) + mean((sus.model[days] - sus.data)^2) + mean((rem.model[days] - rem.data)^2))
    return(err)
  }
  
  
  #Function to be put in the function which finds the minimum SSE.
  model.sir <- function(params, initialdcm, controldcm, days, inf.data, sus.data, rem.data) {
    param1 <- param.dcm(R0 = params["r0"], gamma = params["gamma"])
    model1 <- dcm(param1,initialdcm,controldcm)
    sse <- opt(model1$epi$I[-1,1], inf.data, model1$epi$S[-1,1], sus.data, model1$epi$R[-1,1], rem.data, days)
    return(sse)
  }
  
  
  #Function to minimize the SSE between the data and the model. 
  sir.optim <- function(parm, initdcm, contdcm, inf.data, sus.data, rem.data, days) {
    par.sir <- optim(parm, model.sir, initialdcm = initdcm,
                     controldcm = contdcm,
                     days = days,
                     sus.data = sus.data,
                     inf.data = inf.data,
                     rem.data = rem.data,
                     hessian = T, control = list(ndeps = c(ndeps, ndeps)))
    return(c(par.sir))
  }
  
  sensitivity_analysis <- function(par1, par2, T, t,  X0 = 9500, 
                                   Y0 = 500, Z0 = 0, inner_fxn = SIR_inner, delta = .05,
                                   do_plot = FALSE){
    out <- SIR2(X0, Y0, Z0, par1 = par1, par2 = par2,
                t0 = 1, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
    out_par1 <- SIR2(X0, Y0, Z0, par1 = par1 + delta, par2 = par2,
                     t0 = 1, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
    out_par2 <- SIR2(X0, Y0, Z0, par1 = par1, par2 = par2 + delta,
                     t0 = 1, T = T, inner_fxn = inner_fxn, do_plot = FALSE)
    Ypar1 <- (out_par1[, 3] - out[, 3]) / delta
    Ypar2 <- (out_par2[, 3] - out[, 3]) / delta
    out <- data.frame(out)
    out$Ypar1 <- Ypar1
    out$Ypar2 <- Ypar2
    out <- out[out$time %in% t,]
    chi <- as.matrix(out[, c("Ypar1", "Ypar2")])
    sigma2 <- 1 / (length(days) - length(model_est$par)) * model_est$value
    new_var <- sigma2 * solve(t(chi) %*% chi)
    return(new_var)
  }
  
  SIR2 <- function(X0=950, Y0=50 , Z0=0, par1 = .6, par2 = .3,
                   t0=0, T=100, step=.1, inner_fxn = SIR_inner,
                   do_plot = TRUE){
    
    ## Initialize params for integration
    params <- c(par1, par2)
    state <- c(X = X0, Y = Y0, Z = Z0)
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
  
  SIR_re_inner <- function(t, state, params){
    N <- sum(state)
    with(as.list(c(state, params)), {
      dX <- - params[1] * params[2] * Y.I * X.S / N
      dZ <- params[2] * Y.I
      dY <- -dX - dZ
      list(c(dX, dY, dZ))
    })
  }
  
  #Solving as regular SIR.
  init.sir <- init.dcm(S = init.sus, I = init.inf, R  = init.rem)
  control.sir <- control.dcm(nsteps = max(days) + 1, dt = 1, new.mod = SIR)
  model_est <- sir.optim(init.param, init.sir, control.sir, inf.data, sus.data, rem.data, days)
  
  r0_est <- model_est$par["r0"]
  r0_hessian <- solve(model_est$hessian / model_est$value)
  r0_sd <- sqrt(abs(r0_hessian[1,1]))
  
  r0var <- sensitivity_analysis(model_est$par["r0"], model_est$par["gamma"], 
                                 T = max(days), t = days, X0 = init.sus, 
                                 Y0 = init.inf, Z0 = init.rem, inner_fxn = SIR_re_inner)
  r0sd2 <- sqrt(1 / r0var[1,1])
  
  return(list(est = r0_est, sd = r0_sd, sd2 = r0sd2, output = model_est))
}