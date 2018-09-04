require(plyr)
require(dplyr)

# Inputs:
### start_pop is the starting estimate for the number of people in each compartment
### sus_data is the data for total number of susceptible people.
### inf_data is the data for prevalence
### times is in case the data are not evenly spaced and we need time point labels
### Can add more parameters needed for calculation
## Assume total population stays the same
# Outputs:
### r0 is the R0 estimates
### r0_sd is the estimated standard error for the given estimate.
sir_likelihood <- function(start_pop, sus_data, inf_data,
                          times, starting_params = c(.5, .2),...) {

                                        # Stuff
    N <- sum(start_pop)
    data <- data.frame(tt = times, X1 = sus_data,
                       X2 = inf_data,
                       X3 = N - sus_data - inf_data)
    data$ll <- 1
    do_plug_in <- FALSE # Run the SIR CM each time as opposed to using observed values
    disease_list <- list(params = starting_params,
                         params_names = c("beta", "gamma"),
                         times = times,
                         init_vals = start_pop)

    optim_pars <- optim(par = starting_params,
                           fn = loglike_sir, data = data,
                           disease_list = disease_list,
                           do_plug_in = do_plug_in,
                        hessian = TRUE)
    
    r0_est <- optim_pars$par[1] / optim_pars$par[2]
    
    ## Using delta method and hessian
    r0_hessian <- optim_pars$hessian / optim_pars$value
    print(r0_hessian)
    dh <- c(1/optim_pars$par[2], - optim_pars$par[1] / (1/optim_pars$par[2]^2))
    r0_sd <- sqrt(t(dh) %*% solve(r0_hessian) %*% dh)
                         
  return(list(est = r0_est, sd = r0_sd, output = optim_pars$par))
}







#' Estimate negative log like from SIR
#'
#' @param params vector of parameters (e.g. beta, gamma)
#' @param data data frame of OBSERVED time, X1, X2, and X3 values
#' @param disease_list list with parameters
#' "params" - a numeric list of disease parameters (e.g. .1, .03)
#' "params_names" - optional variable names of params (e.g. beta, gamma)
#' "times" - times
#' "init_vals" - vector of initial values of the states.  Must be nonnegative.  Length of vector is K, the number of states, and sum of values is N the constant number of individuals
#' @param do_plug_in whether to use theoretical or observed SIR values in probability of transition in likelihood
#' @return negative log likelihood of SIR given the data
loglike_sir <- function(params,
                        data,
                        disease_list = NULL,
                        do_plug_in = FALSE){


    N <- sum(disease_list$init_vals)
    beta <- params[1]
    gamma <-params[2]
    new_data <- get_SIR_diffs(data)
    new_data <- get_SIR_lags(params, new_data, disease_list, do_plug_in)
   

    new_data <- na.omit(new_data)
    
    s_loglike <- sum(apply(new_data, 1,
                           function(row){
                               prob <- beta * row["lag_X2"] / N
                               if(prob < 0 | prob > 1){
                                  return(-10e3)
                               }
                               like <- dbinom(round(row["diff_X1"]),
                                          size = pmax(round(row["obs_X1"]), 0),
                                          prob = prob)
                               if(like > 0) return(log(like))
                               return(-10e3)
                               
                            }))
    r_loglike <- sum(apply(new_data, 1,
                           function(row){
                               if((N - row["obs_X1"] - row["obs_X3"]) <= 0) return(0)
                             if(gamma < 0 | gamma > 1){
                               return(-10e3)
                             }
                               like <- dbinom(round(row["diff_X3"]),
                                          size = pmax(round(N - row["obs_X1"] - row["obs_X3"]), 0),
                                          prob = gamma)
                               if(like > 0) return(log(like))
                               return(-10e3)
                            }))
    loglike <- s_loglike + r_loglike
    return(-loglike)
}










SIR_fxn <- function(t, state, params){
    N <- sum(state)
    with(as.list(c(state, params)), {
        dX1 <- -params[1] * state[1] * state[2] / N
        dX3 <- params[2] * state[2]
        dX2 <- -dX1 - dX3
        list(c(dX1, dX2, dX3))
    })
}


## Wrapper to make a general CM

#' Function using deSolve to numerically integrate the CM
#'
#' @param disease_list  List with
#' "params" - a numeric list of disease parameters (e.g. .1, .03)
#' "params_names" - optional variable names of params (e.g. beta, gamma)
#' "times" - times to use
#' "init_vals" - vector of initial values of the states.  Must be nonnegative.  Length of vector is K, the number of states, and sum of values is N the constant number of individuals
#' @param CM_fxn - function to use in deSolve::ode
#' @param step - step size returned.  Default is 1
#' @param do_plot - Logical.  Default is FALSE.  If TRUE we return a base plot of the curves
#' @param ... plotting parameters to be passed
#' @return data frame of time and X1 through XK
integrate_CM <- function(disease_list, CM_fxn=SIR_fxn,
                         step = 1,
                         do_plot = FALSE,
                         ...){

    ## Extract input parameters
    params <- disease_list$params
    state <- disease_list$init_vals
    K <- length(state)
    names(state) <- paste0("X", 1:K)
    times <- disease_list$times
    ## Integrate with the solver
    ode_results <- deSolve::ode(state, times, func = CM_fxn,
                                parms = params, maxsteps = 10000,
                                method = "rk4")

    ## Plot results
    if(do_plot){
        plot_ode_base(ode_results, state,
                      params, ...)
    }

    ## Return as data frame
    colnames(ode_results)[1] <- "t"
    results <- as.data.frame(ode_results)
    return(results)


}

#' Plot base R curves of results from deSolve:::ode
#'
#' @param ode_results - output from deSolve:::ode
#' @param state initial values.  Sum is N.  Length is K, the number of compartments
#' @param params disease parameters.
#' @param ... plotting params to be passed
#' @return TRUE
plot_ode_base <- function(ode_results, state, params, ...){
    ## Set up base plot
    N <- sum(state)
    K <- length(state)
    plot(1, 1, type = "n", xlim = range(ode_results[, 1]),
         ylim = range(ode_results[, 2:(K+1)]),
         main = paste0("CM curves \n N = ", N, "; Params = ",
                       paste0(params, collapse = ", ")))
         for(kk in 1:K){
             lines(ode_results[, 1], ode_results[, kk+1],
                   col = kk, lwd = 2)

         }
         legend("topright", paste0("X", 1:K),
                col = 1:K, lwd =2)
                       
    return(TRUE)

}


#' Format data to add differences between states from one time to the next
#' 
#' @param data data frame of OBSERVED time, X1, X2, and X3 values
#' @return new_data with differences and lagged values at each time step
get_SIR_diffs <- function(data){
    ## Return X1, X2, X3 diffs and previous values
    sub_df <- plyr::ddply(.data = data, .variables = c("ll"),
                          .fun =  function(x){
                              obs_X1 <- c(dplyr::lag(x$X1, 1))
                              obs_X2 <- c(dplyr::lag(x$X2, 1))
                              obs_X3 <- c(dplyr::lag(x$X3, 1))
                              return(data.frame(obs_X1= obs_X1,
                                                obs_X2 = obs_X2,
                                                obs_X3 = obs_X3))
                          })
    sub_df$diff_X1 <- sub_df$obs_X1 - data$X1
    sub_df$diff_X3 <- -sub_df$obs_X3 + data$X3
    return(data.frame(data, sub_df[, -1]))
}
                          

#' Format SIR data to use in likelihood
#'
#' @param params beta and gamma
#' @param new_data SIR data with obs_X1, obs_X2, and obs_X3 values, which are lagged differences
#' @param disease_list list with parameters
#' "params" - a numeric list of disease parameters (e.g. .1, .03)
#' "params_names" - optional variable names of params (e.g. beta, gamma)
#' "T" - max time.  The steps we take are 0, 1, ..., T inclusive
#' "init_vals" - vector of initial values of the states.  Must be nonnegative.  Length of vector is K, the number of states, and sum of values is N the constant number of individuals
#' @param do_plug_in whether to use theoretical or observed SIR values in probability of transition in likelihood
get_SIR_lags <- function(params,
                         new_data,
                         disease_list,
                         do_plug_in = TRUE){
    if(!do_plug_in){
        ## USE theoretical SIR values in like

        ode_results <- integrate_CM(disease_list, CM_fxn = SIR_fxn,
                                    step = 1, do_plot = FALSE)
        ode_results <- as.data.frame(ode_results)
        ode_results$tt <- ode_results$t + 1
        colnames(ode_results) <- c("old_t", "lag_X1", "lag_X2", "lag_X3", "tt")
        new_data <- plyr::join(new_data, ode_results[,-1], by = "tt")

    } else {
        ## DON'T RUN the actual SIR   
        new_data$lag_X1 <- new_data$obs_X1
        new_data$lag_X2 <- new_data$obs_X2
        new_data$lag_X3 <- new_data$obs_X3

    }
  return(new_data)

}
