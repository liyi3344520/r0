### SIR through Harko's 

# start_param is the intial parameter guess for SIR model
# start_pop is the initial compartment populations
# inf.data is the infected prevalence data
# rem.data is the removed prevalence data
harkoSIR <- function(start_param = c("beta" = 0.06, "gamma" = 0.05),
                             start_pop = c("S" = 99950, "I" = 50, "R" = 0), 
                             sus_data, inf_data, rem_data) {
  tot_pop <- sum(start_pop)
  init_pop <- start_pop / tot_pop
  rem_data <- rem_data / tot_pop
  inf_data <- inf_data / tot_pop
  sus_data <- sus_data / tot_pop
#   #Find the SSE between the real data and the simulation from the model. 
#   opt <- function(inf.model,inf.data) {
#     err<- sum((inf.model - inf.data)^2)
#     return(err)
#   }
#   
#   harko_sir <- function(params, inf.data, rem.data, init) {
#     beta <- params["beta"]
#     gamma <- params["gamma"]
#     sus <- init["S"] * exp(beta / gamma * init["R"]) * exp(- beta / gamma * rem.data)
#     inf <- sum(init) - rem.data - sus
#     return(inf)
#   }
#   
#   #Function to be put in the function which finds the minimum SSE.
#   model.sir <- function(params,initialpop, inf.data, rem.data) {
#     inf.model <- harko_sir(params, inf.data, rem.data, initialpop)
#     sse <- opt(inf.model,inf.data)
#     return(sse)
#   }
#   
#   
#   #Function to minimize the SSE between the data and the model. 
#   sir.optim <- function(parm, initpop, inf.data, rem.data) {
#     par.sir <- optim(parm, model.sir, initialpop = initpop,
#                      inf.data = inf.data,
#                      rem.data = rem.data, hessian = T)
#     return(c(par.sir))
#   }
#   
#   #Solving as regular SIR.
#   regularSIR <- matrix(ncol = length(init.param))
#   colnames(regularSIR) <- c("beta","gamma")
#   model_est <- sir.optim(init.param, init.pop, inf.data, rem.data)
  lin_reg <- lm(log(pmax(sus_data / init_pop["S"], 0.01)) ~ rem_data)
  
  
  r0_est <- abs(summary(lin_reg)$coef[2, 1])
  r0_sd <- summary(lin_reg)$coef[2, 2]
#   return(list(est = r0_est))
  return(list(est = r0_est, sd = r0_sd, output = summary(lin_reg)))
}


# dat <- read.csv("https://raw.githubusercontent.com/atzechang/datasets/master/GitData/Baseline1Norm.csv?token=AUVaoJY0HB6ltuwnAYGcjJUe15u8rzgOks5Yz1SWwA%3D%3D")[,-1]
# harkoSIR(c("beta" = 0.06, "gamma" = 0.03), c("S" = 99950, "I" = 50, "R" = 0), dat[, 3], dat[, 4])

