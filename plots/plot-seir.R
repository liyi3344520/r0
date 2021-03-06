## SKG
## Plotting the baseline SEIR

## Package from simCAM (https://github.com/shannong19/simCAM)

devtools::load_all("~/simCAM")
library(ggplot2)
library(reshape2)
library(latex2exp)

r0_var <- function(hess, beta, gamma){
    hess <- hess[1:2, 1:2]
    grad <- c(1/gamma, -beta/gamma^2)
    var <- t(grad) %*% solve(hess) %*% grad
    return(var)
}

T <- 365
init_vals <- c(9500, 0, 500, 0) #SEIR 
beta <- .06
gamma <- .03
alpha <- .01
N <- sum(init_vals)
step <- 7
## Plot the SIR
do_plot = FALSE
seir_data <- SEIR(T, init_vals, beta, gamma,
                  alpha, step, SEIR_inner, do_plot)
## Optimize function
inits <- c(beta, gamma, alpha)
params <- optim(par=inits, fn=SSE_SEIR, data=seir_data,
                init_vals = init_vals, hessian = TRUE)
colnames(seir_data)[2:5] <- c("X", "E", "Y", "Z")
## Change SEIR data to SIR
sir_data <- data.frame(t = seir_data$t,
                       X = seir_data$X + seir_data$E,
                       Y = seir_data$Y,
                       Z = seir_data$Z)
init_sir <- inits[-3]
init_vals_sir <- init_vals[-2]
params_sir <- optim(par=init_sir, fn=SSE_SIR, data=sir_data,
                    init_vals = init_vals_sir, hessian = TRUE)
r0_seir <- params$par[1] / params$par[2]
r0_sir <- params_sir$par[1] / params_sir$par[2]
c(r0_seir, r0_sir)

sir_data$type <- "XYZ"
sir_melt <- melt(sir_data, id.vars=c("t", "type"))
seir_data$type <- "XEYZ"
seir_melt <- melt(seir_data, id.vars = c("t", "type"))
df_melt <- rbind(seir_melt, sir_melt)
colnames(df_melt)[3] <- "Compartment"

g <- ggplot(data = df_melt, aes(x=t, y=value, linetype=Compartment,
                                col=Compartment)) + facet_wrap(~type, dir="v") + 
    geom_line(size=1) +
    scale_colour_grey() + 
    theme_bw() + geom_point(size=2) + 
    labs(x = "Time", y = "Number of Individuals",
         title = "SEIR/SIR (XEYZ/XYZ) Curves",
         subtitle = TeX(sprintf("$N$ = %.2e; $\\beta$=%.2f; $\\gamma$= %.2f; $\\mu$= %.2f; $(X(0), E(0), Y(0))$= (%.3e, 0,  %.1e)",
                                sum(init_vals), beta, gamma, alpha,  init_vals[1], init_vals[3]))) +
    ggplot2::theme(
                     axis.text.x = ggplot2::element_text(size = 16),
                     axis.text.y= ggplot2::element_text(size = 16),
                     axis.title.x= ggplot2::element_text(size = 18),
                     axis.title.y= ggplot2::element_text(size = 18),
                     plot.title = ggplot2::element_text(size = 24),
                     legend.title = ggplot2::element_text(size = 20),
                     legend.text = ggplot2::element_text(size=16),
                     legend.key.size = ggplot2::unit(3, "line"),
                     plot.subtitle = ggplot2::element_text(size=16)
                     )
g

ggsave("seir-sir-data.pdf", width=10, height=6)


t <- proc.time()[3]
## ranging over different values of alpha
T <- 365
init_vals <- c(9500, 0, 500, 0) #SEIR 
beta <- .06
gamma <- .03
step <- 1
N <- sum(init_vals)
alpha_seq <- seq(from=.005, 1, by = .005)
r0_mat <- matrix(0, ncol=4, nrow=length(alpha_seq))
row <- 0
for(alpha in alpha_seq){
    row <- row + 1
    do_plot = FALSE
    seir_data <- SEIR(T, init_vals, beta, gamma,
                      alpha, step, SEIR_inner, do_plot)
    ## Optimize function
    inits <- c(beta, gamma, alpha)
    params <- optim(par=inits, fn=SSE_SEIR, data=seir_data,
                    init_vals = init_vals, hessian = TRUE)
    colnames(seir_data)[2:5] <- c("X", "E", "Y", "Z")
    ## Change SEIR data to SIR
    sir_data <- data.frame(t = seir_data$t,
                           X = seir_data$X + seir_data$E,
                           Y = seir_data$Y,
                           Z = seir_data$Z)
    init_sir <- inits[-3]
    init_vals_sir <- init_vals[-2]
    params_sir <- optim(par=init_sir, fn=SSE_SIR, data=sir_data,
                        init_vals = init_vals_sir, hessian = TRUE)
    r0_seir <- params$par[1] / params$par[2]
    var_seir <- r0_var(params$hes, beta=params$par[1], gamma = params$par[2])
    r0_sir <- params_sir$par[1] / params_sir$par[2]
    var_sir <- r0_var(params_sir$hes, beta=params_sir$par[1], gamma = params_sir$par[2])
    r0_mat[row, 1:2 ] <- c(r0_seir, r0_sir)
    r0_mat[row, 3:4] <- c(var_seir, var_sir)
}
proc.time()[3] - t
r0_df <- as.data.frame(r0_mat)
colnames(r0_df) <- c("R0_seir", "R0-sir", "var_seir", "var_sir")
write.csv(r0_df, "r0-est.csv", row.names = FALSE)

r0_df <- read.csv("r0-est.csv")

## Formatt for ggplot
seir <- r0_df[, c(1, 3)]
colnames(seir) <- c("Estimate", "Variance")
seir$Type <- "SEIR"
seir$alpha <- alpha_seq
sir <- r0_df[, c(2, 4)]
colnames(sir) <- c("Estimate", "Variance")
sir$Type <- "SIR"
sir$alpha <- alpha_seq
df <- rbind(seir, sir)
df$Variance <- ifelse(df$Variance < 0, NA, df$Variance)
df$lower <- df$Estimate - 2 * sqrt(df$Variance)
df$upper <- df$Estimate + 2 * sqrt(df$Variance)

g <- ggplot(data = df, aes(x= alpha, y=Estimate, group = Type,
                           col = Type)) + geom_line() + theme_bw() + geom_point(size=2) +
    scale_colour_grey() + xlim(0, .25) + 
    labs(x=TeX(sprintf("$\\mu$")), title = TeX(sprintf("Estimates of $\\hat{R}_0$"))) + 
    ggplot2::theme(
                 axis.text.x = ggplot2::element_text(size = 16),
                 axis.text.y= ggplot2::element_text(size = 16),
                 axis.title.x= ggplot2::element_text(size = 18),
                 axis.title.y= ggplot2::element_text(size = 18),
                 plot.title = ggplot2::element_text(size = 24),
                 legend.title = ggplot2::element_text(size = 20),
                 legend.text = ggplot2::element_text(size=16),
                 legend.key.size = ggplot2::unit(3, "line"),
                 plot.subtitle = ggplot2::element_text(size=16)
             )
g

ggsave("r0-est.pdf", width=10, height=6)
#    geom_errorbar(aes(ymin=lower, ymax = upper))
