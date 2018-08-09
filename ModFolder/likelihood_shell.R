
# Inputs:
### start_pop is the starting estimate for the number of people in each compartment
### sus_data is the data for total number of susceptible people.
### inf_data is the data for prevalence
### times is in case the data are not evenly spaced and we need time point labels
### Can add more parameters needed for calculation
## Assume total population stays the same
# Outputs:
### r0 is the R0 estimates
### r0 is the estimated standard error for the given estimate.
sir_likelihood <- function(start_pop, sus_data, inf_data,
                          times, ...) {

  # Stuff
  return(list(est = r0, sd = r0_sd))
}
