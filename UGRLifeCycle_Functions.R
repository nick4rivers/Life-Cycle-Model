
library(VGAM) # postive only normal distribution function


#--------------BETA DISTRIBUTIONS----------------#
# Estimates shape parameters for a beta distribution based on mean and var
# Used to keep survival and transition probabilityes from 0 to 1
# Used in bev_holt and move function
est_beta_parms <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(parms = list(alpha = alpha, beta = beta))
}




###########################################################
##########   BEAVERTON HOLT FUNCTIONS   ###################
###########################################################
# Take two parameters
# stage1 = name of the start life stage
# stage_param = array of productivity, capacity, and 


#--------------BEAVERTON HOLT----------------#
bev_holt <- function (stage1, stage_param) {
  # Define productivity and capacity
  # Use beta dist if productivity < 1 and sd > 0
  if (stage_param[1] < 1 & stage_param[2] > 0) {
    beta_shapes <- est_beta_parms(stage_param[1], stage_param[2]^2)
    prod <-  stage_param[7] * rbeta(1, beta_shapes$alpha, beta_shapes$beta)
    if (prod > 1) {
      prod <- 1
    }
  } else {
    # Use normal distribution for productivity
    prod <-  stage_param[7] * rposnorm(1, mean = stage_param[1], sd = stage_param[2])
  }
  # Set capacity
  cap <- stage_param[9] * rposnorm(1, mean = stage_param[3], sd = stage_param[4])
  # The bev holt
  stage2 = floor(stage1 / (((1/prod) + ((1/cap)*stage1))))
  return(stage2)
}


#--------------HATCHERY BEAVERTON HOLT----------------#
h1_bev_holt <- function(stage1, stage_param) {
  # Define productivity and capacity
  # Use beta dist if productivity < 1 and sd > 0
  if (stage_param[1] < 1 & stage_param[2] > 0) {
    beta_shapes <- est_beta_parms(stage_param[1], stage_param[2]^2)
    prod <-  stage_param[8] * rbeta(1, beta_shapes$alpha, beta_shapes$beta)
    if (prod > 1) {
      prod <- 1
    }
  } else {
    # Use normal distribution for productivity
    prod <-  stage_param[8] * rposnorm(1, mean = stage_param[1], sd = stage_param[2])
  }
  # Set capacity using normal distribution
  cap <- stage_param[10] * rposnorm(1, mean = stage_param[3], sd = stage_param[4])
  # The bev holt  
  stage2_pop = floor(stage1 / (((1/prod) + ((1/cap)*stage1))))
  return(stage2)
}