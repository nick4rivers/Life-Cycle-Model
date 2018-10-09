library(VGAM) # postive only normal distribution function

# TODO refactor beta distribution function throughout?
#--------------BETA DISTRIBUTIONS----------------#
# Estimates shape parameters for a beta distribution based on mean and var
# Used to keep survival and transition probabilityes from 0 to 1
# Used in bev_holt and move function
# Became get_random_beta <- est_beta_parms
get_random_beta <- function(mean, stdev) {
    var <- stdev^2
    alpha <- ((1 - mean) / var - 1 / mean) * mean ^ 2
    beta <- alpha * (1 / mean - 1)
    random_beta_draw <- rbeta(1, alpha, beta)
    return(random_beta_draw)
}


# TODO Refactor this shit too, only better
#--------------LOG-NORM-Distribution----------------#
# Estimates shape parameters for log-normal distribution based on mean and standard deviation
# Used to used in bev-holt for productivity and capacity stochasticity
est_log_parms <- function(mu, stdev) {
    location <- log(mu^2 / sqrt(stdev^2 + mu^2))
    shape <- sqrt(log(1 + (stdev^2 / mu^2)))
    return(parms = list(location = location, shape = shape))
}


###########################################################
##########   MOVEMENT STOCHASTICITY     ###################
###########################################################
# Allows stochastic estimate of choice with two outcomes
# Takes parameter array for first choice
# Outputs a list containing the probability of each choice
# became move_choices to be more explicit, update
move_choices <- function(choice1_params) {
    prob1 <- get_random_beta(choice1_params[5], choice1_params[6])
    prob2 <- 1 - prob1
    choice_probs <- list(prob1 = prob1, prob2 = prob2)
    return(choice_probs)
}



###########################################################
##########   BEAVERTON HOLT FUNCTIONS   ###################
###########################################################
# Take two parameters
# stage1 = name of the start life stage
# stage_param = array of productivity, capacity, and 

#--------------BEAVERTON HOLT----------------#
bev_holt <- function (stage1, stage_param) {
    # Set Productivity
    # No variation just set productivity * natural scaler
    if (stage_param[2] == 0) {
        prod <- stage_param[1] * stage_param[7]
        # if it was a survival, make sure it didn't go over 1
        if (stage_param[1] < 1) {
            prod <- 1
        }
        # if it's a survival use beta distribution
    } else if (stage_param[1] < 1) {
        prod <- get_random_beta(stage_param[1], stage_param[2])
        # multiply by the productivity scaler for natural adults
        prod <- prod * stage_param[7]
        # and if over 1 just set back to 1
        if (prod > 1) {
            prod <- 1
        }
        # if its fecundity use positive normal distribution
    } else if (stage_param[1] >= 1) {
        # TODO will turn this into log distribution
        # use positive normal distribution for productivity
        prod <- rposnorm(1, mean = stage_param[1], sd = stage_param[2])
        # scale with natural scaler
        prod <- prod * stage_param[7] 
    }
    # Set Capacity
    # TODO change to log-normal
    # as positive normal distribution function
    cap <- rposnorm(1, mean = stage_param[3], sd = stage_param[4])
    # multiplied by natural capacity scaler
    cap <- cap * stage_param[9]
    
    # Finally the beverton holt function for next stage population
    stage2 = floor(stage1 / (((1/prod) + ((1/cap)*stage1))))
    #return the population size
    return(stage2)
}

#--------------HATCHERY BEAVERTON HOLT----------------#
h1_bev_holt <- function (stage1, stage_param) {
    # Set Productivity
    # No variation just set productivity * natural scaler
    if (stage_param[2] == 0) {
        prod <- stage_param[1] * stage_param[7]
        # if it was a survival, make sure it didn't go over 1
        if (stage_param[1] < 1) {
            prod <- 1
        }
        # if it's a survival use beta distribution
    } else if (stage_param[1] < 1) {
        prod <- get_random_beta(stage_param[1], stage_param[2])
        # multiply by the productivity scaler for natural adults
        prod <- prod * stage_param[8]
        # and if over 1 just set back to 1
        if (prod > 1) {
            prod <- 1
        }
        # if its fecundity use positive normal distribution
    } else if (stage_param[1] >= 1) {
        # TODO will turn this into log distribution
        # use positive normal distribution for productivity
        prod <- rposnorm(1, mean = stage_param[1], sd = stage_param[2])
        # scale with natural scaler
        prod <- prod * stage_param[8] 
    }
    # Set Capacity
    # TODO change to log-normal
    # as positive normal distribution function
    cap <- rposnorm(1, mean = stage_param[3], sd = stage_param[4])
    # multiplied by natural capacity scaler
    cap <- cap * stage_param[10]
    
    # Finally the beverton holt function for next stage population
    stage2 = floor(stage1 / (((1/prod) + ((1/cap)*stage1))))
    #return the population size
    return(stage2)
}