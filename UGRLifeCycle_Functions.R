library(VGAM) # postive only normal distribution function

#--------------BETA DISTRIBUTIONS----------------#
# Estimates shape parameters for a beta distribution based on mean and stdev
# Used to keep survival and transition probabilityes from 0 to 1
# Used in bev_holt and move function
# returns a single random draw from a beta distribution
get_random_beta <- function(mean, stdev) {
    # turn the standard deviation into a variance
    var <- stdev^2
    alpha <- ((1 - mean) / var - 1 / mean) * mean ^ 2
    beta <- alpha * (1 / mean - 1)
    # get the random draw from beta distribution
    random_beta_draw <- rbeta(1, alpha, beta)
    return(random_beta_draw)
}

#--------------LOG-NORMAL DISTRIBUTIONS----------------#
# Estimates shape parameters for log-normal distribution based on mean and standard deviation
# Used to used in bev-holt for capacity stochasticity
# returns a single random draw from a log-normal distribution
get_random_log <- function(mean, stdev) {
    location <- log(mean^2 / sqrt(stdev^2 + mean^2))
    shape <- sqrt(log(1 + (stdev^2 / mean^2)))
    random_log_draw <- rlnorm(1, location, shape)
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
# TODO refactor into a sinlge bev-holt function
# Take two parameters
# stage1 = population at start life stage
# stage_param = array of productivity, capacity, and stdeviations
# returns stage 2 = population size at next life stage

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
    # using log-normal distribution function
    cap <- get_random_log(stage_param[3], stage_param[4])
    # multiplied by natural capacity scaler
    cap <- cap * stage_param[9]
    
    # Finally the beverton holt function for next stage population size
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
    # using log-normal distribution function
    cap <- get_random_log(stage_param[3], stage_param[4])
    # multiplied by natural capacity scaler
    cap <- cap * stage_param[10]
    
    # Finally the beverton holt function for next stage population
    stage2 = floor(stage1 / (((1/prod) + ((1/cap)*stage1))))
    #return the population size
    return(stage2)
}

#--------------MODIFIED NATURAL BEAVERTON HOLT----------------#
# based on reformulation of beaverton holt equation fit to log data
# allows variation stochastic error around model fit to be log distributed
log_bev_holt <- function(stage1, stage_param) {
    # scale productivity and make sure < 1
    stage_param[1] <- stage_param[1] * stage_param[7]
    if (stage_param[1] > 1) {
        stage_param[1] = 1
    }
    # scale capacity
    stage_param[3] * stage_param[7]
    # solve for beta from carrying capacity and productivity (survival)
    beta <- stage_param[3] / stage_param[1]
    # log predicted next stage
    log_stage2 <- log(stage_param[3]) - log(beta + stage1)
    # add error
    log_stage2 <- log_stage2 + rnorm(1, 0, stage_param[4])
    # transform to real value
    stage2 <- floor(stage1*(exp(log_stage2)))
    return(stage2)
}

# TODO refactor so prevent using two versions of beverton holt functions
# and the hatchery version
h1_log_bev_holt <- function(stage1, stage_param) {
    # scale productivity and make sure < 1
    stage_param[1] <- stage_param[1] * stage_param[8]
    if (stage_param[1] > 1) {
        stage_param[1] = 1
    }
    # scale capacity
    stage_param[3] * stage_param[10]
    # solve for beta from carrying capacity and productivity (survival)
    beta <- stage_param[3] / stage_param[1]
    # log predicted next stage
    log_stage2 <- log(stage_param[3]) - log(beta + stage1)
    # add error
    log_stage2 <- log_stage2 + rnorm(1, 0, stage_param[4])
    # transform to real value
    stage2 <- floor(stage1*(exp(log_stage2)))
    return(stage2)
}
