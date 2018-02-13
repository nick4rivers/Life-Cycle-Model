#########################################
##New




# Adult beaverton holt function
bev_holt_adult <- function (x) {
  x/(((1/rnorm(1, mean = adult_prod, sd = adult_prod_sd))+((1/rnorm(1, mean = adult_cap, sd = adult_cap_sd))*x)))
}