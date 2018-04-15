

# 1--------------------SPECIFY SCENARIO-----------------------#
scenario_name <- "CC" # your scenario name here
years <- 10
runs <- 10
population <- "CC" # CC or UGR

# Initial values
seed_fry <- 100000
seed_hatchery_smolt <- 100000

# Stop supplementation in year
stop_sup <- 50 # set to YEARS if ON, set to ZERO if OFF

# 2---------------READ IN THE INPUT FILE----------------------#
#model inputs
input <- read.csv(file.choose(), stringsAsFactors = FALSE)

# 3---------------CALL THE MODEL-------------------------------#
source("UGRLifeCycle_Run.R")

# 4----------------CALL SUMMARY GRAPHICS-----------------------#
source("UGRLifeCycle_Plot.R", print.eval=TRUE)


# 5 write out a single summarization
# change to your directory of choice
write.csv(final, file="/Users/nick/Desktop/final.csv")




