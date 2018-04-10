

# 1--------------------SPECIFY SCENARIO-----------------------#
scenario_name <- "CC" # your scenario name here
years <- 100
runs <- 500
population <- "CC" # CC or UGR

# Initial values start at natural fry and hatchery smolt
seed_fry <- 1000000
seed_hatchery_smolt <- 150000

# Stop supplementation in year
stop_sup <- 80 # set to YEARS if ON

# 2---------------READ IN THE INPUT FILE----------------------#
#model inputs
input <- read.csv(file.choose(), stringsAsFactors = FALSE)

# 3---------------CALL THE MODEL-------------------------------#
source("UGRLifeCycle_Run.R")

# 4----------------CALL SUMMARY GRAPHICS-----------------------#
source("UGRLifeCycle_Plot.R", print.eval=TRUE)


# 5 write out a single summarization
# change to your directory of choice
write.csv(sims, file="/Users/nick/Desktop/final.csv")




