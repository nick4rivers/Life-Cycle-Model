# 1--------------------SPECIFY SCENARIO-----------------------#
model_name <- "CC_WidPNV" # Your scenario name here
years <- 100
runs <- 20
population <- "CC" # CC or UGR

# Initial values
seed_fry <- 100000
seed_hatchery_smolt <- 100000

# Stop supplementation in year stop_sup, set to ZERO to turn supplimentation OFF
stop_sup <- 60 # set to YEARS if ON, set to ZERO if OFF

# 2---------------READ IN THE INPUT FILE---------------------------#
#model inputs
input <- read.csv(file.choose(), stringsAsFactors = FALSE)

# 3---------------LOAD FUNCTIONS AND RUN THE MODEL-----------------#
source("UGRLifeCycle_Functions.R")
source("UGRLifeCycle_Run.R")

# 4----------------CALL SUMMARY AND GRAPHICS-----------------------#
source("UGRLifeCycle_Plot.R", print.eval=TRUE)

# 5------------WRITE OUTPUT TO WORKING DIRECTORY----------------#
# final dataframe with all raw simulation values
write.csv(final, file=paste(getwd(),"/",model_name,"_final.csv", sep=""))
# summary dataframe with summarized values
write.csv(summary, file=paste(getwd(),"/",model_name,"_summary.csv", sep=""))




# WRITE SIMULATIONS TO DESKTOP
write.csv(sims, file=("/Users/nick/Desktop/sims.csv"))
