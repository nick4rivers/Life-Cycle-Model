# MAKE SURE YOU HAVE NECESSARY PACKAGES
library(VGAM) # positive only normal distribution function
library(tidyverse) # summary and graphics
library(tcltk) # for folder selection


# 1---------------READ IN THE INPUT FILES---------------------------#
model_folder <- tk_choose.dir()
input <- read.csv(paste(model_folder, "/inputs.csv", sep=""), stringsAsFactors = FALSE)
settings <- read.csv(paste(model_folder, "/settings.csv", sep=""), stringsAsFactors = FALSE)


# 2---------------LOAD FUNCTIONS------------------------------------#
source("UGRLifeCycle_Functions.R")


# 3---------------CALL THE MODEL------------------------------------#
source("UGRLifeCycle_Run.R")



# 4----------------CALL SUMMARY AND GRAPHICS-----------------------#
source("UGRLifeCycle_Plot.R", print.eval=TRUE)


# 5------------WRITE OUTPUT MODEL DIRECTORY----------------#
# final dataframe with all raw simulation values
write.csv(final, file=paste(model_folder, "/output_raw.csv", sep=""))
# summary dataframe with summarized values
write.csv(summary, file=paste(model_folder, "/output_summary.csv", sep=""))
