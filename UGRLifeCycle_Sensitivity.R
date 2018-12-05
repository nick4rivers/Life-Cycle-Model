# MAKE SURE YOU HAVE NECESSARY PACKAGES
library(VGAM) # positive only normal distribution function
library(tidyverse) # summary and graphics
library(scales) # more graphics shit
library(tcltk) # for folder selection

# 1---------------LOAD FUNCTIONS------------------------------------#
source("UGRLifeCycle_Functions.R")
model_folder <- tk_choose.dir()



# Set up required parameters to move and what to set them to
sensitivity_on <- "ON"
# add the base parameter value
base_value <- 0.02
sensitivity_scaler <- c(0.01, 0.2, 0.4, 0.6, 0.8, 1, base_value)
# Name the life-stage for data
sensitivity_life_stage <- "LGD Smolt - Adult Return"
# "p" for productivity, "c" for capacity
sensitivity_param <- "p"

# 2---------------READ IN THE INPUT FILES---------------------------#
input <- read.csv(paste(model_folder, "/inputs.csv", sep=""), stringsAsFactors = FALSE)
settings <- read.csv(paste(model_folder, "/settings.csv", sep=""), stringsAsFactors = FALSE)


# Call the model iteratively
# Be sure to set the proper life-stage in _Run.R on line 43
for (s in 1:length(sensitivity_scaler)) {
    source("UGRLifeCycle_Run.R")
    final$SensitivityStage <- sensitivity_life_stage
    final$SensitivityScaler <- sensitivity_scaler[s]
    final$SensitivityParameter <- sensitivity_param
    final$SensitivityRep <- s
    final$SensitivityBase <- if (sensitivity_scaler[s] == base_value) {
                                    "BASE"
                                } else {
                                    "NOT BASE"
                                }
    
    # create final sensitivity dataframe for sensitivity trials
    # new on first rep
    if (s == 1) {
        final_sensitivity <- final
    # append on other reps
    } else {
        final_sensitivity <- rbind(final_sensitivity, final)
    }
    print("---------------------------------------------")
    print(paste("Sensitivity rep:",s," - WICKED GOOD"))
    print("---------------------------------------------")
    Sys.sleep(0.5)
}

# Write out the final sensitivity dataset
write.csv(final_sensitivity, file=paste(model_folder, "/sens_", sensitivity_life_stage, "_", sensitivity_param, ".csv", sep=""))

#------------------------------------#
# OUTPUT IMPORT
#------------------------------------#

# Link to the folder containing sensitivity output
sensitivity_dir <- tk_choose.dir()

# Loop through directories and stack up dataframes
#Get the number of files in the directory
sensitivity_files <- list.files(sensitivity_dir, recursive=FALSE)

# get the first one, make a temp table, and final results table
sensitivity_table <- read.csv(paste(sensitivity_dir,"/" , sensitivity_files[1], sep=""))

# read the rest and append to results_table
for (i in 2:length(sensitivity_files)) {
    temp_sensitivity_table <- read.csv(paste(sensitivity_dir,"/" , sensitivity_files[i], sep=""))
    sensitivity_table <- rbind(sensitivity_table, temp_sensitivity_table)
}
rm(temp_sensitivity_table)



#------------------------------------#
# SPAWNER SUMMARIZATION
#------------------------------------#
sensitivity_summary <- sensitivity_table %>%
    select(SensitivityStage, ModelName, Run, Year, SensitivityParameter, SensitivityScaler, SensitivityBase, Spawner) %>%
    filter(Year > 50) %>%
    group_by(SensitivityStage, ModelName, Run, SensitivityBase, SensitivityScaler) %>%
        summarise(medSpawner = median(Spawner)) %>%
        group_by(SensitivityStage, ModelName, SensitivityBase, SensitivityScaler) %>%
            summarise(med_Spawner = median(medSpawner)) %>%
       arrange(SensitivityStage)

sensitivity_base <- sensitivity_summary %>%
    filter(SensitivityBase == 'BASE')

ggplot() +
    geom_line(data = sensitivity_summary, aes(x = SensitivityScaler, y = med_Spawner, color = SensitivityStage), alpha = 0.7, size = 1.3) +
    geom_point(data = sensitivity_base, aes(x = SensitivityScaler, y = med_Spawner, color = SensitivityStage), alpha = 0.8, size = 4) +
    labs(y = 'Natural Spawners', x = 'p Survival', title = 'Productivity') +
    theme(axis.text = element_text(size=12,face="bold"),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=16, face="bold"),
          plot.subtitle = element_text(size=14, face="bold", color = "darkgray"),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.background = element_rect(fill=alpha('white', 0.5)),
          legend.position = c(0.25, 0.85))
    
#------------------------------------#
# QER SUMMARIZATION                  #
#------------------------------------#
# Mark spawners with respect to 50, the threshold
QER_table <- sensitivity_table %>%
    select(ModelName, SensitivityStage, Run, Year, SensitivityBase, SensitivityScaler, TotalSpawner) %>%
    # cut out burn in years and final 5 years
    # consider without hatchery program
    # TODO double check on how years are handled
    filter(Year >= 50 & Year <= max(Year) - 5) %>%
    # create risk with risky = 1, not-risky = 0
    mutate(Risk = ifelse(TotalSpawner > 50, 0, 1)) %>%
    # create blank column for running risk total 
    mutate(RunningRisk = 0)

# do the running consecutive years of extinction
# check first year
if (QER_table$Risk[1] == 1) {
    QER_table$RunningRisk[1] = 1
}
# now loop to total up consecutive years of risk
for (i in 2:nrow(QER_table)) {
    # if it's the same run check for risk
    if (QER_table$Run[i] == QER_table$Run[i - 1]) {
        # add the running consecutive total
        if (QER_table$Risk[i] == 1) {
            QER_table$RunningRisk[i] = QER_table$Risk[i] + QER_table$RunningRisk[i - 1]
        } else {
            QER_table$RunningRisk[i] = 0
        }
        # if run changes check current risk
    } else {
        if (QER_table$Risk[i] == 1) {
            QER_table$RunningRisk[i] = 1
        }
    }
}

# QER Summary
QER_summary <- QER_table %>%
    group_by(ModelName, SensitivityStage, Run, SensitivityBase, SensitivityScaler) %>%
    summarise(
        TotalRisk = sum(Risk),
        MaxRisk = max(RunningRisk)) %>%
    mutate(RiskyRuns = ifelse(MaxRisk > 3, 1, 0)) %>%
    group_by(ModelName, SensitivityStage, SensitivityBase, SensitivityScaler) %>%
    summarise(
        TotalRiskyRuns = sum(RiskyRuns),
        TotalRuns = max(Run)) %>%
    mutate(QER = TotalRiskyRuns / TotalRuns)


QER_base <- QER_summary %>%
    filter(SensitivityBase == 'BASE')

ggplot() +
    geom_line(data = QER_summary, aes(x = SensitivityScaler, y = QER, color = SensitivityStage), alpha = 0.7, size = 1.3, se = FALSE) +
    geom_point(data = QER_base, aes(x = SensitivityScaler, y = QER, color = SensitivityStage), alpha = 0.8, size = 4) +
    labs(y = 'pQER', x = 'p Survival', title = 'Quasi-Extinction Risk') +
    theme(axis.text = element_text(size=12,face="bold"),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=16, face="bold"),
          plot.subtitle = element_text(size=14, face="bold", color = "darkgray"),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.background = element_rect(fill=alpha('white', 0.5)),
          legend.position = c(0.75, 0.80))
