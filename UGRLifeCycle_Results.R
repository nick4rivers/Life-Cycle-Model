library(tidyverse) # summary and graphics
library(scales)
library(gridExtra)

####################################################
########        IMPORT DATA                 ########
####################################################

# get data
results_dir <- tk_choose.dir()


# Loop through directories and stack up dataframes
#Get the number of files in the directory
scenario_dirs <- list.dirs(results_dir, recursive=FALSE)

# get the first one, make a temp table, and final results table
temp_raw_table <- read.csv(paste(scenario_dirs[1],"/output_raw.csv", sep=""))
temp_summary_table <- read.csv(paste(scenario_dirs[1],"/output_summary.csv", sep=""))

raw_table <- temp_raw_table
summary_table <- temp_summary_table

# read the rest and append to results_table
for (i in 2:length(scenario_dirs)) {
    temp_raw_table <- read.csv(paste(scenario_dirs[i],"/output_raw.csv", sep=""))
    raw_table <- rbind(raw_table, temp_raw_table)
    temp_summary_table <- read.csv(paste(scenario_dirs[i],"/output_summary.csv", sep=""))
    summary_table <- rbind(summary_table, temp_summary_table)
}


######################################################################################
########    Create a Table of Just Natural Spawner Abundances min, max, etc...########
######################################################################################

# take the median value among years for each run
raw_summary <- raw_table %>%
    select(ModelName, Model, Population, Scenario, Year, Run, Egg, Parr, LGDSmolt, H1_LGDSmolt, TrapAdult, H1_TrapAdult, Brood, TotalTrap, H1_Brood, Spawner, H1_Spawner, TotalSpawner) %>%
    # cut out burn in years and final years, shoot for 50 years
    filter(Year >= 40 & Year <= max(Year) - 10) %>%
    group_by(ModelName, Population, Scenario, Model, Run) %>%
    # get median for list stages by run
    summarise(
        Egg = median(Egg),
        Parr = median(Parr),
        LGDSmolt = median(LGDSmolt),
        H1_LGDSmolt = median(H1_LGDSmolt),
        TrapAdult = median(TrapAdult),
        H1_TrapAdult = median(H1_TrapAdult),
        TotalTrap = median(TotalTrap),
        Spawner = median(Spawner),
        H1_Spawner = median(H1_Spawner),
        TotalSpawner = median(TotalSpawner)
        )

# Final spawner table, which is summary of median values
# this will get written out for table in report
spawner_table <- raw_summary %>%
    group_by(ModelName, Population, Scenario, Model) %>%
        summarise(spawnerMin = min(Spawner),
                  spawnerMed = median(Spawner),
                  spawnerMax = max(Spawner),
                  spawnerSD = sd(Spawner)) %>%
        mutate(supplementation = "Off")

# write out spawner table results
pop <- as.character(spawner_table$Population[1])
write.csv(spawner_table, file=paste(results_dir, "/",pop,"_spawners.csv", sep=""))


####################################################
########    BOXPLOT MODEL COMPARISON        ########
####################################################
# 1000 X 500 px
# CC = seagreen
# UGR = steelblue

# Order factors for boxplots
raw_summary$Scenario <- factor(raw_summary$Scenario, levels = c('Curr', 'Clim', 'ClimVeg', 'ClimVegWid'))

# the boxplot function
box_compare <- function(data, stage, fill_color, y_label, plot_title) {
    ggplot(data, aes(y = stage, x = Scenario)) +
        geom_jitter(width = 0.3, alpha = 0.3) +
        geom_boxplot(fill = fill_color, alpha = 0.7) +
        labs(y = y_label, title = plot_title, subtitle = "Justice 2007 - Cease Supplementation") +
        theme(axis.text = element_text(size=12,face="bold"),
              axis.title = element_text(size=14,face="bold"),
              plot.title = element_text(size=18, face="bold"),
              plot.subtitle = element_text(size=14, face="bold", color = "darkgray")) +
        scale_y_continuous(labels = comma)
}

# Parr
box_compare(raw_summary, raw_summary$Parr, "green3", "Median Parr", "Late Summer Parr")

# LGD Smolts
box_compare(raw_summary, raw_summary$LGDSmolt, "coral3", "Median Smolt", "Natural Smolt at LGD")

# Natural Spawners
ugr_box <-
# cc_box <-
box_compare(raw_summary, raw_summary$Spawner, "seagreen", "Median Natural Spawners", "Catherine Creek")

# Natural Adult To Trap
box_compare(raw_summary, raw_summary$Parr, "green3", "Median Parr", "Late Summer Parr")


# Save the plot for later to merge with the one from Cathrine Creek
 


####################################################
########    EXTINCTION THRESHOLD            ########
####################################################

# Mark spawners with respect to 50, the threshold
QER_table <- raw_table %>%
    select(ModelName, Model, Scenario, Population, Run, Year, TotalSpawner) %>%
    # cut out burn in years and final 5 years
    filter(Year >= 40 & Year <= max(Year) - 10) %>%
    # create risk with risky = 1, not-risky = 0
    mutate(Risk = ifelse(TotalSpawner > 50, 0, 1)) %>%
    # create blank column for running risk total  %>%
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
    group_by(ModelName, Scenario, Population, Model, Run) %>%
    summarise(
        TotalRisk = sum(Risk),
        MaxRisk = max(RunningRisk)) %>%
    mutate(RiskyRuns = ifelse(MaxRisk > 3, 1, 0)) %>%
    group_by(ModelName, Scenario, Population, Model) %>%
    summarise(
        TotalRiskyRuns = sum(RiskyRuns),
        TotalRuns = max(Run)) %>%
    mutate(QER = TotalRiskyRuns / TotalRuns)


# write out the QER table to directory
write.csv(QER_summary, file=paste(results_dir, "/",pop,"_QER.csv", sep=""))


####################################################
########    RIBBON COMPARISON               ########
####################################################
# used to compare two model runs, place in seperate folder

b1 <- summary_table %>%
    filter(Model == 'CC-Curr') %>%
    ggplot() +
    geom_ribbon(aes(x = Year, ymin = SpawnerMin, ymax = SpawnerMax),alpha = 0.5, fill = "dodgerblue") +
    geom_ribbon(aes(x = Year, ymin = Spawner25, ymax = Spawner75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = SpawnerMed), lwd = 1, alpha = 0.7) +
    labs(y = "Natural Spawners", title = "Catherine Creek", subtitle = "Base Model") +
    theme(axis.text = element_text(size=12,face="bold"),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=18, face="bold"),
          plot.subtitle = element_text(size=14, face="bold", color = "darkgray")) +
    scale_y_continuous(labels = comma)

b2 <- summary_table %>%
    filter(Model == 'UGR-Curr') %>%
    ggplot() +
    geom_ribbon(aes(x = Year, ymin = SpawnerMin, ymax = SpawnerMax),alpha = 0.5, fill = "dodgerblue") +
    geom_ribbon(aes(x = Year, ymin = Spawner25, ymax = Spawner75),  alpha = 0.5) +
    geom_line(aes(x = Year, y = SpawnerMed), lwd = 1, alpha = 0.7) +
    labs(y = "Natural Spawners", title = "Upper Grande Ronde", subtitle = "Base Model") +
    theme(axis.text = element_text(size=12,face="bold"),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=18, face="bold"),
          plot.subtitle = element_text(size=14, face="bold", color = "darkgray")) +
    scale_y_continuous(labels = comma)

grid.arrange(b1, b2, nrow = 2)
