library(tidyverse) # summary and graphics

# Loop through directories and stack up dataframes
#Get the number of files in the directory
scenario_dirs <- list.dirs(results_dir, recursive=FALSE)

# get the first one, make a temp table, and final results table
temp_results_table <- read_csv(paste(scenario_dirs[1],"/output_raw.csv", sep=""))
results_table <- temp_results_table

# read the rest and append to results_table
for (i in 2:length(scenario_dirs)) {
    temp_results_table <- read_csv(paste(scenario_dirs[i],"/output_raw.csv", sep=""))
    results_table <- rbind(results_table, temp_results_table)
}

results_summary <- results_table %>%
    select(ModelName, Model, Year, Run, Egg, Parr, LGDSmolt, H1_LGDSmolt, TrapAdult, H1_TrapAdult, Brood, TotalTrap, H1_Brood, Spawner, H1_Spawner, TotalSpawner) %>%
    # cut out burn in years and final 5 years
    filter(Year >= 50 & Year <= 195) %>%
    group_by(ModelName, Model, Run) %>%
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

# Parr
ggplot(results_summary, aes(y = Parr, x = Model)) +
    geom_jitter(width = 0.3, alpha = 0.3) +
    geom_boxplot(fill = "green3", alpha = 0.7) +
    labs(y = "Median Parr", title = "Late Summer Parr", subtitle = paste(max(results_summary$Run),"Model Runs")) +
    theme(axis.text = element_text(size=12,face="bold"),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=18, face="bold"),
          plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# LGD Smolts
ggplot(results_summary, aes(y = LGDSmolt, x = Model)) +
    geom_jitter(width = 0.3, alpha = 0.3) +
    geom_boxplot(fill = "dodgerblue3", alpha = 0.7) +
    labs(y = "Median Smolt", title = "Natural Smolt", subtitle = paste(max(results_summary$Run),"Model Runs")) +
    theme(axis.text = element_text(size=12,face="bold"),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=18, face="bold"),
          plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Spawner
ggplot(results_summary, aes(y = Spawner, x = Model)) +
    geom_jitter(width = 0.3, alpha = 0.3) +
    geom_boxplot(fill = "dodgerblue3", alpha = 0.7) +
    labs(y = "Median Spawners", title = "Natural Spawners", subtitle = paste(max(results_summary$Run),"Model Runs")) +
    theme(axis.text = element_text(size=12,face="bold"),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=18, face="bold"),
          plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Trap Adults Hatchery
ggplot(results_summary, aes(y = H1_TrapAdult, x = Model)) +
    geom_jitter(width = 0.3, alpha = 0.3) +
    geom_boxplot(fill = "deeppink3", alpha = 0.7) +
    labs(y = "Median Adults", title = "Hatchery Adults to Trap", subtitle = paste(max(results_summary$Run),"Model Runs")) +
    theme(axis.text = element_text(size=12,face="bold"),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=18, face="bold"),
          plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))

# Trap Adults Natural
ggplot(results_summary, aes(y = TrapAdult, x = Model)) +
    geom_jitter(width = 0.3, alpha = 0.3) +
    geom_boxplot(fill = "dodgerblue3", alpha = 0.7) +
    labs(y = "Median Adults", title = "Natural Adults to Trap", subtitle = paste(max(results_summary$Run),"Model Runs")) +
    theme(axis.text = element_text(size=12,face="bold"),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=18, face="bold"),
          plot.subtitle = element_text(size=14, face="bold", color = "darkgray"))